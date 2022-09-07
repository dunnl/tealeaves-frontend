{-# language OverloadedStrings #-}

module Tealeaves.Frontend.Coq where

import Control.Monad (when, unless)
import Data.Maybe (isNothing)
import Data.List (sortBy)
--import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
--import Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.Traversable as Tr
import qualified Data.Text.Metrics as Metrics
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Class (modify, gets)

import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.App
import Tealeaves.Frontend.Logging
import Tealeaves.Frontend.DecoratedMonad
import Tealeaves.Frontend.TraversableExtra

-- | An 'STE' (Symbol Table Entry) is a pattern paired with the name
-- of the accompanying text which should be printed in place of this
-- occurrence (an inductive type for a non-terminal or a pre-existing
-- type for a metavariable) or None if the symbol should not be
-- printed (i.e. if it is a terminal symbol in the object grammar)
data STE = STE
  { ste_sym  :: Symbol -- ^ The symbol
  , ste_name :: Name
  , ste_pp   :: Maybe Text
  } deriving (Eq, Show)

type SymbolTable = Map Symbol AnyRule

-- | Assemble a set of 'Rules' into a 'SymbolTable'
-- TODO: This operation should signal about potential errors
buildSymbolTable :: Rules -> App () SymbolTable
buildSymbolTable rules = do
  app_logLn debugInfo $ "Building symbol table."
  stes <- stackOf $ helper rules
  app_logLn debugInfo $ "Symbol table built."
  return $ M.fromList stes
  where
    helper :: Rules -> App [(Symbol, AnyRule)] ()
    helper (Rules mvrs trs ntrs) = do
      for_ ntrs $
        \ntr -> do
          let symbols = ntr_symbols ntr
              name = ntr_name ntr
          app_logLn debugInfo $ "Processing nonterminal \""  <> name <> "\". Symbols = " <> T.pack (show symbols)
          for_ symbols $
           \symbol ->
             do stack <- getContext
                app_logLn debugInfo $ "Currently processing symbol " <> (T.pack (show (length stack)))
                push (symbol, Rl_ntr ntr)
      for_ trs $
        \tr -> do
          let name = tr_name tr
              symbol = tr_symbol tr
          app_logLn debugInfo $ "Processing terminal \""  <> name <> "\". Symbol = " <> symbol
          push (symbol, Rl_tr tr)
      for_ mvrs $
        \mvr -> do
          let name = mvr_name mvr
              symbols = mvr_symbols mvr
          app_logLn debugInfo $ "Processing metavariable \"" <> name <> "\". Symbols = " <> T.pack (show symbols)
          for_ symbols $
            \symbol -> push (symbol, Rl_mvr mvr)

-- | Given a symbol table and user-supplied symbol @usym@ return a
-- list of potentially matching symbols in the table sorted by
-- Levenshtein distance from @usym@.
getMatches :: SymbolTable -> -- ^ Symbol table
              Text -> -- ^ Lexer token
               [(Symbol, Int, AnyRule)] -- ^ Sorted list of matching tokens
getMatches symt token =
  sortBy (\(_,i,_) (_,j,_) -> compare i j) (fmap fix $ M.toList symtw)
    where
      fix (a, (b, c)) = (a, b, c)
      symtw = M.mapWithKey (\sym rl -> (Metrics.levenshtein token sym, rl)) symt

-- | Iterate over a list of 'Symbol's and push the constructor
-- arguments
pushConstructorArgs :: SymbolTable -> -- ^ Symbol table
                       [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                       App [Text] () -- ^ Pushes the symbols representing a constructor argument to the stack
pushConstructorArgs symt syms = do
  for_ syms $
    \usym -> do -- user-supplied symbol
      let mmatch = M.lookup usym symt
      case mmatch of
        Nothing -> do
          let nonmatches = getMatches symt usym
          app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
          app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
          error "AAAAAAHHHH"
        Just rl ->
          case rl of
            Rl_ntr (Ntr name prefix productions syms) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> name <> "\""
              push name
            Rl_mvr (Mvr name _ pp) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> name <> "\""
              push pp
            Rl_tr (Tr name sym) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> name <> "\" and won't be printed."


-- | Pretty print one 'Nonterminal' symbol as an @Inductive@ type
-- definition in Coq.
ppConstructors :: SymbolTable -> -- ^ Symbol table
                  Nonterminal -> -- ^ A 'Nonterminal' symbol in the user's syntax
                  App [Text] ()  -- ^ Pushes a set of lines representing a Coq @Inductive@ type.
ppConstructors symt (Ntr name prefix productions _) = do
  for_ productions $
    \(Pr rname production _) -> do
      prefixes <- getPrefixes production
      constr_args <- stackOf $ pushConstructorArgs symt prefixes
      push $ "| " <> prefix <> rname <> " : " <> (T.intercalate " -> " constr_args) <> " -> " <> name
  where
    getPrefixes :: (Monoid w) => Text -> App w [Symbol]
    getPrefixes production =
      let symbols = T.words production
      in for symbols (\symbol -> maybe (mention_noparse symbol) return (prefixOfSymbol symbol))
      where mention_noparse sym = do
              app_logLn debugInfo $ "Symbol " <> sym <> " does not have an alphabetic prefix. This must be a terminal symbol."
              return sym

-- | Pretty print a list of 'Nonterminal' symbols.  Each 'Text' value
-- on the stack represents the complete declaration of an @Inductive@
-- type, with newlines inserted on all but the last line, concatenated
-- together into one list element.
ppNonterminals :: SymbolTable ->
                  [Nonterminal] -> -- ^ Set of 'Nonterminal's to print
                  App [Text] ()
ppNonterminals symt ntrs = do
  for_ ntrs $ \ntr -> do
      ntr_lines <- stackOf $ do
        push $ "Inductive " <> ntr_name ntr <>  " :="
        ppConstructors symt ntr
      push $ formatRule "  " ntr_lines

-- | Compute the text of the @Inductive@ type declarations in Coq.
ppInductiveTypes :: Rules ->
                    App () Text
ppInductiveTypes rules = do
    symt <- buildSymbolTable rules
    lines <- stackOf $ ppNonterminals symt (rls_ntrs rules)
    return $ T.intercalate "\n" lines


-- | Iterate over a list of 'Symbol's and push the constructor
-- arguments
getConstrArgs :: SymbolTable -> -- ^ Symbol table
                 [Symbol] -> -- ^ Set of words in a production expression
                 App [(Symbol, Either Nonterminal Metavar)] ()
getConstrArgs symt syms = do
  for_ syms $
    \usym -> do
      let prefix = maybe usym id (prefixOfSymbol usym)
          mmatch = M.lookup prefix symt
      case mmatch of
        Nothing -> do
          app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
          let nonmatches = getMatches symt usym
          app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
          error "Can't proceed without a match"
        Just rl ->
          case rl of
            Rl_ntr ntr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> ntr_name ntr <> "\""
              push (usym, Left ntr)
            Rl_mvr mvr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> mvr_name mvr <> "\""
              push (usym, Right mvr)
            Rl_tr tr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> tr_name tr <> "\" and is not an argument."


pushVariableName :: (Monad m) => Symbol -> (Either Nonterminal Metavar) -> AppT Environment (Map Text Int) [Text] m ()
pushVariableName usym rl = do
  let prefix = maybe usym id (prefixOfSymbol usym)
      name = case rl of Left ntr -> ntr_name ntr
                        Right mvr -> mvr_name mvr
  occurrences <- gets (M.findWithDefault 0 usym)
  modify $ M.insertWith (+) usym 1
  push $ (T.singleton . T.head $ name) <> (T.pack (show occurrences))

{-
printCase :: (Monoid w) => SymbolTable -> Nonterminal -> App w [Text]
printCase symt (Ntr name prefix productions symbols) = stackOf $ do
  for_ productions $ \(Pr pr_name pr_exp _bindmap) -> do
    push $ name <> pr_name -- Constructor name
    args <- stackOf $ for_ (T.words pr_exp) $ \usym -> do
      let prefix = maybe usym id (prefixOfSymbol usym)
          mmatch = M.lookup prefix symt
      case mmatch of
        Nothing -> do
              app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
              let nonmatches = getMatches symt usym
              app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
              error "Can't proceed without a match"
        Just rl ->
          case rl of
            Rl_ntr ntr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> ntr_name ntr <> "\""
              push (Left ntr)
            Rl_mvr mvr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> mvr_name mvr <> "\""
              push (Right mvr)
            Rl_tr tr -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> tr_name tr <> "\" and is not an argument."
    argNames <- for args $ \arg -> do
      let name = case arg of
                 Right mvr -> mvr_name mvr
                 Left ntr -> ntr_name ntr
      Just suffix <- getsContext (M.lookup name)
      add_context $ M.singleton name 1
      return (name <> suffix)
    for argNames $ push
-}

{-
mkOneRecursiveCall :: SymbolTable -> -- ^ Symbol table
                      Symbol -> -- ^ Name of the
                      Nonterminal -> -- ^ The 'Nonterminal' whose binddt we're printing

                      [Symbol] -> -- ^ A list of symbols in a production expression for this non-terminal
                      App [Either Metavar Nonterminal] () -- ^ Pushes the symbols representing binddt on one constructor
pushBinddtArgs symt syms = do

-- | Iterate over a list of 'Symbol's and push the constructor
-- arguments
pushBinddtArgs :: SymbolTable -> -- ^ Symbol table
                  Nonterminal -> -- ^ The 'Nonterminal' whose binddt we're printing
                  Metavar -> -- ^ The metavariable being substituted by this binddt
                  [Symbol] -> -- ^ A list of symbols in a production expression for this non-terminal
                  App [Either Metavar Nonterminal] () -- ^ Pushes the symbols representing binddt on one constructor
pushBinddtArgs symt syms = do
  for_ syms $
    \usym -> do
      let prefix = maybe usym id (prefixOfSymbol usym)
          mmatch = M.lookup usym prefix
      case mmatch of
        Nothing -> do
          let nonmatches = getMatches symt usym
          app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
          app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
          error "AAAAAAHHHH"
        Just rl ->
          case rl of
            Rl_ntr ntr@(Ntr name prefix productions syms) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal and is part of the recursive call \"" <> name <> "\""
              push (Right ntr)
            Rl_mvr (Mvr name _ pp) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> name <> "\""
              push (Left pp)
            Rl_tr (Tr name sym) -> do
              app_logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> name <> "\" and won't be printed."


ppBinddt :: SymbolTable -> -- ^ Symbol table
            Nonterminal -> -- ^ A 'Nonterminal' symbol in the user's syntax
            App [Text] ()  -- ^ Pushes a set of lines representing a Coq @Inductive@ type.
ppBinddt symt (Ntr name prefix productions _) = do
  push $ "Fixpoint binddt_" <> name <> " : " <> binddt_type <> " := "
  where
    varName = T.head name
    binddt_type =  "(list W * A -> F (" <> name <> " B)) -> " <> name <> " A -> F (" <> name <> " B)"
-}
