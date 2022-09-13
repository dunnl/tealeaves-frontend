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

-- | Assemble a set of 'Rules' into a 'SymbolTable'
-- TODO: This operation should signal about potential errors in the symbol table
buildSymbolTable :: Rules -> App () SymbolTable
buildSymbolTable rules = do
  app_logLn debugInfo $ "Building symbol table."
  stes <- logOf $ helper rules
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
             do stack <- getl
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

-- | Given a list of symbols (probably lexed from a production
-- expression of a nonterminal rule), compute the associated list of
-- corresponding 'AnyRule's. An exception is thrown if a symbol does
-- not match with a rule.
getMatchingRulesOf :: (Monoid w) =>
                      SymbolTable -> -- ^ Symbol table
                      [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                      App w [(Symbol,AnyRule)] -- ^ Pushes the symbols representing a constructor argument to the stack
getMatchingRulesOf symt syms = do
  for syms $
    \usym -> do
      let mmatch = M.lookup usym symt
      case mmatch of
        Nothing -> do
          let nonmatches = getMatches symt usym
          app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
          app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
          error "AAAAAAHHHH"
        Just rl ->
          return $ (usym, rl)

-- | Iterate over a list of 'Symbol's and push the constructor
-- arguments
-- postcondition: the log contains a set of
constrArgTypes :: (Monoid w) =>
                  SymbolTable -> -- ^ Symbol table
                  [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                  App w [Text] -- ^ Pushes the symbols representing a constructor argument to the stack
constrArgTypes symt syms = do
  rules <- getMatchingRulesOf symt syms
  logOf $ for_ rules $
    \(usym, rl) ->
      case rl of
        Rl_ntr (Ntr name _ _ _) -> do
          app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> name <> "\""
          push name
        Rl_mvr (Mvr name _ pp) -> do
          app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> name <> "\""
          push pp
        Rl_tr (Tr name _) -> do
          app_logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> name <> "\" and won't be printed."

getPrefixes :: (Monoid w) => Text -> App w [Symbol]
getPrefixes production =
  let symbols = T.words production
  in for symbols (\symbol -> maybe (mention_noparse symbol) return (prefixOfSymbol symbol))
  where mention_noparse sym = do
          app_logLn debugInfo $ "Symbol " <> sym <> " does not have an alphabetic prefix. This must be a terminal symbol."
          return sym


-- | Pretty print a Coq @Inductive@ definition corresponding to a
-- 'Nonterminal' symbol in the object syntax.
-- postcondition: the buffer contains a list of 'Text' values, each of which is one line in a pretty-printed inductive definition
inductiveType :: (Monoid w) =>
                  SymbolTable -> -- ^ Symbol table
                  Nonterminal -> -- ^ A 'Nonterminal' symbol in the object syntax
                  App w [Text]  -- ^ Pushes a set of lines representing a Coq @Inductive@ type.
inductiveType symt (Ntr name prefix productions _) = logOf $ do
  push $ "Inductive " <> name <>  " :="
  for_ productions $
    \(Pr rname production _) -> do
      prefixes <- getPrefixes production
      constr_args <- constrArgTypes symt prefixes
      push $ "| " <> prefix <> rname <> " : " <> (T.intercalate " -> " constr_args) <> " -> " <> name

-- | Pretty print a list of 'Nonterminal' symbols.  Each 'Text' value
-- on the stack represents the complete declaration of an @Inductive@
-- type, with newlines inserted on all but the last line, concatenated
-- together into one list element.
inductiveTypes :: (Monoid w) =>
                  SymbolTable ->
                  [Nonterminal] -> -- ^ Set of 'Nonterminal's to print
                  App w [Text]
inductiveTypes symt ntrs = do
  logOf $ for_ ntrs $ \ntr -> do
      idef <- inductiveType symt ntr
      push (concatDefinition idef)

-- | Compute a single 'Text' value whose contents are the set of
-- @Inductive@ type declarations corresponding to each non-terminal in
-- the object grammar.
ppIDefs :: Rules ->
           App () Text
ppIDefs rules = do
    symt <- buildSymbolTable rules
    idefs <- inductiveTypes symt (rls_ntrs rules)
    return $ T.intercalate "\n" idefs

pushBinddt :: SymbolTable -> -- ^ Symbol table
              Nonterminal -> -- ^ A 'Nonterminal' symbol in the object syntax
              App [Text] ()
pushBinddt symt (Ntr name prefix productions syms) = do
  let head_symbol = head syms
  push $ "Fixpoint binddt_" <> name <>  " f " <> head_symbol <> " := match " <> head_symbol <> " with"
  for_ productions $
    \(Pr rname production _) -> do
      prefixes <- getPrefixes production
      body_calls <- logOf $ pushBinddtCase symt prefixes
      push $ "| ... -> " <> (T.intercalate " " body_calls)

pushFnDefs :: SymbolTable ->
             [Nonterminal] -> -- ^ Set of 'Nonterminal's to print
             App [Text] ()
pushFnDefs symt ntrs = do
  for_ ntrs $ \ntr -> do
      fn_def <- logOf $ pushBinddt symt ntr
      push (concatDefinition fn_def)

ppFnDefs :: Rules ->
            App () Text
ppFnDefs rules = do
    symt <- buildSymbolTable rules
    fn_defs <- logOf $ pushFnDefs symt (rls_ntrs rules)
    return $ T.intercalate "\n" fn_defs

-- | push recursive calls to 'binddt'
pushBinddtCase :: SymbolTable -> -- ^ Symbol table
                  [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                  App [Text] () -- ^ Pushes the symbols representing a constructor argument to the stack
pushBinddtCase symt syms = do
  rules <- getMatchingRulesOf symt syms
  for_ rules $
    \(usym, rl) ->
      case rl of
        Rl_ntr (Ntr name _ _ _) -> do
          app_logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> name <> "\""
          push $ "(binddt f " <> name <> ")"
        Rl_mvr (Mvr name _ pp) -> do
          app_logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> name <> "\""
          push $ "(f " <> name <> ")"
        Rl_tr (Tr name _) -> do
          return ()
