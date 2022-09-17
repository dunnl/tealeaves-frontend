{-# language OverloadedStrings #-}

module Tealeaves.Frontend.Coq where

import           Data.List (sortBy)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Aeson as A
import qualified Data.Text.Metrics as Metrics
import           Data.Map (Map)
import qualified Data.Map as M
import Control.Monad.State.Class (modify, put, get, gets)

import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.App
import Tealeaves.Frontend.Logging
import Tealeaves.Frontend.DecoratedMonad
import Tealeaves.Frontend.TraversableExtra

type SymbolTable = Map Symbol AnyRule



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
             do stack <- get
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
-- not match any rule.
pairSymbolsWithRules :: SymbolTable -> -- ^ Symbol table
                      [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                      App s [(Symbol, AnyRule)] -- ^ Pushes the symbols representing a constructor argument to the stack
pairSymbolsWithRules symt syms = do
  for syms $
    \usym -> do
      let mmatch = M.lookup usym symt
      case mmatch of
        Nothing -> do
          let nonmatches = matchesOf symt usym
          app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
          app_logLn debugError $ "Dumping non-matching symbols ordered by Levenshtein distance: " <> (T.pack (show nonmatches))
          error "AAAAAAHHHH"
        Just rl ->
          return $ (usym, rl)

-- | Iterate over a list of 'Symbol's in a production expression and
-- compute the set of arguments to the constructor
constrArgTypes :: SymbolTable -> -- ^ Symbol table
                  [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
                  App s [Text] -- ^ The list of types accepted by the constructor associated with the production expression
constrArgTypes symt syms = do
  sym_rules <- pairSymbolsWithRules symt syms
  logOf $ for_ sym_rules $
    \(symbol, rule) ->
      case rule of
        Rl_ntr (Ntr name _ _ _) -> do
          app_logLn debugInfo $ "Symbol " <> symbol <> " matches non-terminal \"" <> name <> "\""
          push name
        Rl_mvr (Mvr name _ pp) -> do
          app_logLn debugInfo $ "Symbol " <> symbol <> " matches metavariable \"" <> name <> "\""
          push pp
        Rl_tr (Tr name _) -> do
          app_logLn debugInfo $ "Symbol " <> symbol <> " matches terminal \"" <> name <> "\" and won't be printed."

splitIntoPrefixes :: Text -> App s [Symbol]
splitIntoPrefixes production =
  for symbols $
  \symbol -> case (prefixOfSymbol symbol) of
               Nothing -> do
                 app_logLn debugInfo $ "Symbol " <> symbol <> " does not have an alphabetic prefix. This must be a terminal symbol."
                 return symbol
               Just prefix -> return prefix
  where symbols = T.words production

-- | Pretty print a Coq @Inductive@ definition corresponding to a
-- 'Nonterminal' symbol in the object syntax. Returns a list of 'Text'
-- values, each of which is one line in a pretty-printed inductive
-- definition. Lines are not newline-terminated or indented.
ppAbstractSyntaxType :: SymbolTable -> -- ^ Symbol table
                        Nonterminal -> -- ^ A 'Nonterminal' symbol in the object syntax
                        App s Text  -- ^ Returns a set of lines representing the @Inductive@ definition of a AST type.
ppAbstractSyntaxType symt (Ntr name prefix productions _) = do
  lines <- logOf $ do
    push $ "Inductive " <> name <>  " :="
    for_ productions $ \(Pr rname production _) -> do
      prefixes <- splitIntoPrefixes production
      constr_args <- constrArgTypes symt prefixes
      push $ "| " <> prefix <> rname <> " : " <> (T.intercalate " -> " constr_args) <> " -> " <> name
  return $ concatDefinition lines

-- | Pretty print each of the 'Nonterminal' symbols as an @Inductive@
-- type definition in Coq. The returned 'Text' value contains the
-- entire AST definition section of the pretty-printed output.
ppAbstractSyntaxTypes :: SymbolTable ->
                         Rules ->
                         App w Text
ppAbstractSyntaxTypes symt rules = do
  types <- for (rls_ntrs rules) $ \ntr ->
    ppAbstractSyntaxType symt ntr
  return $ T.intercalate "\n" types

-- | Pretty print the definition of @binddt@ for each 'Nonterminal'
-- w.r.t. the given 'Metavar' as a @Fixpoint@ in Coq. The
-- returned 'Text' value contains the entire function definition
-- section of the pretty-printed output.
ppFunctions :: SymbolTable -> -- ^ Symbol table
               Metavar ->
               Rules ->
               App s Text
ppFunctions symt mvr rules = do
  fns <- for (rls_ntrs rules) $ \ntr ->
    ppFunction symt mvr ntr
  return $ T.intercalate "\n" fns

ppAllFunctions :: SymbolTable ->
                  Rules ->
                  App s Text
ppAllFunctions symt rules = do
  fns <- for (rls_mvrs rules) $ \mvr ->
    ppFunctions symt mvr rules
  return $ T.intercalate "\n" fns


head_symbol :: Nonterminal -> Text
head_symbol ntr = head (ntr_symbols ntr)

fn_name :: Text -> Text -> Text
fn_name name var_name = "binddt_" <> name <> "_" <> var_name

-- | Pretty print the @binddt@ function of a 'Nonterminal'.
ppFunction :: SymbolTable ->
              Metavar ->
              Nonterminal ->
              App s Text
ppFunction symt mvr@(Mvr var_name var_syms var_type) ntr@(Ntr name prefix productions symbols) = do
  lines <- logOf $ do
    push $ "Fixpoint " <> fn_name name var_name  <>  " f " <> head_symbol ntr <> " := match " <> head_symbol ntr <> " with"
    for_ productions $ \(Pr rname production bindmap) -> do
      prefixes <- splitIntoPrefixes production
      body_calls <- fnCaseBody symt mvr prefixes
      match_case <- fnCaseCase symt mvr prefixes
      push $ "| " <> (T.intercalate " " match_case) <> " -> " <> (T.intercalate " " body_calls)
  return $ concatFunction lines


fnCaseBody :: SymbolTable -> -- ^ Symbol table
              Metavar ->
              [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
              App s [Text]
fnCaseBody symt mvr syms = do
  let var_name = mvr_name mvr
  rules <- pairSymbolsWithRules symt syms
  logOf $ for rules $ \(usym, rl) ->
    case rl of
      Rl_ntr (Ntr name _ _ _) -> do
        push $ "(" <> fn_name name var_name <> " f " <> name <> ")"
      Rl_mvr (Mvr name _ pp) -> do
        push $ "(f " <> name <> ")"
      Rl_tr (Tr name _) -> do
        return ()

fnCaseCaseHelper :: SymbolTable -> -- ^ Symbol table
                    Metavar ->
                    (Symbol, AnyRule) ->
                    App (Map Symbol Int, [Text]) ()
fnCaseCaseHelper symt mvr (sym, rl) = do
  let var_name = mvr_name mvr
  case rl of
    Rl_ntr ntr -> do
      let hsym = head_symbol ntr
      (counts, args) <- get
      let count = M.findWithDefault 0 hsym counts
          new_counts = M.insertWith (+) hsym 1 counts
          arg = hsym <> T.pack (show count)
      put (new_counts, arg : args)
    Rl_mvr (Mvr name _ pp) -> do
      return ()
    Rl_tr (Tr name _) -> do
      return ()

fnCaseCase :: SymbolTable -> -- ^ Symbol table
              Metavar ->
              [Symbol] -> -- ^ A list of symbols in a production expression, with each symbol's numerical suffixes dropped (if any)
              App s [Text]
fnCaseCase symt mvr syms = do
  rules <- pairSymbolsWithRules symt syms
  fmap (reverse . snd) $ execSubAppT (M.empty, []) $ for rules $ \(usym, rl) ->
    fnCaseCaseHelper symt mvr (usym, rl)

{-
-- | Given a list of @(symbol, rule)@ pairs for a production expression,
-- build a list of variable arguments to the
getMatchConstrArgs :: SymbolTable -> -- ^ Symbol table
                      [(Symbol, AnyRule)] -- ^ A list of symbols, paired with matching rules

                    Nonterminal -> -- ^ A 'Nonterminal' symbol in the object syntax
                    Metavar -> -- ^ The 'Metavar' symbol targetted by binddt
                    App s [Text]
getMatchConstrArgs


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

-}
