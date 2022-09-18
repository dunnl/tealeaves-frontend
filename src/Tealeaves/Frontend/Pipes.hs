{-# language OverloadedStrings #-}

module Tealeaves.Frontend.Pipes where

import           Data.List (sortBy, intersperse)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Metrics as Metrics
import           Pipes
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as P
import           Data.Traversable as Tr
import           Control.Monad.State.Class (modify, put, get, gets)
import           Control.Monad.State.Strict

import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.App
import Tealeaves.Frontend.Logging
import Tealeaves.Frontend.DecoratedMonad
import Tealeaves.Frontend.TraversableExtra

type SymbolTable = Map Symbol AnyRule

-- | This is isomorphic to @ListT (App ()) a@, but it has the
-- advantage we can compose it with 'Pipe's, rather than just 'P.for'
type Buffer a = Producer a (App ()) ()

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap

-- | Assemble a set of 'Rules' into a 'SymbolTable'.
-- TODO: This operation should signal about potential errors in the symbol table
buildSymbolTable :: Rules -> -- ^ The rules of a user's grammar
                    App () SymbolTable -- ^ Computes a map from symbols to corresponding rules
buildSymbolTable rules = do
  app_logLn debugInfo $ "Building symbol table."
  stes <- runBuffer . helper $ rules
  app_logLn debugInfo $ "Symbol table entries: " <> T.pack (show stes)
  app_logLn debugInfo $ "Symbol table built."
  return $ M.fromList stes
  where
    helper :: Rules -> Buffer (Symbol, AnyRule)
    helper (Rules mvrs trs ntrs) = mconcat $
      [ foldFor ntrs $ \ntr -> do
          let symbols = ntr_symbols ntr
              name = ntr_name ntr
          lift . app_logLn debugInfo $ "Processing nonterminal \""  <> name <> "\". Symbols = " <> T.pack (show symbols)
          foldFor symbols $ \symbol ->
            return (symbol, Rl_ntr ntr)
      , foldFor trs $ \tr -> do
        let name = tr_name tr
            symbol = tr_symbol tr
        lift . app_logLn debugInfo $ "Processing terminal \""  <> name <> "\". Symbol = " <> symbol
        return (symbol, Rl_tr tr)
      , foldFor mvrs $ \mvr -> do
        let name = mvr_name mvr
            symbols = mvr_symbols mvr
        lift . app_logLn debugInfo $ "Processing metavariable \"" <> name <> "\". Symbols = " <> T.pack (show symbols)
        foldFor symbols $ \symbol ->
          return (symbol, Rl_mvr mvr)]

data CoqInductiveType = CoqIT
  { it_name :: Text
  , it_args :: [Text]
  , it_sort :: Maybe Text
  , it_constr :: [CoqConstructor]
  }

data CoqConstructor = CoqC
  { con_name :: Text
  , con_args :: [Text]
  }

-- | Pretty-print one of the constructors of an inductive type.

-- The inductive type argument is necessary because it contains the
-- name of the type, which is used when printing constructors.
ppConstructor :: CoqInductiveType -> CoqConstructor -> Text
ppConstructor it (CoqC name args) =
  "| " <> name <> " : " <> T.intercalate " -> " args <> " -> " <> (it_name it)

-- | Pretty-print an inductive type.
ppInductiveType :: CoqInductiveType -> Text
ppInductiveType it@(CoqIT name args msort constrs) =
  "Inductive " <> name <> T.intercalate " " args <> maybeSort <> " :=\n" <>
  (mconcat . indentLnAll 4 . endWithPeriod . fmap (ppConstructor it) $ constrs)
  where
    maybeSort = case msort of {Just sort -> " : " <> sort; Nothing -> ""}

-- | Given a symbol table and user-supplied symbol @usym@, organize
-- the known symbols into a list, sorted by distance from @usym@.
-- This operation can be used for debugging.
matchesOf :: SymbolTable -> -- ^ Symbol table
             Symbol -> -- ^ Token from the user
             [(Symbol, Int, AnyRule)] -- ^ Triples @(rsym, w, rule)@, ordered by increasing @w@, where @rsym@ is a symbol of @rule@ and @w@ is the distance from @usym@ to @rsym@.
matchesOf symt usym =
  sortBy (\(_,i,_) (_,j,_) -> compare i j) (fmap fix $ M.toList symtw)
    where
      fix (a, (b, c)) = (a, b, c)
      symtw = M.mapWithKey (\sym rl -> (Metrics.levenshtein usym sym, rl)) symt

-- | Given a grammar rule, yield the pretty-printed Coq name
-- corresponding to the rule, if one exists.  @symbol@ is used only
-- for logging. If @rule@ is terminal, nothing is yielded.
coqNameForSymbol :: Symbol -> AnyRule -> Buffer Symbol
coqNameForSymbol symbol rule =
  case rule of
    Rl_ntr (Ntr name _ _ _) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches non-terminal \"" <> name <> "\""
      return name
    Rl_mvr (Mvr name _ pp) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches metavariable \"" <> name <> "\""
      return pp
    Rl_tr (Tr name _) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches terminal \"" <> name <> "\" and won't be printed."
      mempty

-- | Convert a production rule to the list of arguments to the
-- corresponding constructor.
prodExprToArgs :: SymbolTable -> ProductionRule -> Buffer Symbol
prodExprToArgs symt pr = do
  let usyms = (dropSuffices . Select . each . T.words . pr_expr) pr :: Buffer Symbol
  usym <- usyms
  case M.lookup usym symt of
    Nothing -> do
      let nonmatches = matchesOf symt usym
      lift . app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
      lift . app_logLn debugError $ "Symbols sorted by Levenshtein distance: " <> (T.pack (show nonmatches))
      error "crash"
    Just rl ->
      coqNameForSymbol usym rl

-- | Drop the suffix of each symbol, if there is one.
dropSuffices :: Buffer Symbol -> Buffer Symbol
dropSuffices syms = do
  sym <- syms
  case (prefixOfSymbol sym) of
    Nothing -> return sym
    Just prefix -> return prefix

-- | Convert a buffer to a list in the @App@ monad.
runBuffer :: Buffer a -> App () [a]
runBuffer = P.toListM . enumerate

-- | Given a production rule, calculate the corresponding Coq constructor.
prodToConstructor :: SymbolTable ->
                     Nonterminal -> -- ^ The 'Nonterminal' this production rule is drawn from, which contains the constructor prefix.
                     ProductionRule -> -- ^ One production rule of the 'Nonterminal'.
                     App () CoqConstructor
prodToConstructor symt ntr pr = do
  args <- runBuffer (prodExprToArgs symt $ pr)
  return $ CoqC (ntr_prefix ntr <> pr_name pr) args

-- | Convert a 'Nonterminal' to an inductive type
nonterminalToInductiveType :: SymbolTable -> Nonterminal -> App () CoqInductiveType
nonterminalToInductiveType symt ntr = do
  let prods = ntr_productions ntr
  constrs <- traverse (prodToConstructor symt ntr) prods
  return $ CoqIT name [] (Just "Type") constrs
  where
    name = ntr_name ntr

pipes_main :: Rules -> App () ()
pipes_main rules = do
  symt <- buildSymbolTable rules
  coqits <- traverse (fmap ppInductiveType . nonterminalToInductiveType symt) (rls_ntrs rules) :: App () [Text]
  forM_ (intersperse "\n" coqits) $
    liftIO . T.putStr

type StatefulBuffer s a = ListT (StateT s (App ())) a

evalStateBuffer :: s -> StatefulBuffer s a -> Buffer a
evalStateBuffer s buff = Select $ PL.evalStateP s (enumerate buff)

appendNumber :: Text -> StatefulBuffer (Map Text Int) Text
appendNumber text = do
  count <- gets (M.findWithDefault 0 text)
  modify (M.insertWith (+) text 1)
  return (text <> T.pack (show count))

number :: Buffer Text -> StatefulBuffer (Map Text Int) Text
number buff = do
  text <- buff
  appendNumber text

{-

type Buffer a = ListT (App ()) a

-- | Assemble a set of 'Rules' into a 'SymbolTable'
-- TODO: This operation should signal about potential errors in the symbol table
buildSymbolTable :: Rules -> App () SymbolTable
buildSymbolTable rules = do
  app_logLn debugInfo $ "Building symbol table."
  stes <- runBuffer . helper $ rules
  app_logLn debugInfo $ "Symbol table entries: " <> T.pack (show stes)
  app_logLn debugInfo $ "Symbol table built."
  return $ M.fromList stes
  where
    helper :: Rules -> Buffer (Symbol, AnyRule)
    helper (Rules mvrs trs ntrs) = mconcat $
      [ foldFor ntrs $ \ntr -> do
          let symbols = ntr_symbols ntr
              name = ntr_name ntr
          lift . app_logLn debugInfo $ "Processing nonterminal \""  <> name <> "\". Symbols = " <> T.pack (show symbols)
          foldFor symbols $ \symbol ->
            return (symbol, Rl_ntr ntr)
      , foldFor trs $ \tr -> do
        let name = tr_name tr
            symbol = tr_symbol tr
        lift . app_logLn debugInfo $ "Processing terminal \""  <> name <> "\". Symbol = " <> symbol
        return (symbol, Rl_tr tr)
      , foldFor mvrs $ \mvr -> do
        let name = mvr_name mvr
            symbols = mvr_symbols mvr
        lift . app_logLn debugInfo $ "Processing metavariable \"" <> name <> "\". Symbols = " <> T.pack (show symbols)
        foldFor symbols $ \symbol ->
          return (symbol, Rl_mvr mvr)]

data CoqInductiveType = CoqIT
  { it_name :: Text
  , it_args :: [Text]
  , it_sort :: Maybe Text
  , it_constr :: [CoqConstructor]
  }

data CoqConstructor = CoqC
  { con_name :: Text
  , con_args :: [Text]
  }

-- | Pretty-print one of the constructors of an inductive type.

-- The inductive type argument is necessary because it contains the
-- name of the type, which is used when printing constructors.
ppConstructor :: CoqInductiveType -> CoqConstructor -> Text
ppConstructor it (CoqC name args) =
  "| " <> name <> " : " <> T.intercalate " -> " args <> " -> " <> (it_name it)

-- | Pretty-print an inductive type.
ppInductiveType :: CoqInductiveType -> Text
ppInductiveType it@(CoqIT name args msort constrs) =
  "Inductive " <> name <> T.intercalate " " args <> maybeSort <> " :=\n" <>
  (mconcat . indentLnAll 4 . endWithPeriod . fmap (ppConstructor it) $ constrs)
  where
    maybeSort = case msort of {Just sort -> " : " <> sort; Nothing -> ""}

-- | Given a symbol table and user-supplied symbol @usym@, organize
-- the known symbols into a list, sorted by distance from @usym@.
-- This operation can be used for debugging.
matchesOf :: SymbolTable -> -- ^ Symbol table
             Symbol -> -- ^ Token from the user
             [(Symbol, Int, AnyRule)] -- ^ Triples @(rsym, w, rule)@, ordered by increasing @w@, where @rsym@ is a symbol of @rule@ and @w@ is the distance from @usym@ to @rsym@.
matchesOf symt usym =
  sortBy (\(_,i,_) (_,j,_) -> compare i j) (fmap fix $ M.toList symtw)
    where
      fix (a, (b, c)) = (a, b, c)
      symtw = M.mapWithKey (\sym rl -> (Metrics.levenshtein usym sym, rl)) symt

-- | Given a grammar rule, yield the pretty-printed Coq name
-- corresponding to the rule, if one exists.  @symbol@ is used only
-- for logging. If @rule@ is terminal, nothing is yielded.
coqNameForSymbol :: Symbol -> AnyRule -> Buffer Symbol
coqNameForSymbol symbol rule =
  case rule of
    Rl_ntr (Ntr name _ _ _) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches non-terminal \"" <> name <> "\""
      return name
    Rl_mvr (Mvr name _ pp) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches metavariable \"" <> name <> "\""
      return pp
    Rl_tr (Tr name _) -> do
      lift . app_logLn debugInfo $ "Symbol " <> symbol <> " matches terminal \"" <> name <> "\" and won't be printed."
      mempty

-- | Convert a production rule to the list of arguments to the
-- corresponding constructor.
prodExprToArgs :: SymbolTable -> ProductionRule -> Buffer Symbol
prodExprToArgs symt pr = do
  let usyms = (dropSuffices . Select . each . T.words . pr_expr) pr :: Buffer Symbol
  usym <- usyms
  case M.lookup usym symt of
    Nothing -> do
      let nonmatches = matchesOf symt usym
      lift . app_logLn debugError $ "The symbol " <> usym  <> " has no matches in the symbol table."
      lift . app_logLn debugError $ "Symbols sorted by Levenshtein distance: " <> (T.pack (show nonmatches))
      error "crash"
    Just rl ->
      coqNameForSymbol usym rl

-- | Drop the suffix of each symbol, if there is one.
dropSuffices :: Buffer Symbol -> Buffer Symbol
dropSuffices syms = do
  sym <- syms
  case (prefixOfSymbol sym) of
    Nothing -> return sym
    Just prefix -> return prefix

-- | Convert a buffer to a list in the @App@ monad.
runBuffer :: Buffer a -> App () [a]
runBuffer = P.toListM . enumerate

-- | Given a production rule, calculate the corresponding Coq constructor.
prodToConstructor :: SymbolTable ->
                     Nonterminal -> -- ^ The 'Nonterminal' this production rule is drawn from, which contains the constructor prefix.
                     ProductionRule -> -- ^ One production rule of the 'Nonterminal'.
                     App () CoqConstructor
prodToConstructor symt ntr pr = do
  args <- runBuffer (prodExprToArgs symt $ pr)
  return $ CoqC (ntr_prefix ntr <> pr_name pr) args

-- | Convert a 'Nonterminal' to an inductive type
nonterminalToInductiveType :: SymbolTable -> Nonterminal -> App () CoqInductiveType
nonterminalToInductiveType symt ntr = do
  let prods = ntr_productions ntr
  constrs <- traverse (prodToConstructor symt ntr) prods
  return $ CoqIT name [] (Just "Type") constrs
  where
    name = ntr_name ntr

pipes_main :: Rules -> App () ()
pipes_main rules = do
  symt <- buildSymbolTable rules
  coqits <- traverse (fmap ppInductiveType . nonterminalToInductiveType symt) (rls_ntrs rules) :: App () [Text]
  forM_ (intersperse "\n" coqits) $
    liftIO . T.putStr

type StatefulBuffer s a = ListT (StateT s (App ())) a

evalStateBuffer :: s -> StatefulBuffer s a -> Buffer a
evalStateBuffer s buff = Select $ PL.evalStateP s (enumerate buff)

appendNumber :: Text -> StatefulBuffer (Map Text Int) Text
appendNumber text = do
  count <- gets (M.findWithDefault 0 text)
  modify (M.insertWith (+) text 1)
  return (text <> T.pack (show count))

number :: Buffer Text -> StatefulBuffer (Map Text Int) Text
number buff = do
  text <- buff
  appendNumber text

-}
