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
import qualified Pipes.Prelude as P
import           Data.Traversable as Tr
import           Control.Monad.State.Class (modify, put, get, gets)

import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.App
import Tealeaves.Frontend.Logging
import Tealeaves.Frontend.DecoratedMonad
import Tealeaves.Frontend.TraversableExtra

type SymbolTable = Map Symbol AnyRule

type Buffer a = ListT (App ()) a

annotate :: (a -> b) -> a -> (a, b)
annotate f a = (a, f a)

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

list :: Rules -> [(Nonterminal, [(ProductionRule, [Symbol])])]
list rules = do
  ntr <- rls_ntrs rules
  let rest = do
        prod <- ntr_productions ntr
        return (prod, T.words . pr_expr $ prod)
  return (ntr, rest)

runOps :: (Nonterminal -> Buffer o) ->
          (Nonterminal -> ProductionRule -> Buffer o) ->
          (Nonterminal -> ProductionRule -> Symbol -> Buffer o) ->
          [(Nonterminal, [(ProductionRule, [Symbol])])] ->
          Buffer o
runOps op1 op2 op3 stream = do
  foldMap (\(ntr, prods) -> op1 ntr <> foldMap (\(pr, syms) -> op2 ntr pr <> foldMap (op3 ntr pr) syms) prods)  stream

-- printNonterminal :: Nonterminal -> Buffer Text
-- printNonterminal (Ntr name prefix productions _) = do
--  return $ "Inductive " <> name <>  " :="

--printProductionRule :: Nonterminal -> ProductionRule -> Buffer Text
--printProductionRule (Ntr name prefix _ _) (Pr rname production _) = do
--  return $ "| " <> prefix <> "_" <> rname

--go :: Rules -> Buffer Text
--go = runOps printNonterminal printProductionRule . pairWithProductions . eachNonterminal

--pipes_main :: Rules -> App s ()
--pipes_main rules =
--  P.foldM (\_ a -> lift $ print a) (return ()) return $ (enumerate (go rules))

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

ppConstructor :: CoqInductiveType -> CoqConstructor -> Text
ppConstructor it (CoqC name args) =
  "| " <> name <> " : " <> T.intercalate " -> " args <> " -> " <> (it_name it)

ppInductiveType :: CoqInductiveType -> Text
ppInductiveType it@(CoqIT name args msort constrs) =
  "Inductive " <> name <> T.intercalate " " args <> maybeSort <> " :=\n" <>
  (mconcat . indentLnAll 4 . endWithPeriod . fmap (ppConstructor it) $ constrs)
  where
    maybeSort = case msort of {Just sort -> " : " <> sort; Nothing -> ""}

-- | Given a symbol table and user-supplied symbol @usym@, organize
-- the known symbols into a list, sorted by distance from @usym@.
matchesOf :: SymbolTable -> -- ^ Symbol table
             Symbol -> -- ^ Token from the user
             [(Symbol, Int, AnyRule)] -- ^ Triples @(rsym, w, rule)@, ordered by increasing @w@, where @rsym@ is a symbol of @rule@ and @w@ is the distance from @usym@ to @rsym@.
matchesOf symt usym =
  sortBy (\(_,i,_) (_,j,_) -> compare i j) (fmap fix $ M.toList symtw)
    where
      fix (a, (b, c)) = (a, b, c)
      symtw = M.mapWithKey (\sym rl -> (Metrics.levenshtein usym sym, rl)) symt

-- | Given a user symbol and its corresponding grammar rule, yield the
-- pretty-printed Coq name corresponding to the symbol, if one exists.
-- If @symbol@ is terminal, the constructed list is empty.
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

dropSuffices :: Buffer Symbol -> Buffer Symbol
dropSuffices syms = do
  sym <- syms
  case (prefixOfSymbol sym) of
    Nothing -> return sym
    Just prefix -> return prefix

runBuffer :: Buffer a -> App () [a]
runBuffer = P.toListM . enumerate

prodToConstructor :: SymbolTable -> Nonterminal -> ProductionRule -> App () CoqConstructor
prodToConstructor symt ntr pr = do
  args <- runBuffer (prodExprToArgs symt $ pr)
  return $ CoqC (ntr_prefix ntr <> pr_name pr) args

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
  coqits <- fmap (intersperse "\n") $ traverse (fmap ppInductiveType . nonterminalToInductiveType symt) (rls_ntrs rules) :: App () [Text]
  forM_ coqits $
    liftIO . T.putStr
{-
annotate :: (Functor f) => (a -> b) -> f a -> f (a, b)
annotate f = fmap (\a -> (a, f a))

--annotate :: (a -> b) -> Pipe a b m ()
--annotate f = P.map f

pairWithProductions :: Buffer Nonterminal -> Buffer (Nonterminal, Buffer ProductionRule)
pairWithProductions = annotate (each . ntr_productions)

runOps :: (Nonterminal -> Buffer o) ->
          (Nonterminal -> ProductionRule -> Buffer o) ->
          Buffer (Nonterminal, Buffer ProductionRule) ->
          Buffer o
runOps op1 op2 stream =
  stream >>= (\(ntr, prods) -> op1 ntr <> (prods >>= op2 ntr))

printNonterminal :: Nonterminal -> Buffer Text
printNonterminal (Ntr name prefix productions _) = do
  return $ "Inductive " <> name <>  " :="

printProductionRule :: Nonterminal -> ProductionRule -> Buffer Text
printProductionRule (Ntr name prefix _ _) (Pr rname production _) = do
  return $ "| " <> prefix <> "_" <> rname

go :: Rules -> Buffer Text
go = runOps printNonterminal printProductionRule . pairWithProductions . eachNonterminal

pipes_main :: Rules -> App s ()
pipes_main rules =
  P.foldM (\_ a -> lift $ print a) (return ()) return $ (enumerate (go rules))

-}
