{-# language OverloadedStrings #-}
{-# language TupleSections     #-}

{-|
Module      : Main.hs
Description : Main routines for pretty-printing Coq source files from user grammars
Copyright   : (c) Lawrence Dunn, 2022
-}

module Tealeaves.Frontend.Main where

import           Control.Monad.State.Class (modify, put, get, gets)
import           Control.Monad.State.Strict
import           Data.List (sortBy, intersperse, insert, union)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Metrics as Metrics
import           Data.Traversable as Tr
import           Data.Foldable as Fold
import qualified Pipes
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as P

import Tealeaves.Frontend.App
import Tealeaves.Frontend.Coq
import Tealeaves.Frontend.Graph
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Pipeline
import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Rules

---- Miscellaneous
getFreshType :: (Monad m) => StateT Int m Text
getFreshType = do
  count <- get
  let letter = T.singleton $ toEnum count
  modify (+1)
  return letter

---- SymbolTable operations
type SymbolTable = (Map Symbol AnyRule, Map Symbol AnyRule)

lookupSymbol :: SymbolTable -> Symbol -> Maybe AnyRule
lookupSymbol (namet, symt) sym = M.lookup sym symt

lookupRule :: SymbolTable -> Symbol -> Maybe AnyRule
lookupRule (namet, symt) sym = M.lookup sym namet

getRuleOfSymbol :: SymbolTable -> Symbol -> App () AnyRule
getRuleOfSymbol symt usym =
  case lookupSymbol symt usym of
      Nothing -> do
        let nonmatches = matchesOf symt usym
        logLn debugError $ "The symbol " <> usym <> " has no matches in the symbol table."
        logLn debugError $ "Symbols sorted by Levenshtein distance: " <> (T.pack (show nonmatches))
        error "crash"
      Just rl -> return rl

-- | Assemble a set of 'Rules' into a 'SymbolTable'. TODO: This
-- operation should signal about potential errors in the symbol table
buildSymbolTable :: Rules -> -- ^ The rules of a user's grammar
                    App () SymbolTable -- ^ Computes a map from symbols to corresponding rules
buildSymbolTable  (Rules mvrs trs ntrs) = do
  logLn debugInfo $ "Building symbol table."
  names <- P.toListM ruleStream
  stes <- P.toListM symbolStream
  logLn debugInfo $ "Symbol table built. Symbol table entries: " <> T.pack (show stes)
  return $ (M.fromList names, M.fromList stes)
  where
    ruleStream :: Stream (Symbol, AnyRule)
    ruleStream = do
      forM_ ntrs $ \ntr -> do
        yield (rule_name ntr, Rl_ntr ntr)
      forM_ trs $ \tr -> do
        yield (rule_name tr, Rl_tr tr)
      forM_ mvrs $ \mvr -> do
        forM_ (mvr_symbols mvr) $ \symbol ->
          yield (rule_name mvr, Rl_mvr mvr)
    symbolStream :: Stream (Symbol, AnyRule)
    symbolStream = do
      forM_ ntrs $ \ntr -> do
        logLn debugInfo $ "Processing nonterminal \""  <> rule_name ntr <> "\". Symbols = " <> T.pack (show . ntr_symbols $ ntr)
        forM_ (ntr_symbols ntr) $ \symbol ->
            yield (symbol, Rl_ntr ntr)
      forM_ trs $ \tr -> do
        logLn debugInfo $ "Processing terminal \""  <> rule_name tr <> "\". Symbol = " <> T.pack (show . tr_symbol $ tr)
        yield (tr_symbol tr, Rl_tr tr)
      forM_ mvrs $ \mvr -> do
        logLn debugInfo $ "Processing metavariable \"" <> rule_name mvr <> "\". Symbols = " <> T.pack (show . mvr_symbols $ mvr)
        forM_ (mvr_symbols mvr) $ \symbol ->
          yield (symbol, Rl_mvr mvr)

-- | Given a symbol table and user-supplied symbol @usym@, organize
-- the known symbols into a list, sorted by distance from @usym@.
-- This is intended to be used to generate error messages if a
-- user-supplied symbol doesn't match any table entry.  Outputs are
-- triples @(rsym, w, rule)@, ordered by increasing @w@, where @rsym@
-- is a symbol of @rule@ and @w@ is the distance from @usym@ to
-- @rsym@.
matchesOf :: SymbolTable -> -- ^ Symbol table
             Symbol -> -- ^ Symbol from the user
             [(Symbol, Int, AnyRule)] -- ^ Symbols in the table ordered by increasing distance
matchesOf symt usym =
  sortBy (\(_,i,_) (_,j,_) -> compare i j) (fmap fix $ M.toList symtw)
    where
      fix (a, (b, c)) = (a, b, c)
      symtw = M.mapWithKey (\sym rl -> (Metrics.levenshtein usym sym, rl)) (snd symt)



---- General-purpose helper pipeline utilities
-- | A stateful pipe which maintains a history of inputs. Each input
-- @in@ is echoed after appending the count of previously seen symbols
-- equal @in@.
appendNumber_ :: PipelineS (Map Text Int) Text Text
appendNumber_ = do
  text <- await
  count <- gets (M.findWithDefault 0 text)
  modify (M.insertWith (+) text 1)
  yield (text <> T.pack (show count))
  appendNumber_

-- | A pipe which appends a count of previously seen occurrences to
-- each input symbol, beginning with an empty history.
appendNumber :: Pipeline Text Text
appendNumber =
  PL.evalStateP M.empty appendNumber_

annotateNumber_ :: PipelineS (Map Text Int) (Text, Text) (Text, Text)
annotateNumber_ = do
  (usym, sym) <- await
  count <- gets (M.findWithDefault 0 sym)
  st <- get
  logLn debugInfo $ "annotateNumber_ state: " <> T.pack (show st)
  logLn debugInfo $ "Inserting symbol " <> sym <> " while annotating numbers"
  modify (M.insertWith (+) sym 1)
  yield (usym, sym <> T.pack (show count))
  annotateNumber_

-- | Append a number to each input, and create a map from old values to new ones
annotateNumber :: Pipeline (Text, Text) (Text, Text)
annotateNumber = do
  PL.evalStateP (M.empty) annotateNumber_



---- Stream operations
-- | Given a grammar rule, yield the pretty-printed Coq name
-- corresponding to the rule, if one exists.  @symbol@ is used only
-- for logging. If @rule@ is terminal, nothing is yielded.
coqTypeForRule :: (Symbol, AnyRule) -> Pipeline a Symbol
coqTypeForRule (symbol, rule) =
  case rule of
    Rl_ntr ntr -> do
      logLn debugInfo $ "Symbol " <> symbol <> " matches non-terminal \"" <> rule_name ntr <> "\""
      yield (rule_type ntr)
    Rl_mvr mvr -> do
      logLn debugInfo $ "Symbol " <> symbol <> " matches metavariable \"" <> rule_name mvr <> "\""
      yield (rule_type mvr)
    Rl_tr tr -> do
      logLn debugInfo $ "Symbol " <> symbol <> " matches terminal \"" <> rule_name tr <> "\" and won't be printed."

-- | Given a grammar rule, yield the pretty-printed Coq variable name
-- corresponding to the rule, if one exists.  @symbol@ is used only
-- for logging. If @rule@ is terminal, nothing is yielded.
coqVarForRule :: (Symbol, AnyRule) -> Pipeline a (Symbol, Symbol)
coqVarForRule (usym, rule) =
  case rule of
    Rl_ntr ntr -> do
      logLn debugInfo $ "Symbol " <> usym <> " matches non-terminal \"" <> rule_name ntr <> "\""
      yield (usym, rule_var ntr)
    Rl_mvr mvr -> do
      logLn debugInfo $ "Symbol " <> usym <> " matches metavariable \"" <> rule_name mvr <> "\""
      yield (usym, rule_var mvr)
    Rl_tr tr -> do
      logLn debugInfo $ "Symbol " <> usym <> " matches terminal \"" <> rule_name tr <> "\" and won't be printed as a variable."

-- | For each symbol @usym@ from the user, look up the associated
-- 'Rule' @rule@ and echo @(usym, rule)@. Crashes if @in@ does not
-- correspond to any rule.
addRulesToSymbols :: SymbolTable -> Pipeline Symbol (Symbol, AnyRule)
addRulesToSymbols symt = mapAnnotateM (getRuleOfSymbol symt)

-- | For each @a@ seen, echo @a@, modifying the state according to
-- @action@.
echoWithState :: (s -> a -> s) -> PipelineS s a a
echoWithState action = P.mapM $ \a -> do
  st <- get
  put (action st a)
  return a

-- | For each @a@ seen, echo @a@, modifying the state according to
-- @action@.
echoWithStateM :: (s -> a -> App () s) -> PipelineS s a a
echoWithStateM action = P.mapM $ \a -> do
  st <- get
  st' <- lift (action st a)
  put st'
  return a

-- | Each input @(usym, rule)@ seen should be a dependency of @ntr_name@.
-- For each input pair, echo modify the state to record the dependency.
logDependencies :: Symbol -> PipelineS (Map Symbol [Dependency]) (Symbol, AnyRule) (Symbol, AnyRule)
logDependencies ntr_name = echoWithStateM $ \map (usym, rl) -> do -- in App ()
  case rl of
    Rl_ntr ntr -> do
      logLn debugInfo $ "Symbol " <> usym <> " references inductive type " <> rule_name ntr <> "."
      return $ M.insertWith union ntr_name ([DepNtr $ rule_name ntr]) map
    Rl_mvr mvr -> do
      logLn debugInfo $ "Symbol " <> usym <> " references parametric type " <> rule_name mvr <> "."
      return $ M.insertWith union ntr_name ([DepVar $ rule_type mvr]) map
    _ -> return map

-- | For each input @(usym, rule)@, echo @rule_type rule@ if this exists.
filterRulesToTypes :: Pipeline (Symbol, AnyRule) Symbol
filterRulesToTypes = forInput coqTypeForRule



-- | Pretty-print recursive call to @binddt@ @((usym, rule), var)@ is
-- the original user symbol, the associated rule, and the variable
-- name associated with this rule.
policy :: Map Text Text -> Metavar -> ((Symbol, AnyRule), [Symbol], Symbol) -> Text
policy binder_names mvr ((usym, rl), binders, var) =
  case rl of
    Rl_ntr ntr ->
      case binders of
        [] -> "(binddt_" <> rule_name ntr <> " f " <> var <> ")"
        nonempty ->
          let bs = intersperse "∘" $ flip foldMap nonempty binder_to_name
          in "(binddt_" <> rule_name ntr <> " (f ∘ " <> mconcat bs <> ") " <> var <> ")"
    Rl_mvr mvr -> "pure " <> var
    Rl_tr tr -> error "terminal!"
    where
      binder_to_name sym =
        case M.lookup sym binder_names of { Nothing -> error "crash"; Just sym' -> ["push " <> sym'] }

-- | For each input @(usym, rule)@, echo @rule_var rule@ if this exists.
filterRulesToVars :: Pipeline (Symbol, AnyRule) (Symbol, Symbol)
filterRulesToVars = forInput coqVarForRule

-- | Input @(user_symbol, rule)@
-- Output @((user_symbol, rule), binders, var_name)@
annotateRulesToVars_ :: BindMap -> Pipeline (Symbol, AnyRule) ((Symbol, AnyRule), [Symbol], Symbol)
annotateRulesToVars_ bindmap =
  PL.evalStateP M.empty action
  where
    action :: PipelineS (Map Text Int) (Symbol, AnyRule) ((Symbol, AnyRule), [Symbol], Symbol)
    action = Tealeaves.Frontend.Pipeline.for cat $ \(usym, rule) ->
      case rule of
        Rl_ntr ntr -> do
          let var = rule_var ntr
          count <- gets (M.findWithDefault 0 var)
          let binders = maybe [] return $ M.lookup var bindmap
          modify (M.insertWith (+) var 1)
          yield ((usym, rule), binders, var <> T.pack (show count))
        Rl_mvr mvr -> do
          let var = rule_var mvr
          count <- gets (M.findWithDefault 0 var)
          modify (M.insertWith (+) var 1)
          yield ((usym, rule), [], var <> T.pack (show count))
        Rl_tr tr -> return ()

-- | Convert a @ProductionRule@ into the list of argument types of the
-- constructor associated with the rule (of the @Inductive@ type
-- corresponding to the @Nonterminal@ the production rule is drawn
-- from).
prodExprToTypeArgs :: SymbolTable -> Symbol -> ProductionRule -> StreamS (Map Symbol [Dependency]) Symbol
prodExprToTypeArgs symt ntr_name pr =
  (hoistState $ filterRulesToTypes) <-< logDependencies ntr_name <-< (hoistState $ addRulesToSymbols symt <-< dropSuffices <-< (each . T.words . pr_expr $ pr))

-- | Drop the suffix of each symbol, if there is one.
dropSuffices :: Pipeline Symbol Symbol
dropSuffices = P.map $ \sym -> fromMaybe sym (prefixOfSymbol sym)



data Dependency =
    DepVar Symbol | DepNtr Symbol
    deriving (Eq, Show)

--- Application operations to assemble Coq types from user rules
-- | Given a production rule, calculate the corresponding Coq constructor.
prodToConstructor :: SymbolTable ->
                     Nonterminal -> -- ^ The 'Nonterminal' this production rule is drawn from.
                     ProductionRule -> -- ^ One production rule of the 'Nonterminal'.
                     StateT (Map Symbol [Dependency]) (App ()) CoqConstructor
prodToConstructor symt ntr pr = do -- in the StateT monad
  refs <- get
  (typeArgs, refs') <- lift . runStreamS refs $ (prodExprToTypeArgs symt (rule_name ntr) pr)
  put refs'
  return $ (CoqC constr typeArgs)
    where
      constr = constrName ntr pr

-- | Convert a 'Nonterminal' to an Coq @Inductive@ type.
nonterminalToInductiveType :: SymbolTable ->
                              Nonterminal -> -- ^ The nonterminal rule to represent as an inductive definition
                              StateT (Map Symbol [Dependency]) (App ()) CoqInductiveType
nonterminalToInductiveType symt ntr = do
  constrs <- (Tr.forM (ntr_productions ntr) $ \pr ->
                 prodToConstructor symt ntr pr)
  dependencies <- get
  let var_deps = M.map (\deps -> deps >>= (\dep -> case dep of { DepNtr ntr -> []; DepVar var -> [(var, Just "Set")]})) dependencies
      type_args = M.findWithDefault [] (rule_name ntr) var_deps
  return $ CoqIT name type_args (Just "Type") constrs
  where
    name = ntr_name ntr

ppNonterminal :: SymbolTable ->
                 Nonterminal ->
                 App () Text
ppNonterminal symt ntr = do
  it <- flip evalStateT M.empty $ nonterminalToInductiveType symt ntr
  return $ prettyPrint it



-- | Convert a @ProductionRule@ into the list of argument names to use
-- for the constructor associated with the rule in a @match@
-- expression.  The @Stream@ maintains a map from the original binder
-- symbols given by the user to their pretty-printed variable names,
-- which can be used later while pretty-printing the body of this case
-- to match user-supplied symbols with in-scope variables in the body.
prodExprToFixCaseArgs :: SymbolTable ->
                         ProductionRule -> -- ^ The 'ProductionRule' of some 'Nonterminal'.
                         StreamS (Map Symbol Text) Symbol -- ^ A stream of symbols representing the argument types of the associated constructor.
prodExprToFixCaseArgs symt pr =
  noteNewNames <-< hoistState (annotateNumber <-< filterRulesToVars <-< addRulesToSymbols symt <-< dropSuffices <-< (each . T.words . pr_expr $ pr))

-- | Given @(<user symbol>, <pretty var>)@, make a note in the map
-- that @<user symbol>@ maps to @<pretty var>@, then 'yield' @<pretty
-- var>@.
noteNewNames :: PipelineS (Map Symbol Symbol) (Text, Text) Text
noteNewNames = do
  (usym, sym) <- await
  modify (M.insert usym sym)
  yield sym
  noteNewNames

-- | Convert a @ProductionRule@ into the body of the associated case
-- of the @Fixpoint binddt@ definition for @Nonterminal@ the
-- @ProductionRule@ is drawn from.  The output is a @Stream@ of
-- symbols which can be concatenated to form the @fxc_body@ of a
-- @CoqFixCase@.
prodExprToFixBody :: Map Symbol Text ->
                     SymbolTable -> -- ^ The global symbol table
                     Metavar -> -- ^ The 'Metavar' being targetted for substitution
                     Nonterminal -> -- ^ The 'Nonterminal' the 'ProductionRule' is drawn from.
                     ProductionRule -> -- ^ The 'ProductionRule' whose case body we are generating.
                     Stream Symbol
prodExprToFixBody binder_names symt mvr ntr pr = pipeIntersperse " <*> " <-< do
  yield ("pure " <> constrName ntr pr)
  P.map (policy binder_names mvr) <-< annotateRulesToVars_ bindmap <-< addRulesToSymbols symt <-< dropSuffices <-< (each . T.words . pr_expr $ pr)
  where
    bindmap = fromMaybe (M.empty) (pr_binds pr)


--- Application operations to assemble Coq fixpoints from user rules
-- | Given a production rule, calculate the corresponding arguments to @binddt@.
prodToFixCase :: SymbolTable ->
                 Metavar -> -- ^ The 'Metavar' targetted for substitution
                 Nonterminal -> -- ^ The 'Nonterminal' this production rule is drawn from, which contains the constructor prefix.
                 ProductionRule -> -- ^ One production rule of the 'Nonterminal'.
                 App () CoqFixCase
prodToFixCase symt mvr ntr pr = do
  logLn debugInfo $ "Building Fixpoint case for constructor " <> constr <> " of type " <> (rule_name ntr) <> " w.r.t. variable " <> (rule_name mvr)
  (args, binder_names) <- runStreamS M.empty (prodExprToFixCaseArgs symt pr)
  body <- runStream (prodExprToFixBody binder_names symt mvr ntr pr)
  return $ CoqFixCase constr args (mconcat body)
  where
    constr = constrName ntr pr

-- | Convert a 'Nonterminal' to a @CoqFixpoint@ declaration.
nonterminalToFixpoint :: SymbolTable ->
                         Metavar -> -- ^ Metavariable whose instances to target for substitution
                         Nonterminal -> -- ^ Nonterminal within which substitution is performed
                         App () CoqFixpoint
nonterminalToFixpoint symt mvr ntr = do
  let prods = ntr_productions ntr
  cases <- traverse (prodToFixCase symt mvr ntr) prods
  return $ CoqFix ("binddt_" <> rule_name ntr) ["<args to binddt>"] Nothing recvar cases
  where
    recvar = rule_var ntr

-- | Pretty print Coq definition of @Fixpoint binddt_<nonterminal>_<var>@.
ppNonterminalFix :: SymbolTable ->
                    Metavar -> -- ^ Metavariable whose instances to target for substitution
                    Nonterminal -> -- ^ Nonterminal within which substitution is performed
                    App () Text -- ^ Pretty-printed @Fixpoint@ definition of @binddt@, terminated with a newline.
ppNonterminalFix symt mvr ntr = do
  binddt <- nonterminalToFixpoint symt mvr ntr
  return $ prettyPrint binddt



---- Simple main operation
pipes_main :: Rules -> App () ()
pipes_main rules = do
  symt <- buildSymbolTable rules
  let ntrs = rls_ntrs rules
  (types, dependencies) <- flip runStateT M.empty $ Tr.for ntrs $ \ntr ->
    (nonterminalToInductiveType symt ntr)
  let type_map = M.fromList $ (\c -> (it_name c, c)) <$> types
      type_deps = M.map (\deps -> deps >>= (\dep -> case dep of { DepNtr ntr -> [ntr]; DepVar _ -> [] })) dependencies
  liftIO . T.putStr . T.intercalate "\n" . fmap prettyPrint $
    (getStronglyConnectedComponents type_map type_deps)
  liftIO . T.putStr $ "\n(* Begin functions *)\n"
  fns <- fmap mconcat . Tr.for (rls_mvrs rules) $ \mvr ->
    traverse (ppNonterminalFix symt mvr) ntrs
  forM_ (intersperse "\n" fns) $
    liftIO . T.putStr
