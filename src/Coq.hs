{-# language OverloadedStrings #-}

module Coq where

import qualified Control.Monad as Monad
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.Traversable as Tr
import qualified Data.Text.Metrics as Metrics
import PP
import Parsing
import Rules
import App


dropNothings :: [Maybe a] -> [a]
dropNothings xs = case xs of
  [] -> []
  (Just x : rest) -> x : dropNothings rest
  _ : rest -> dropNothings rest


-- | An 'STE' (Symbol Table Entry) is a pattern paired with the name
-- of the accompanying text which should be printed in place of this
-- occurrence (an inductive type for a non-terminal or a pre-existing
-- type for a metavariable) or None if the symbol should not be
-- printed (i.e. if it is a terminal symbol in the object grammar)
data STE = STE
  { ste_sym  :: Symbol
  , ste_name :: Name
  , ste_pp   :: Maybe Text
  } deriving (Eq, Show)

-- | Given the rules, list all possible candidate symbols we could encounter,
-- along with their TokenType, and the symbol to use in their pretty printer.
toSymbolTable :: Rules -> [STE]
toSymbolTable (Rules mvrs trs ntrs) =
  let f (Ntr name _ _ symbols) = fmap (\s -> STE s name (Just name)) symbols
      g (Tr name symbol) = STE symbol name Nothing
      h (Mvr name symbols coq_name) = fmap (\s -> STE s name (Just coq_name)) symbols
  in concatMap f ntrs ++ fmap g trs ++ concatMap h mvrs

-- | Measure the Levenshtein distance between a user's symbol and a
-- symbol table entry
testSimilarity :: Symbol -> STE -> Int
testSimilarity sym ste =
  Metrics.levenshtein sym (ste_sym ste)

-- | Given a symbol table, user-supplied symbol, and return the list of candidate STEs sorted by distance from usym.
getMatches :: [STE] -> -- ^ Symbol table
              Text -> -- ^ Lexer token
              [(STE, Int)] -- ^ Sorted list of matching tokens
getMatches symt token =
  sortBy (\(_,i) (_,j) -> compare i j) symtw
    where
      -- symbol table with weights
      symtw = fmap (\ste -> (ste, testSimilarity token ste)) symt

-- | Pair a value with a function computed from it on the right
annotateBy :: (a -> b) -> a -> (a, b)
annotateBy f a = (a, f a)

{-
-- | Given a symbol table and user's list of symbols, tag each symbol
-- with it's sorted list of matching STE's
decorateWithMatches :: [STE] -> [Symbol] -> [(Symbol, [(STE, Int)])]
decorateWithMatches symt = fmap (annotateBy getMatches)
-}

-- | Get the printable attribute, if any, of a symbol
getPrintableSymbol :: (Symbol, [(STE, Int)]) -> App s (Maybe Text)
getPrintableSymbol (usym, matches) =
  case matches of
    [] -> do
        app_logLn debugError $ "ppDecSymbol: the symbol " <> usym <> " is decorated with no matches"
        error "`matches` empty in ppMatch"
    (STE msym name mpp, w) : rest ->
      if w /= 0
      then do
        app_logLn debugError (mconcat ["ppDecSymboL: Symbol ", usym, " has no exact match. Here is the full weighted symbol table: ",
                                      T.pack (show matches)
                                    ])
        error "no match found for symbol"
      else do
        app_logLn debugInfo (mconcat ["ppDecSymboL: Symbol ", usym, " matches the rule named ", name])
        case mpp of
          Nothing -> do
            app_logLn debugInfo (mconcat ["ppDecSymboL: Symbol ", usym, " is not associated with a print string and won't be printed."])
            return Nothing
          Just pp -> return (Just pp)


bind :: (Monad m) => (a -> m b) -> m a -> m b
bind f x = x >>= f

-- | 'mmapM' is a monadic traversal over a 'Traversable' value that is
-- already under the same monad, so that the two layers of monadic effect
-- may be 'join'ed. This is a monadic version of 'mapM'
mmapM :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
mmapM f = bind (mapM f)

mmapM_ :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m ()
mmapM_ f = bind (mapM_ f)

-- |
mforM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
mforM = flip mmapM

mforM_ :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m ()
mforM_ = flip mmapM_

-- | Given a production string, compute the list of individual
-- symbols. Where these symbols decompose into
-- <alphabetic-string><suffix-string>, drop the suffix.
prefixesOfProduction :: Text -> App s [Symbol]
prefixesOfProduction production =
  let symbols = T.words production
  in for symbols
     (\symbol -> maybe (mention_noparse symbol) return (prefixOfSymbol symbol))
  where mention_noparse sym = do
          app_logLn debugInfo (mconcat ["prefixesOfProduction: Symbol ", sym, " does not have an alphabetic prefix. This must be a terminal symbol."])
          return sym

-- | Parse a production rule into a list of constructor arguments for
-- pretty-printing the AST inductive datatype. This function uses 'App'.
-- This is fundamentally a traverse-bind
argsOfProduction :: Rules -> Text -> App s [Text]
argsOfProduction rules production = do
  let symt = toSymbolTable rules
  dropNothings <$> mforM (prefixesOfProduction production)
    (\prefix -> (getPrintableSymbol . annotateBy (getMatches symt)) prefix)

-- | Pretty print one production rule
textOfProduction :: Rules -> Text -> Text -> ProductionRule -> App s Text
textOfProduction rules name prefix (Pr rname production _) = do
  let pre = mconcat ["| ", prefix, rname, " : "]
      post = mconcat [" -> ", name]
  constr_args <- T.intercalate " -> " <$> argsOfProduction rules production
  return $ mconcat $ [pre, constr_args, post]
