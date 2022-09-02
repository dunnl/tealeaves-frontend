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
import Driver


catMaybes :: [Maybe a] -> [a]
catMaybes xs = case xs of
  [] -> []
  (Just x : rest) -> x : catMaybes rest
  _ : rest -> catMaybes rest


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

-- | Given a symbol table and user's list of symbols, tag each symbol
-- with it's sorted list of matching STE's
decorateWithMatches :: [STE] -> [Symbol] -> [(Symbol, [(STE, Int)])]
decorateWithMatches symt usyms =
  fmap (\sym -> (sym, getMatches symt sym)) usyms

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















-- | Given a production string, compute the list of individual
-- symbols. Where these symbols decompose into
-- <alphabetic-string><suffix-string>, drop the suffix.
productionPrefixes :: Text -> App s [Symbol]
productionPrefixes prod =
  let syms = T.words prod :: [Symbol] in
  Tr.for syms (\sym -> maybe (mention_noparse sym) return (symbolPrefix sym))
  where mention_noparse sym = do
          app_logLn debugInfo (mconcat ["productionPrefixes: Symbol ", sym, " does not have a well-formed prefix. This must be a terminal symbol."])
          return sym

-- | Parse a production rule into a list of constructor arguments for
-- pretty-printing the AST inductive datatype. This function uses 'App'.
-- This is fundamentally a traverse-bind
productionToConstrArgs :: Rules -> Text -> App s [Text]
productionToConstrArgs rules prod = do
  let symt = toSymbolTable rules
      prefixes = productionPrefixes prod :: App s [Symbol]
      x = traverse getPrintableSymbol <$> decorateWithMatches symt <$> prefixes :: App s (App s [Maybe Text])
  fmap catMaybes $ Monad.join x

-- | Pretty print one production rule
ppProduction :: Rules -> Text -> Text -> ProductionRule -> App s Text
ppProduction rules name prefix (Pr rname prod _) = do
  let pre = mconcat ["| ", prefix, rname, " : "]
      post = mconcat [" -> ", name]
  constr_args <- T.intercalate " -> " <$> productionToConstrArgs rules prod
  return $ mconcat $ [pre, constr_args, post]





{-

-- | Print the recursive call
getPrintedCall :: Names -> -- ^ Final name map
                  (Symbol, [(STE, Int)]) -> -- ^ Name with matches
                  App (Maybe Text) -- Eh
getPrintedCall (usym, matches) =
  case matches of
    [] -> do error "getPrintedCall"
    (STE msym name mpp, w) : rest ->
      if w /= 0
      then do
        error "getPrintedCall"
      else do
        case mpp of
          Nothing -> do
            return Nothing
          Just pp ->
            return (Just pp)

productionToConstrArgs :: Rules -> Text -> App [Text]
productionToConstrArgs rules prod = do
  let symt = toSymbolTable rules
      x = traverseWithFinalState (\map ->  <$> decorateWithMatches symt (T.words prod) :: App (App [Maybe Text])
  fmap catMaybes $ Monad.join x

ppProduction :: Rules -> Text -> Text -> ProductionRule -> App Text
ppProduction rules name prefix (Pr rname prod _) = do
  let pre = mconcat ["| ", prefix, rname, " : "]
      post = mconcat [" -> ", name]
  constr_args <- T.intercalate " -> " <$> productionToConstrArgs rules prod
  return $ mconcat $ [pre, constr_args, post]
-}
