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

import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Parsing
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.App
import Tealeaves.Frontend.Logging
import Tealeaves.Frontend.TraversableExtra

--import Control.Monad.Reader
--import Control.Monad.State

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
  { ste_sym  :: Symbol -- ^ The symbol
  , ste_name :: Name
  , ste_pp   :: Maybe Text
  } deriving (Eq, Show)

-- | Given the rules, list all possible candidate symbols we could encounter,
-- along with their TokenType, and the symbol to use in their pretty printer.
buildSymbolTable :: (Monoid w) => Rules -> App w [STE]
buildSymbolTable (Rules mvrs trs ntrs) = stackOf $ do
  app_logLn debugInfo $ "Building symbol table."
  for_ ntrs $
    \(Ntr name _ _ symbols) ->
      do app_logLn debugInfo $ "Processing nonterminal \""  <> name <> "\". Symbols = " <> T.pack (show symbols)
         for_ symbols $
           \symbol -> push $ STE symbol name (Just name)
  for_ trs $
    \(Tr name symbol) ->
      do app_logLn debugInfo $ "Processing nonterminal \""  <> name <> "\". Symbol = " <> symbol
         push $ STE symbol name Nothing
  for_ mvrs $
     \(Mvr name symbols coq_name) ->
       do app_logLn debugInfo $ "Processing metavariable \"" <> name <> "\". Symbols = " <> T.pack (show symbols)
          for_ symbols $
            \symbol -> push $ STE symbol name (Just coq_name)

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
      symtw = fmap (\ste -> (ste, testSimilarity token ste)) symt
      -- symbol table with weights

-- | Pair a value with a function computed from it on the right
annotateBy :: (a -> b) -> a -> (a, b)
annotateBy f a = (a, f a)

-- | Get the printable attribute, if any, of a symbol
pushPrintableSymbol :: [STE] -> Symbol -> App [Text] ()
pushPrintableSymbol symt usym = do
  case getMatches symt usym of
    [] -> do app_logLn debugError $ "The symbol " <> usym <> " has no matches in the symbol table."
             app_logLn debugError $ "Dumping symbol table: " <> (T.pack (show symt))
             error "AAAAAAHHHH"
    (STE msym name mpp, w) : rest ->
      if w /= 0 then do
        app_logLn debugError $ "Symbol " <> usym <> " has no exact match. "
        app_logLn debugError $ "Dumping matches: " <> T.pack (show $ getMatches symt usym)
        error "AAAAAH"
      else do
        app_logLn debugInfo $ "Symbol " <> usym <> " matches the rule \"" <> name <> "\""
        case mpp of
          Nothing ->
            app_logLn debugInfo $ "Symbol " <> usym <> " is not associated with a print string and won't be printed."
          Just pp -> push pp

-- | Given a production string, compute the list of individual
-- symbols. Where these symbols decompose into
-- <alphabetic-string><suffix-string>, drop the suffix.
getPrefixesOfProduction :: (Monoid w) => Text -> App w [Symbol]
getPrefixesOfProduction production =
  let symbols = T.words production
  in for symbols
     (\symbol -> maybe (mention_noparse symbol) return (prefixOfSymbol symbol))
  where mention_noparse sym = do
          app_logLn debugInfo $ "Symbol " <> sym <> " does not have an alphabetic prefix. This must be a terminal symbol."
          return sym

-- | Parse a production rule into a list of constructor arguments for
-- pretty-printing the AST inductive datatype.
getArgsOfProduction :: (Monoid w) => [STE] -> Text -> App w [Text]
getArgsOfProduction symt production = stackOf $ do
  mforM (getPrefixesOfProduction production) $
    (\prefix -> pushPrintableSymbol symt prefix)

-- | Pretty print one production rule
getTextOfProduction :: (Monoid w) =>
                       [STE] -> -- ^ Symbol table
                       Text -> -- ^ Name of this non-terminal
                       Text -> -- ^ Prefix to use for this non-terminal's production rules
                       ProductionRule -> -- ^ One 'ProductionRule' of this 'Nonterminal'
                       App w Text -- ^ Returns a pretty-printed string for one constructor in Coq
getTextOfProduction symt name prefix (Pr rname production _) = do
  constr_args <- T.intercalate " -> " <$> getArgsOfProduction symt production
  return $ "| " <> prefix <> rname <> " : " <> constr_args <> " -> " <> name
