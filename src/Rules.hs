{-# language OverloadedStrings #-}
{-# language DeriveGeneric     #-}

module Rules where

import Data.List (sortBy)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Text.Metrics as Metrics
import GHC.Generics
import PP

data TokenType =
  TkTerminal | TkNonTerminal | TkMetavariable
  deriving (Eq, Show)

type Symbol = Text

-- | Metavariable rule
data Metavar = Mvr
  { mvr_name :: Text
  , mvr_symbols :: [Symbol]
  , mvr_coqtype :: Text
  } deriving (Generic, Show)

instance FromJSON Metavar where
instance ToJSON Metavar where


-- | Non-terminal rule
data NTRrule = NTRrule
  { ntr_name :: Text -- inductive type name
  , ntr_prefix :: Text -- common prefix for production names
  , ntr_productions :: [( Text -- production name
                        , Text -- production string
                        )]
  , ntr_symbols :: [Symbol] -- Symbols that can represent this non-terminal
  } deriving (Generic, Show)

instance FromJSON NTRrule where
instance ToJSON NTRrule where

-- | Terminal rule
data Terminal = Terminal
  { tr_name :: Text
  , tr_symbol :: Symbol
  } deriving (Generic, Show)

instance FromJSON Terminal where
instance ToJSON Terminal where
  
-- | Rules
data Rules = Rules
  { env_ntrs :: [NTRrule]
  , env_trs :: [Terminal]
  , env_mvrs :: [Metavar]
  } deriving (Generic, Show)

instance FromJSON Rules where
instance ToJSON Rules where

-- | A 'Candidate' is a pattern paired with the name of the accompanying rule name
-- which should be printed in place of this occurrence
type Candidate = (Symbol, TokenType, Text)

-- | Given the rules, list all possible candidate symbols we could encounter,
-- along with their TokenType, and the symbol to use in their pretty printer
envToCandidates :: Rules -> [Candidate]
envToCandidates (Rules ntrs trs mvrs) =
  let toCandidatesNtr (NTRrule name prefix prods symbols) =
        fmap (\symb -> (symb, TkNonTerminal, name)) symbols
      toCandidateTr (Terminal name symbol) = (symbol, TkTerminal, name)
      toCandidatesMvr (Mvr name symbols coq_name) =
        fmap (\symbol -> (symbol, TkMetavariable, coq_name)) symbols
  in concatMap toCandidatesNtr ntrs ++
     fmap toCandidateTr trs ++
     concatMap toCandidatesMvr mvrs
     
-- | Take a set of rules, a lexer token, and return the sorted list of candidates
getMatches :: Text -> [Candidate] -> [(TokenType, Text, Int)]
getMatches token cands =
  let xs = fmap (\(val, tt, name) -> (tt, name, Metrics.levenshtein val token)) cands
      sxs = sortBy (\(_, _, i) (_, _, j) -> compare i j) xs
  in sxs

-- | Given the rules, return the best guess for this token
bestGuess :: Rules -> Text -> (TokenType, Text)
bestGuess rules token =
  let cands = envToCandidates rules
      matches = getMatches token cands
      (tt, val, weight) = head matches
  in  (tt, val)

-- | Given the rules and a text, 
productionToCategories :: Rules -> Text -> [(TokenType, Text)]
productionToCategories rules input =
  fmap (bestGuess rules) $ T.words input

-- | Pretty print one production rule
ppProduction :: Rules -> Text -> Text -> (Text, Text) -> Text
ppProduction rules name prefix (rname, prod) =
  let cats = productionToCategories rules prod
      args = concatMap (\(tt, name) ->
                          case tt of
                            TkNonTerminal -> [name]
                            TkMetavariable -> [name]
                            _ -> []
                       ) cats
  in mconcat $ ["| ", prefix, rname, " : ",
                T.intercalate " -> " args, " -> ", name]

extractCoq :: Rules -> NTRrule -> Text
extractCoq rules (NTRrule name prefix prods ntrs) =
  let xs = fmap (ppProduction rules name prefix) prods
  in mconcat $ ["Inductive ", name, " := \n"] ++ (prependBlockLns "  " . endWithPeriod) xs
