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

-- | Non-terminal rule
data NTRrule = NTRrule
  { ntr_name :: Text
  , ntr_prefix :: Text
  , ntr_productions :: [(Text, Text)]
  , ntr_nonterminals :: [Text]
  } deriving (Generic, Show)

instance FromJSON NTRrule where
instance ToJSON NTRrule where

-- | Terminal rule
data Terminal = Terminal
  { tr_name :: Text
  , tr_value :: Text
  } deriving (Generic, Show)

instance FromJSON Terminal where
instance ToJSON Terminal where
  
-- | Rules
data Rules = Rules
  { env_ntrs :: [NTRrule]
  , env_trs :: [Terminal]
  } deriving (Generic, Show)

instance FromJSON Rules where
instance ToJSON Rules where

-- | A 'Candidate' is a pattern paired with the name of the accompanying rule
type Candidate = (Text, TokenType, Text)

-- | 
envToCandidates :: Rules -> [Candidate]
envToCandidates (Rules ntrs trs) =
  let toCandidateNtr rule = fmap (\str -> (str, TkNonTerminal, ntr_name rule)) (ntr_nonterminals rule)
      toCandidateTr (Terminal name value) = (value, TkTerminal, name)
  in concatMap toCandidateNtr ntrs ++
     fmap toCandidateTr trs

getMatches :: Text -> [Candidate] -> [(TokenType, Text, Int)]
getMatches token cands =
  let xs = fmap (\(val, tt, name) -> (tt, name, Metrics.levenshtein val token)) cands
      sxs = sortBy (\(_, _, i) (_, _, j) -> compare i j) xs
  in sxs

bestGuess :: Rules -> Text -> (TokenType, Text)
bestGuess env token =
  let cands = envToCandidates env
      matches = getMatches token cands
      (tt, val, weight) = head matches
  in  (tt, val)

productionToCategories :: Rules -> Text -> [(TokenType, Text)]
productionToCategories env input =
  let words = T.words input
  in fmap (bestGuess env) words

ppProduction :: Rules -> Text -> Text -> (Text, Text) -> Text
ppProduction env name prefix (rname, prod) =
  let cats = productionToCategories env prod
      args = fmap snd $ filter (\(tt, _) -> tt == TkNonTerminal) cats
  in mconcat $ ["| ", prefix, rname, " : ",
                T.intercalate " -> " args, " -> ", name]

extractCoq :: Rules -> NTRrule -> Text
extractCoq env (NTRrule name prefix prods ntrs) =
  let xs = fmap (ppProduction env name prefix) prods
  in mconcat $ ["Inductive ", name, " := \n"] ++ (prependBlockLns "  " . endWithPeriod) xs
