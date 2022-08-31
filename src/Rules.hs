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

-- | Names of rules (used for error reporting and/or to name inductive definitions)
type Name = Text
-- | Symbols that can be used for a certain metavariable or non-terminal
type Symbol = Text

-- | Metavariable rule
-- A metavariable is a symbol used in place of object-level variables, whose types
-- are not inductively defined with this tool but stand for some Coq type.
data Metavar = Mvr
  { mvr_name :: Name -- ^ Used for logging/error reporting only
  , mvr_symbols :: [Symbol] -- ^ Symbols that can be used for this metavar
  , mvr_coqtype :: Text -- ^ The Coq type these are translated to
  } deriving (Generic, Show)

instance FromJSON Metavar where
  parseJSON = A.withObject "Metavar" $ \v -> Mvr
        <$> v .: "name"
        <*> v .: "symbols"
        <*> v .: "coq"
instance ToJSON Metavar where

-- | Non-terminal rule
data NTRrule = NTRrule
  { ntr_name :: Name -- ^ Used for error reporting and the generated inductive type
  , ntr_prefix :: Text -- ^ Common prefix for production names (in Coq, constructor names)
  , ntr_productions :: [( Name -- ^ production name
                        , Text -- ^ production expression
                        )]
  , ntr_symbols :: [Symbol] -- Symbols that can represent this non-terminal
  } deriving (Generic, Show)

instance FromJSON NTRrule where
  parseJSON = A.withObject "NTRrule" $ \v -> NTRrule
        <$> v .: "name"
        <*> v .: "prefix"
        <*> v .: "productions"
        <*> v .: "symbols"
        
--instance ToJSON NTRrule where

-- | Terminal rule
data Terminal = Terminal
  { tr_name :: Name
  , tr_symbol :: Symbol
  } deriving (Generic, Show)

instance FromJSON Terminal where
  parseJSON = A.withObject "Terminal" $ \v -> Terminal
        <$> v .: "name"
        <*> v .: "symbol"
        
--instance ToJSON Terminal where
  
-- | Rules
data Rules = Rules
  { env_mvrs :: [Metavar]
  , rls_trs  :: [Terminal]
  , rls_ntrs :: [NTRrule]
  } deriving (Generic, Show)

instance FromJSON Rules where
  parseJSON = A.withObject "Rules" $ \v -> Rules
        <$> v .: "metavariables"
        <*> v .: "terminals"
        <*> v .: "nonterminals"
        
--instance ToJSON Rules where

-- | A 'Candidate' is a pattern paired with the name of the accompanying rule name
-- which should be printed in place of this occurrence
type Candidate = (Symbol, TokenType, Text)

-- | Given the rules, list all possible candidate symbols we could encounter,
-- along with their TokenType, and the symbol to use in their pretty printer
envToCandidates :: Rules -> [Candidate]
envToCandidates (Rules mvrs trs ntrs) =
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
