{-# language OverloadedStrings    #-}
{-# language DeriveGeneric        #-}

module Tealeaves.Frontend.Rules where

import           Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import           GHC.Generics

data TokenType = TkTr | TkNtr | TkMvr deriving (Eq, Show)

-- | Names of rules (used for error reporting and to name inductive definitions)
type Name = Text

-- | A token that occurs in some production expression for a
-- non-terminal.  A symbol can represent a metavariable, non-terminal,
-- or terminal in the grammar.
type Symbol = Text

-- | Textual values intended to be pretty printed to a @.v@ file.
-- Note that 'Name's of production rules are
type CoqStr = Text

-- | Metavariable rule
-- A metavariable is a symbol used in place of object-level variables, whose types
-- are not inductively defined with this tool but stand for some Coq type.
data Metavar = Mvr
  { mvr_name    :: Name -- ^ Used for logging/error reporting only
  , mvr_symbols :: [Symbol] -- ^ Symbols that can be used for this metavar
  , mvr_coqtype :: CoqStr -- ^ The Coq type these are translated to
  } deriving (Generic, Show)

instance FromJSON Metavar where
  parseJSON = A.withObject "Metavar" $ \v -> Mvr
        <$> v .: "name"
        <*> v .: "symbols"
        <*> v .: "coq"

instance ToJSON Metavar where

 -- | Maps from production expression symbols to the binder symbol (if any) specified
 -- as being in scope at this symbol.
type BindMap = Map Symbol Symbol

 -- | Triplets @(\<production name\>, \<production expression\>, \<bind map\>)@
data ProductionRule = Pr Name Text (Maybe BindMap)
  deriving (Generic, Show)

instance FromJSON ProductionRule where

instance ToJSON ProductionRule where

-- | Non-terminal rule
data Nonterminal = Ntr
  { ntr_name :: Name -- ^ Used for error reporting and the generated inductive type
  , ntr_prefix :: CoqStr -- ^ Common prefix used for generated constructor names in Coq
  , ntr_productions :: [ProductionRule] -- ^ Associated production rules
  , ntr_symbols :: [Symbol] -- ^ Symbols that can represent this non-terminal in production expressions
  } deriving (Generic, Show)

instance FromJSON Nonterminal where
  parseJSON = A.withObject "Nonterminal" $ \v -> Ntr
        <$> v .: "name"
        <*> v .: "prefix"
        <*> v .: "productions"
        <*> v .: "symbols"

instance ToJSON Nonterminal where

-- | Terminal rule
data Terminal = Tr
  { tr_name :: Name
  , tr_symbol :: Symbol
  } deriving (Generic, Show)

instance FromJSON Terminal where
  parseJSON = A.withObject "Terminal" $ \v -> Tr
        <$> v .: "name"
        <*> v .: "symbol"

instance ToJSON Terminal where

-- | The set of grammatical rules in the specification of a user's
-- syntax.
data Rules = Rules
  { rls_mvrs :: [Metavar]
  , rls_trs  :: [Terminal]
  , rls_ntrs :: [Nonterminal]
  } deriving (Generic, Show)

data AnyRule =
    Rl_ntr Nonterminal
  | Rl_mvr Metavar
  | Rl_tr  Terminal
  deriving (Show)

instance FromJSON Rules where
  parseJSON = A.withObject "Rules" $ \v -> Rules
        <$> v .: "metavariables"
        <*> v .: "terminals"
        <*> v .: "nonterminals"

instance ToJSON Rules where
