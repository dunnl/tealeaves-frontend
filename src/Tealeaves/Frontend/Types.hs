module Tealeaves.Frontend.Coq where

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
