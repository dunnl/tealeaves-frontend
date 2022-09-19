{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}

{-|
Module      : Coq.hs
Description : Datatypes and class instances for Coq entities
Copyright   : (c) Lawrence Dunn, 2022
-}

module Tealeaves.Frontend.Coq where

--import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Tealeaves.Frontend.PP
import Tealeaves.Frontend.Rules -- this module implement some PP helpers for 'Rule's

data CoqInductiveType = CoqIT
  { it_name :: Text
  , it_args :: [(Text, Maybe Text)] -- ^ type argument name and sort
  , it_sort :: Maybe Text
  , it_constr :: [CoqConstructor]
  } deriving (Show)

data CoqConstructor = CoqC
  { con_name :: Text
  , con_args :: [Text]
  } deriving (Show)

data CoqFixpoint = CoqFix
  { fix_name :: Text
  , fix_args :: [Text]
  , fix_type :: Maybe Text
  , fix_recvar :: Text -- ^ The variable to structurally recurse on
  , fix_cases :: [CoqFixCase]
  } deriving (Show)

data CoqFixCase = CoqFixCase
  { fxc_constr :: Text
  , fxc_args :: [Text]
  , fxc_body :: Text
  } deriving (Show)

-- | Pretty-print one of the constructors of an inductive type.  The
-- inductive type argument is necessary because it contains the name
-- of the type, which is used when printing constructors.
ppConstructor :: CoqInductiveType -> CoqConstructor -> Text
ppConstructor it (CoqC name args) =
  "| " <> name <> " : " <> T.intercalate " -> " args <> " -> " <> (it_name it)

-- | Pretty-print an inductive type as a list of 'Text' values. The
-- head element is of the form @<name> <args> : <Sort> :=@ while each
-- successive line is of the form @| <constr> : <args> ->
-- <name>@. Lines are not terminated with newlines (nor a terminating
-- '.' character).
ppInductiveTypeCore :: CoqInductiveType -> [Text]
ppInductiveTypeCore it@(CoqIT name args msort constrs) =
  first_line : (ppConstructor it <$> constrs)
  where
    first_line = name <> " " <> (T.intercalate " " typeArgs) <> maybeSort <> " :="
    typeArgs = flip fmap args $ \(var, msort) ->
      maybe var (\sort -> "(" <> var <> " : " <> sort <> ")") msort
    maybeSort = case msort of {Just sort -> " : " <> sort; Nothing -> ""}

-- | Format the output of @ppInductiveTypeCore@, assuming the input is
-- the last element in a list of mutually-recursive types. The leading
-- keyword @Inductive@, if one is needed, is not included.
formatLastInductiveType :: [Text] -> Text
formatLastInductiveType =
  mconcat . onHead ln . indentTailLn 4 . endWithPeriod

-- | Format the output of @ppInductiveTypeCore@, assuming the input is
-- the not the last element in a list of mutually-recursive types, so
-- that a terminating '.' is not included.
formatInitInductiveType :: [Text] -> Text
formatInitInductiveType =
  mconcat . onHead ln . indentTailLn 4

-- | Pretty-print an inductive type.
ppInductiveType :: CoqInductiveType -> Text
ppInductiveType it =
  let lines = ppInductiveTypeCore it
  in "Inductive " <> formatLastInductiveType lines

-- | Pretty-print a mutually-recursive set of inductive types.
ppInductiveTypes :: [CoqInductiveType] -> Text
ppInductiveTypes set = case set of
  [] -> error "Cannot pretty-print an empty list of mutually-recursive inductive types!"
  (x : []) -> "Inductive " <> formatLastInductiveType (ppInductiveTypeCore x)
  (x : xs) -> "Inductive " <> (formatInitInductiveType (ppInductiveTypeCore x)) <> (go xs)
  where
    go (x : []) = "  with " <> (formatLastInductiveType (ppInductiveTypeCore x))
    go (x : xs) = "  with " <> (formatInitInductiveType (ppInductiveTypeCore x)) <> (go xs)

ppFixCase :: CoqFixCase -> Text
ppFixCase (CoqFixCase constr args body) =
  "| " <> constr <> " " <> T.intercalate " " args <> " => " <> body

ppFixpoint :: CoqFixpoint -> Text
ppFixpoint (CoqFix name args mtype recvar cases) =
  "Fixpoint " <> name <> " " <> T.intercalate " " args <> maybeType <> " := match " <> recvar <> " with \n"
    <> (mconcat . indentLnAll 4 . fmap ppFixCase $ cases)
    <> indentLn 4 "end."
  where
    maybeType = case mtype of {Just typ -> " : " <> typ; Nothing -> ""}

instance PrettyPrint CoqInductiveType where
  prettyPrint = ppInductiveType

instance PrettyPrint [CoqInductiveType] where
  prettyPrint = ppInductiveTypes

instance PrettyPrint CoqFixpoint where
  prettyPrint = ppFixpoint

--- Pure utilities
constrName :: Nonterminal ->
              ProductionRule ->
              Text
constrName ntr pr = ntr_prefix ntr <> pr_name pr
