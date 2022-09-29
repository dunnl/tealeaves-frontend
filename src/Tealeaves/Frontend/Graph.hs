{-# language OverloadedStrings #-}

{-|
Module      : Graph.hs
Description : Support for computing with graphs
Copyright   : (c) Lawrence Dunn, 2022
-}

module Tealeaves.Frontend.Graph where

import           Data.Graph
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T

import Tealeaves.Frontend.Coq
import Tealeaves.Frontend.Rules

-- | A map from names to rules
type TypeMap = Map Symbol CoqInductiveType

-- | A map from the name of some 'Nonterminal' @ntr@ to names of other
-- 'Nonterminal's appearing as types of arguments to constructors of @ntr@.
type NtrReachMap = Map Symbol [Symbol]

getEdges :: TypeMap ->
            NtrReachMap ->
            [(CoqInductiveType, Symbol, [Symbol])]
getEdges types reaches =
  foldMap go (M.toList reaches) :: [(CoqInductiveType, Symbol, [Symbol])]
  where
    go :: (Symbol, [Symbol]) -> [(CoqInductiveType, Symbol, [Symbol])]
    go (name, reaches) =
      case M.lookup name types of
        Nothing -> error $ show ("graphFromNameMap: Didn't find name " <> name <> " in rule map " <> (T.pack (show types)))
        Just x -> [(x, name, reaches)]

graphFromNameMap :: TypeMap ->
                    NtrReachMap ->
                    (Graph, Vertex -> (CoqInductiveType, Symbol, [Symbol]), Symbol -> Maybe Vertex)
graphFromNameMap types reaches =
  graphFromEdges $ getEdges types reaches

getStronglyConnectedComponents :: TypeMap ->
                                  NtrReachMap ->
                                  [[CoqInductiveType]]
getStronglyConnectedComponents types reaches =
  flattenSCC <$> (stronglyConnComp (getEdges types reaches))
