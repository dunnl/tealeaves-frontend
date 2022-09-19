{-# language OverloadedStrings #-}

{-|
Module      : Extra.hs
Description : Miscellaneous helper functions
Copyright   : (c) Lawrence Dunn, 2022
-}

foldFor :: (Foldable t, Monoid m) => t a -> (a -> m) -> m
foldFor = flip foldMap
