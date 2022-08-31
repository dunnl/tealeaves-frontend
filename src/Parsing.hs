{-# language OverloadedStrings #-}

module Parsing where

import Data.Char as C
import Data.Text (Text)
import qualified Data.Text as T

isTick :: Char -> Bool
isTick = ('\'' ==)

-- | Given a string, parse it into an alphabetic prefix and numerical or ' suffix
readNTR :: Text -> Maybe (Text, Text)
readNTR token =
  let (alpha, rest) = T.span C.isAlpha token in
    if (T.length alpha == 0 || (not $ T.all C.isNumber rest))
    then Nothing
    else Just (alpha, rest)
