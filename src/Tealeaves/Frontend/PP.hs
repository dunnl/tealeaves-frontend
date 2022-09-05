{-|
Module      : Parsing
Description : Generic helper functions for pretty-printing
Copyright   : (c) Lawrence Dunn, 2022
-}

{-# language OverloadedStrings #-}

module Tealeaves.Frontend.PP where

import qualified Data.Text as T
import Data.Text (Text)

-- | Appliy a function to the final element of a list, if it exists
mapLast :: (a -> a) -> [a] -> [a]
mapLast fn xs = case xs of
  [] -> []
  x : [] -> fn x : []
  x : xs -> x : mapLast fn xs

-- | Prepend a string with a number of spaces
indent :: Int -> Text -> Text
indent n s = (T.replicate n " ") <> s

-- | Prepend with spaces and insert a linebreak
indentLn :: Int -> Text -> Text
indentLn n s = T.replicate n " " <> s <> "\n"

-- | Prepend spaces to each line
indentAll :: Int -> [Text] -> [Text]
indentAll n strs = fmap (indent n) strs

-- | Prepend spaces and add a linebreak to each line
indentLnAll :: Int -> [Text] -> [Text]
indentLnAll n strs = fmap (indentLn n) strs

-- | Given string @new@ and @lines@, prepend the first line
-- with @new@ and indent the rest by @length new@
prependBlock :: Text -> [Text] -> [Text]
prependBlock new [] = []
prependBlock new (str : rest) =
  (new <> str) : indentAll (T.length new) rest

-- | Given string @new@ and @lines@, prepend the first line
-- with @new@ and indent the rest by @length new@
prependBlockLns :: Text -> [Text] -> [Text]
prependBlockLns new [] = []
prependBlockLns new (str : rest) =
  (new <> str <> "\n") : indentLnAll (T.length new) rest

-- | Append a 'Text' value to the last line of list
appendLastWith :: Text -> [Text] -> [Text]
appendLastWith = \end -> mapLast (<> end)

-- | Append a '.' to the last line of a list of 'Text' values
endWithPeriod :: [Text] -> [Text]
endWithPeriod = appendLastWith "."

-- | Append a period and newline to the last line of a list of 'Text' values
endWithPeriodNewline :: [Text] -> [Text]
endWithPeriodNewline = appendLastWith ".\n"
