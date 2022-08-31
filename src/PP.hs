{-# language OverloadedStrings #-}

module PP where

import qualified Data.Text as T
import Data.Text (Text)

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

endWithPeriod :: [Text] -> [Text]
endWithPeriod lines =
  case reverse lines of
    [] -> []
    (ln : rest) -> reverse rest ++ [ln <> "."]
      
