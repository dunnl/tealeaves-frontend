{-|
Module      : PP
Description : Typeclasses and helper functions for pretty-printing Coq entities
Copyright   : (c) Lawrence Dunn, 2022
-}

{-# language OverloadedStrings #-}

module Tealeaves.Frontend.PP where

import Data.Text (Text)
import qualified Data.Text as T

-- | A class for things which can be pretty-printed. The returned text
-- value should contain a trailing newline.
class PrettyPrint t where
  prettyPrint :: t -> Text

-- | Apply a function to all but the final element of a list, if it exists
mapInit :: (a -> a) -> [a] -> [a]
mapInit f xs = case xs of
  [] -> []
  x : [] -> x : []
  x : xs ->  x : mapInit f xs

-- | Apply a function to the final element of a list, if it exists
mapLast :: (a -> a) -> [a] -> [a]
mapLast f xs = case xs of
  [] -> []
  x : [] -> f x : []
  x : xs -> x : mapLast f xs

-- | Apply a function to the first element of a list, if it exists
mapHead :: (a -> a) -> [a] -> [a]
mapHead f xs = case xs of
  [] -> []
  x : xs -> f x : xs

-- | Apply a function to all but the first element of a list, if it exists
mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = case xs of
  [] -> []
  x : xs -> x : map f xs

-- |
mapSegments :: (a -> a) -> (a -> a) -> (a -> a) -> [a] -> [a]
mapSegments fh fb fl xs = case xs of
  [] -> []
  x : [] -> fl (fh x) : []
  x : rest -> fh x : go rest
  where go [] = error "This cannot happen"
        go (x : []) = fl x : []
        go (x : xs) = fb x : go xs

-- | Prepend a string with a number of spaces
indent :: Int -> Text -> Text
indent n s = (T.replicate n " ") <> s

-- | Append a newline
ln :: Text -> Text
ln s = s <> "\n"

-- | Prepend with spaces and insert a linebreak
indentLn :: Int -> Text -> Text
indentLn n s = T.replicate n " " <> s <> "\n"

-- | Prepend spaces to each line
indentAll :: Int -> [Text] -> [Text]
indentAll n strs = fmap (indent n) strs

-- | Prepend spaces and add a linebreak to each line
indentLnAll :: Int -> [Text] -> [Text]
indentLnAll n strs = fmap (indentLn n) strs

-- | Indent each line except the head
indentTail :: Int -> [Text] -> [Text]
indentTail n strs = onTail (indentAll n) strs

-- | For each line except the head, indent and append a newline.
indentTailLn :: Int -> [Text] -> [Text]
indentTailLn n strs = onTail (indentLnAll n) strs

-- | Apply a list operation only to the tail of a list, keeping the head
onTail :: ([a] -> [a]) -> [a] -> [a]
onTail f [] = []
onTail f (x : xs) = x : f xs

-- | Apply an operation the head element of a list, if there is one
onHead :: (a -> a) -> [a] -> [a]
onHead fn [] = []
onHead fn (x : xs) =  fn x : xs

-- | Apply an operation to the last element of a list, if there is one
onLast :: (a -> a) -> [a] -> [a]
onLast fn [] = []
onLast fn (x : []) = fn x : []
onLast fn (x : xs) =  x : onLast fn xs

-- | Given string @new@ and @lines@, prepend the first line
-- with @new@ and indent the rest by @length new@
prependBlock :: Text -> [Text] -> [Text]
prependBlock new lines =
  mapSegments (\head -> new <> head)
              (\body -> indent (T.length new) body)
              (\last -> indent (T.length new) last)
              lines

-- | Append a 'Text' value to all but the last line of list
-- e.g. a newline
appendAllButLast :: Text -> [Text] -> [Text]
appendAllButLast = \end -> mapLast (<> end)

-- | Append a 'Text' value to the last line of list
-- e.g. a period
appendLastWith :: Text -> [Text] -> [Text]
appendLastWith = \end -> mapLast (<> end)

-- | Append a '.' to the last line of a list of 'Text' values
endWithPeriod :: [Text] -> [Text]
endWithPeriod = appendLastWith "."

-- | Append a period and newline to the last line of a list of 'Text' values
endWithPeriodNewline :: [Text] -> [Text]
endWithPeriodNewline = appendLastWith ".\n"

-- | Given the list of lines constituting an inductive definition,
-- indent all body lines by 4 spaces, add a period to the final line,
-- and terminate each line with a newline character. Concatenat the
-- lines to a single 'Text' value.
concatDefinition :: [Text] -- ^ A set of un-terminated lines of an inductive definition
                 -> Text -- ^ The concatenation of the lines as a single un-terminated 'Text' value.
concatDefinition lines =
  T.concat $ mapSegments (\head -> head <> "\n")
                         (\body -> indentLn 4 body)
                         (\last -> indentLn 4 (last <> "."))
                         lines
concatFunction :: [Text] -- ^ A set of un-terminated lines of an inductive definition
                 -> Text -- ^ The concatenation of the lines as a single un-terminated 'Text' value.
concatFunction lines =
  T.concat $ mapSegments (\head -> head <> "\n")
                         (\body -> indentLn 4 body)
                         (\last -> indentLn 4 (last <> "."))
                         (lines ++ ["end."])
