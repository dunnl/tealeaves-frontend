{-# language OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad.State.Class
import Data.Foldable
import Data.Traversable
import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import App
import Rules
import Coq
import PP

import StateWithFuture

-- test :: Char -> StateWithFuture Int String
-- test c = stateWithFuture $ \count -> (\total -> show total, count + 1)
-- print $ forWithFuture 0 ("hello" :: String) test

analyzeString :: Text -> StateWithFuture Int Int
analyzeString str = do
  modify (+1)
  askFinal
  --final <- askFinal
  --return final

analyzeString' :: Text -> App Int Int
analyzeString' str = do
  modify (+1)
  lift askFinal

analyzeString'' :: Text -> App Int Int
analyzeString'' str = do
  modify (+1)
  final <- lift askFinal
  return final

{-
analyzeString :: Text -> App Int Text
analyzeString str = do
  current <- get
  total <- lift askFinal
  app_logLn debugInfo $ "Analyzing string " <> (T.pack (show current)) <> "/" <>
    (T.pack (show total)) <> ": \"" <> str <> "\""
  modify (+1)
  return str
-}

my_words :: [Text]
my_words = ["the", "quick", "brown", "fox", "jumped", "over", "the", "lazy", "dog"]

main :: IO ()
main = do
  config <- initialize
  {-
  ints <- runAppWith config 0 $
            for my_words $
             (\word -> analyzeString'' word)
  -}

  let ints = forWithFuture 0 my_words $
             (\word -> analyzeString word)
  liftIO $ putStrLn $ show ints
  return ()

  {-
  config <- initialize
  runAppWith config () $ do
    app_logLn debugInfo "Initialized the runtime environment. Attempting to parse input rule set."
    rules <- readRules
    app_logLn debugInfo $ "Dumping rules: " <> T.pack (show rules)
    symt <- buildSymbolTable rules
    for_ (rls_ntrs rules) $
      \(Ntr name prefix productions ntrs) ->
        do app_logLn debugInfo $ "Pretty-printing rule \"" <> name <> "\""
           lines <- for productions $
             \production@(Pr pr_name pr_exp pr_bindmap) ->
               do app_logLn debugInfo $ mconcat ["Pretty-printing production ", prefix, pr_name]
                  textOfProduction symt name prefix production
           app_writeLn $ "Inductive " <> name <>  " :="
           app_writeLns $ (prependBlock "  " . endWithPeriod) lines
-}
