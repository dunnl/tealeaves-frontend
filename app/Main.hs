{-# language OverloadedStrings #-}

module Main where

import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Tealeaves.Frontend as Tea
import System.IO
import Control.Monad.RWS

main :: IO ()
main = do
  config <- initialize
  runAppWith config $ do
    add_context ()
    app_logLn debugInfo "Initialized the runtime environment. Attempting to parse input rule set."
    rules <- readRules
    app_logLn debugInfo $ "Dumping rules: " <> T.pack (show rules)
    symt <- buildSymbolTable rules
    env <- ask
    lines <- stackOf $ for_ (rls_ntrs rules) $
      \(Ntr name prefix productions ntrs) -> do
        lines <- stackOf $ do
          app_logLn debugInfo $ "Pretty-printing rule \"" <> name <> "\""
          push $ "Inductive " <> name <>  " :="
          for productions $
            \production@(Pr pr_name pr_exp pr_bindmap) ->
              do app_logLn debugInfo $ mconcat ["Pretty-printing production ", prefix, pr_name]
                 line <- getTextOfProduction symt name prefix production
                 push line
        push $ formatRule "  " lines
    app_write (T.intercalate "\n" lines)
