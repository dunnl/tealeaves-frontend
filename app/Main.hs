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
    app_logLn debugInfo "Initialized the runtime environment. Attempting to parse input rule set."
    rules <- readRules
    app_logLn debugInfo $ "Dumping rules: " <> T.pack (show rules)
    type_declarations <- ppInductiveTypes rules
    app_write type_declarations
    --symt <- buildSymbolTable rules
    --line <- stackOf $ ppBinddt symt (head (rls_ntrs rules))
    --app_write "\n"
    --app_writes line
