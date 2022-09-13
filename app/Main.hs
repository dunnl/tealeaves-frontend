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
    app_writeLn "(* begin inductive type definitions *)"
    type_declarations <- ppIDefs rules
    app_write type_declarations
    app_writeLn "(* end inductive type definitions *)"
    app_writeLn "(* begin function definitions *)"
    fn_defs <- ppFnDefs rules
    app_write fn_defs
    app_writeLn "(* end function definitions *)"
