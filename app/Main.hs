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
    app_logLn debugInfo "Initialized the runtime environment."
    rules <- readRules
    symt <- buildSymbolTable rules
    app_writeLn "(* begin inductive type definitions *)"
    type_declarations <- ppAbstractSyntaxTypes symt rules
    app_write type_declarations
    app_writeLn "(* end inductive type definitions *)"
    app_writeLn ""
    app_writeLn "(* begin function definitions *)"
    fn_defs <- ppAllFunctions symt rules
    app_write fn_defs
    app_writeLn "(* end function definitions *)"
