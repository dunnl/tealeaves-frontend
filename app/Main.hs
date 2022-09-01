{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Control.Monad.IO.Class
import Driver
import Rules
import Coq

{-
term_rule :: NTRrule
term_rule = NTRrule "term" "t_"
  [ ("var", "v")
  , ("app", "t u")
  , ("abs", "\\ t")
  ]
  ["t", "u"]
  
var_rule :: Metavar
var_rule = Mvr "var" ["v"] "leaf"
  
lambda_rule :: Terminal
lambda_rule = Terminal "lambda" "\\"

rules :: Rules
rules =  Rules [var_rule] [lambda_rule] [term_rule]
-}

main :: IO ()
main = do
  config <- initialize
  runApp config $ do
    app_logLn debugInfo "Initialized successfully"
    rules <- readRules
    app_logLn debugInfo $ T.pack $ show $ rules
    traverse (\ntr -> extractCoq rules ntr >> app_writeLn "") (rls_ntrs rules)
    return ()
   
