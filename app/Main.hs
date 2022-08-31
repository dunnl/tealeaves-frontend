{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Driver
import Rules

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
    Driver.log debugInfo "Initialized successfully\n"
    --Driver.log debugInfo $ T.pack $ show $ bestGuess rules "x"
    rules <- readRules
    Driver.log debugInfo $ T.pack $ show $ rules
    let defns = T.intercalate "\n" $ fmap (\ntr ->  (extractCoq rules ntr)) (rls_ntrs rules)
    Driver.writeLn defns
