{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Traversable as Tr
import System.IO
import Control.Monad.IO.Class
import Driver
import Rules
import Coq
import PP

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
    Tr.for (rls_ntrs rules) $
      \(Ntr name prefix prods ntrs) ->
        do app_writeLn $ mconcat $ ["Inductive ", name, " :="]
           lines <- Tr.for prods (ppProduction rules name prefix)
           app_write $ mconcat $ (prependBlockLns "  " . endWithPeriod) lines
    return ()
