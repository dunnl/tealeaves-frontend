{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.Traversable
import System.IO
import Control.Monad.IO.Class
import App
import Rules
import Coq
import PP

import StateWithFuture

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
test :: Char -> StateWithFuture Int String
test c = stateWithFuture $ \count -> (\total -> show total, count + 1)

main :: IO ()
main = do
  print $ forWithFuture 0 ("hello" :: String) test
  config <- initialize
  runAppWith config () $ do
    app_logLn debugInfo "Aut initialized successfully. Attempting to parse rule set."
    rules <- readRules
    app_logLn debugInfo $ "Found rules: " <> T.pack (show rules)
    for_ (rls_ntrs rules) $
      \(Ntr name prefix productions ntrs) ->
        do app_logLn debugInfo $ "Pretty-printing rule \"" <> name <> "\""
           lines <- for productions $
             \production ->
               do app_logLn debugInfo "Pretty-printing a production string."
                  textOfProduction rules name prefix production
           app_writeLn $ "Inductive " <> name <>  " :="
           app_writeLns $ (prependBlock "  " . endWithPeriod) lines
