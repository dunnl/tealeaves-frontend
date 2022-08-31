{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO
import Driver
import Rules

exrule1 :: NTRrule
exrule1 = NTRrule "term" "t_"
  [ ("var", "v")
  , ("app", "t u")
  , ("abs", "\\ t")
  ]
  ["t", "u"]
  
exrule2 :: Metavar
exrule2 = Mvr "var" ["v"] "leaf"
  
exterm1 :: Terminal
exterm1 = Terminal "lambda" "\\"

rules :: Rules
rules =  Rules [exrule1] [exterm1] [exrule2]

main :: IO ()
main = do
  config <- initialize
  runApp config $ do
    Driver.log debugInfo "Initialized successfully\n"
    Driver.log debugInfo $ T.pack $ show $ bestGuess rules "x"
    Driver.writeLn (extractCoq rules exrule1)
