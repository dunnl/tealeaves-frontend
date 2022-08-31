{-# language OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import System.IO
import Driver
import Rules

exrule1 :: NTRrule
exrule1 = NTRrule "term" "t_"
  [ ("var", "x")
  , ("app", "t t")
  , ("abs", "\\ t")
  ]
  ["t", "u"]
  
exrule2 :: NTRrule
exrule2 = NTRrule "var" "v_"
  [ ("var", "x")
  ]
  ["x"]
  
exterm1 :: Terminal
exterm1 = Terminal "lambda" "\\"

exenv1 :: Rules
exenv1 =  Rules [exrule1, exrule2] [exterm1]

main :: IO ()
main = do
  config <- initialize
  runApp config $ do
    Driver.log debugInfo "Initialized successfully\n"
    Driver.writeLn (extractCoq exenv1 exrule2)
    Driver.writeLn "\n"
    Driver.writeLn (extractCoq exenv1 exrule1)
