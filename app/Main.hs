{-# language OverloadedStrings #-}

module Main where

import Data.Text (Text)
import qualified Data.Text as T
import Tealeaves.Frontend as Tea
import System.IO
import Control.Monad.RWS

main :: IO ()
main = do
  config <- initialize
  runAppWith config () $ do
    app_logLn debugInfo "Initialized the runtime environment. Attempting to parse input rule set."
    rules <- readRules
    app_logLn debugInfo $ "Dumping rules: " <> T.pack (show rules)
    symt <- buildSymbolTable rules
    Tea.log 3 $ T.pack $ show (length $ rls_ntrs rules)
    (_, pprules) <- runNestedAppT () $ for_ (rls_ntrs rules) $
      \rule@(Ntr name prefix productions ntrs) ->
        do app_logLn debugInfo $ "Pretty-printing rule \"" <> name <> "\""
           app_writeLn $ "Inductive " <> name <>  " :="
           lines <- for productions $
             \production@(Pr pr_name pr_exp pr_bindmap) ->
               do app_logLn debugInfo $ mconcat ["Pretty-printing production ", prefix, pr_name]
                  getTextOfProduction symt name prefix production
           tell $ [ (prependBlock "  " . endWithPeriod) lines ]
    for_ (pprules :: [[Text]]) $
      \rule -> liftIO $ putStrLn $ show rule
    tell [3]
  return ()
