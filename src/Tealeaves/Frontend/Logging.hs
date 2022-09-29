{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings     #-}
{-# language FlexibleInstances     #-}
{-# language FlexibleContexts      #-}

{-|
Module      : Logging.hs
Description : Typeclasses and utilities for monads which support logging
Copyright   : (c) Lawrence Dunn, 2022
-}

module Tealeaves.Frontend.Logging where

import Control.Monad.State.Strict
import Control.Monad.Trans
import Data.Text (Text)

-- | Debug level for fatal errors
debugError :: Int
debugError = 0

-- | Debug level for non-fatal but suspicious conditions
debugWarning :: Int
debugWarning = 1

-- | Debug level for general information
debugInfo :: Int
debugInfo = 2

-- | Debug level for detailed tracing
debugTrace :: Int
debugTrace = 3

class HasLogging m msg where
  log :: Int -> msg -> m ()

logLn :: (HasLogging m Text) => Int -> Text -> m ()
logLn = \i msg -> Tealeaves.Frontend.Logging.log i (msg <> "\n")

{-
instance (HasLogging m msg, Monad m, MonadTrans t) => HasLogging (t m) msg where
  log = \i msg -> lift $ Tealeaves.Frontend.Logging.log i msg
-}

instance (Monad m, HasLogging m msg) => HasLogging (StateT s m) msg where
  log = \i msg -> lift (Tealeaves.Frontend.Logging.log i msg)
