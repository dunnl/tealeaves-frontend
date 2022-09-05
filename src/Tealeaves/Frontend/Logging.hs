{-# LANGUAGE MultiParamTypeClasses #-}

module Tealeaves.Frontend.Logging where

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
