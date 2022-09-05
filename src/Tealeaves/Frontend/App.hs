{-# language OverloadedStrings #-}

{-# language FlexibleInstances      #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeSynonymInstances   #-}

module Tealeaves.Frontend.App where

import System.IO
import Control.Monad.RWS
import Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Traversable
import Data.Foldable
import Options.Applicative

import Tealeaves.Frontend.TraversableExtra
import Tealeaves.Frontend.Rules
import Tealeaves.Frontend.Logging

-- | Configuration type of the main application
data Configuration = Config
  { conf_infile  :: String -- ^ Input file containing user's syntax specification
  , conf_outfile :: String -- ^ Output file to pretty-print the Coq output
  , conf_logfile :: String -- ^ A log file to dump logging information
  , conf_debug   :: Int -- ^ Debugging threshold (log messages below this level are suppressed)
  }

confParser :: Parser Configuration
confParser = Config
  <$> strOption ( long "in"
                <> short 'i'
                <> value "input.txt"
                <> metavar "IN"
                <> help "Input filename")
  <*> strOption ( long "out"
                <> short 'o'
                <> value "output.txt"
                <> metavar "OUT"
                <> help "Output filename")
  <*> strOption ( long "log"
                <> value "log.txt"
                <> metavar "LOG"
                <> help "Log filename")
  <*> option auto
          ( long "verbosity"
         <> help "Log verbosity level"
         <> showDefault
         <> value debugWarning
         <> metavar "INT" )

initialize :: IO Configuration
initialize = execParser opts
  where
    opts = info (confParser <**> helper)
      ( fullDesc
     <> progDesc "Generate a Coq inductive definition"
     <> header "aut - a cheap Haskell alternative to OTT")

withThreeFiles ::
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  (Handle -> Handle -> Handle -> IO r) -> IO r
withThreeFiles fp0 md0 fp1 md1 fp2 md2 action =
    withFile fp0 md0 $
    \h0 -> withFile fp1 md1 $
           \h1 -> withFile fp2 md2 $
                  \h2 -> action h0 h1 h2

-- | Environment in which the application executes
data Environment = Env
  { env_inh  :: Handle
  , env_outh :: Handle
  , env_logh :: Handle
  , env_debug :: Int
  }

-- | Monad transformer for the main application
type AppT w s m = RWST Environment w s m

type App w s = AppT w s IO

instance (Monoid w) => HasLogging (App w s) Text where
  log = app_log

-- | 'evalRWST' with its arguments flipped
runNestedAppT :: (Monad m, Monoid w, Monoid w') =>
                 s' ->
                 AppT w' s' m b ->
                 AppT w s m (b, w')
runNestedAppT s action = do
  env <- ask
  lift $ runAppT env s action

-- | 'evalRWST' with its arguments flipped
runAppT :: (Monad m, Monoid w) => Environment -> s -> AppT w s m a -> m (a, w)
runAppT env st action = evalRWST action env st

runApp :: (Monoid w) => Environment -> s -> App w s a -> IO (a, w)
runApp = runAppT

runAppWith :: (Monoid w) => Configuration -> s -> App w s a -> IO (a, w)
runAppWith (Config inf out log dbg) st action = do
  withThreeFiles inf ReadMode out WriteMode log WriteMode
    (\hi ho hl -> runApp (Env hi ho hl dbg) st action)

-- | Print a log message to the log file.
app_log :: (Monoid w) => Int -> Text -> App w s ()
app_log lvl msg = do
  threshold <- asks env_debug
  logh <- asks env_logh
  when (threshold >= lvl) $
    liftIO (T.hPutStr logh msg)

-- | Print a log message to the log file, appending a newline.
app_logLn :: (Monoid w) => Int -> Text -> App w s ()
app_logLn lvl msg = app_log lvl (msg <> "\n")

-- | Print a 'Text' value to the output file.
app_write :: Text -> App [Text] s ()
app_write msg = do
  tell [msg]
  {-
  outh <- asks env_outh
  liftIO (T.hPutStr outh msg)
  -}

-- | Print several 'Text' values to the output file.
app_writes :: [Text] -> App [Text] s ()
app_writes msgs = do
  for_ msgs app_write

-- | Print a 'Text' value to the output file, appending a newline.
app_writeLn :: Text -> App [Text] s ()
app_writeLn msg = app_write (msg <> "\n")

-- | Print several 'Text' values to the output file, appending a newline to all except the last.
app_writeLns :: [Text] -> App [Text] s ()
app_writeLns msgs = sequenceA_ (go msgs)
  where go msgs = case msgs of
          [] -> [return ()]
          (x : []) -> app_write x : []
          (x : xs) -> app_writeLn x : go xs

-- | Attempt to parse the user's 'Rules' from the input file.
readRules :: (Monoid w) => App w s Rules
readRules = do
  inh <- asks env_inh
  inf <- liftIO $ BL.hGetContents inh
  case A.eitherDecode inf of
    Left str -> do
      app_log debugError "Error during readRules: "
      app_logLn debugError (T.pack str)
      error "abort"
    Right rules -> return rules
