{-# language OverloadedStrings #-}

module Driver where

import Control.Monad.Reader
import System.IO
import Data.Aeson (ToJSON, FromJSON, (.:), (.=))
import qualified Data.Aeson as A
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative
import Rules

data Configuration = Config
  { conf_infile  :: String
  , conf_outfile :: String
  , conf_logfile :: String
  , conf_debug   :: Int }

data Environment = Env
  { env_inh  :: Handle
  , env_outh :: Handle
  , env_logh :: Handle
  , env_debug :: Int
  }

debugError :: Int
debugError = 0

debugWarning :: Int
debugWarning = 1

debugInfo :: Int
debugInfo = 2

debugDump :: Int
debugDump = 3

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

withTwoFiles ::
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  (Handle -> Handle -> IO r) -> IO r
withTwoFiles fp md fp' md' action =
  withFile fp md (\h -> do
                     withFile fp' md' (action h))
withThreeFiles ::
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  (Handle -> Handle -> Handle -> IO r) -> IO r
withThreeFiles fp0 md0 fp md fp' md' action =
  withFile fp0 md0 (\h1 -> do
                       withTwoFiles fp md fp' md' (action h1))

withConfiguration :: (Environment -> IO a) -> (Configuration -> IO a)
withConfiguration action (Config inf out log dbg) =
  withThreeFiles inf ReadMode out WriteMode log WriteMode
               $ \hi ho hl -> action (Env hi ho hl dbg)

type App a = ReaderT Environment IO a

app_log :: Int -> Text -> App ()
app_log lvl msg = do
  (Env _ outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStr logh msg
    else return ()

app_logLn :: Int -> Text -> App ()
app_logLn lvl msg = do
  (Env _ outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStrLn logh msg
    else return ()

app_write :: Text -> App ()
app_write msg = do
  (Env _ outh _ _) <- ask
  liftIO $ T.hPutStr outh msg

app_writeLn :: Text -> App ()
app_writeLn msg = do
  (Env _ outh _ _) <- ask
  liftIO $ T.hPutStrLn outh msg

runApp_ :: App a -> Environment -> IO a
runApp_ = runReaderT

runApp :: Configuration -> App a -> IO a
runApp cnf app = withConfiguration (runApp_ app) cnf

readRules :: App Rules
readRules = do
  (Env inh _ _ _) <- ask
  inf <- liftIO $ BL.hGetContents inh
  case A.eitherDecode inf of
    Left str -> do
      app_log debugError "Error during readRules: "
      app_logLn debugError (T.pack str)
      error "abort"
    Right rules -> return rules
