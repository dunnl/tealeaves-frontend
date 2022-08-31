module Driver where

import Control.Monad.Reader
import System.IO
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

data Configuration = Config
  { conf_outfile :: String
  , conf_logfile :: String
  , conf_debug   :: Int }

data Environment = Env
  { env_outh :: Handle
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
  <$> strOption ( long "out"
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

withConfiguration :: (Environment -> IO a) -> (Configuration -> IO a)
withConfiguration action (Config out log dbg) =
  withTwoFiles out WriteMode log WriteMode
               $ \ho hl -> action (Env ho hl dbg)

log :: Int -> Text -> App ()
log lvl msg = do
  (Env outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStr logh msg
    else return ()
  
writeLn :: Text -> App ()
writeLn msg = do
  (Env outh _ _) <- ask
  liftIO $ T.hPutStr outh msg
    
type App a = ReaderT Environment IO a

runApp_ :: App a -> Environment -> IO a
runApp_ = runReaderT

runApp :: Configuration -> App a -> IO a
runApp cnf app = withConfiguration (runApp_ app) cnf
