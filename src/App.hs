{-# language OverloadedStrings #-}

{-# language FlexibleInstances      #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeSynonymInstances   #-}

module App where

import Control.Monad.Reader
import System.IO
import Control.Monad.State
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

import StateWithFuture
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
  liftIO $ withFile fp md (\h -> liftIO $ withFile fp' md' (action h))

withThreeFiles ::
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  FilePath -> IOMode ->
  (Handle -> Handle -> Handle -> IO r) -> IO r
withThreeFiles fp0 md0 fp md fp' md' action =
  liftIO $ withFile fp0 md0 (\h1 -> do
                       withTwoFiles fp md fp' md' (action h1))

type AppT s m = ReaderT Environment (StateWithFutureT s m)

type App s = AppT s IO

{-
instance (Monad m) => Applicative (AppT s m) where
  pure a = ReaderT $ \env ->
    StateWithFutureT $ \snow ->
    return (const a, snow)
  appF <*> appA = ReaderT $ \env ->
    (runReaderT appF env) <*> (runReaderT appA env)
    {-
    StateWithFutureT $ \snow ->
    \snow -> do (blockedF, sout0) <- f snow
                (blockedA, sout1) <- a sout0
                return (blockedF <*> blockedA, sout1)
     -}
-}

runAppT :: (Monad m) => Environment -> s -> AppT s m a -> m a
runAppT env st action = evalStateWithFutureT (runReaderT action env) st

tieAppWithFutureT :: (Monad m) => AppT s m b -> ReaderT Environment (StateT s m) b
tieAppWithFutureT appT = ReaderT $ \e -> -- over Reader
                    tieStateWithFutureT (runReaderT appT e)

forAppTSt :: (Monad m, Traversable t) => Environment -> s -> t a -> (a -> AppT s m b) -> m (t b, s)
forAppTSt env s t f = runStateWithFutureT (runReaderT (traverse f t) env) s

forAppT :: (Monad m, Traversable t) => Environment -> s -> t a -> (a -> AppT s m b) -> m (t b)
forAppT env s t f = evalStateWithFutureT (runReaderT (traverse f t) env) s

forAppSt :: (Traversable t) => Environment -> s -> t a -> (a -> App s b) -> IO (t b, s)
forAppSt env s t f = runStateWithFutureT (runReaderT (traverse f t) env) s

forApp :: (Traversable t) => Environment -> s -> t a -> (a -> App s b) -> IO (t b)
forApp env s t f = evalStateWithFutureT (runReaderT (traverse f t) env) s

runApp :: Environment -> s -> App s a -> IO a
runApp = runAppT

runAppWith :: Configuration -> s -> App s a -> IO a
runAppWith (Config inf out log dbg) st action = do
  withThreeFiles inf ReadMode out WriteMode log WriteMode
    (\hi ho hl -> runApp (Env hi ho hl dbg) st action)

app_log :: Int -> Text -> App s ()
app_log lvl msg = do
  (Env _ outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStr logh msg
    else return ()

app_logLn :: Int -> Text -> App s ()
app_logLn lvl msg = do
  (Env _ outh logh debug) <- ask
  if debug >= lvl
    then liftIO $ T.hPutStrLn logh msg
    else return ()

app_write :: Text -> App s ()
app_write msg = do
  (Env _ outh _ _) <- ask
  liftIO $ T.hPutStr outh msg

app_writes :: [Text] -> App s ()
app_writes msgs = do
  (Env _ outh _ _) <- ask
  liftIO $ for_ msgs
    (\msg -> T.hPutStr outh msg)

app_writeLn :: Text -> App s ()
app_writeLn msg = do
  (Env _ outh _ _) <- ask
  liftIO $ T.hPutStrLn outh msg

app_writeLns :: [Text] -> App s ()
app_writeLns msgs = do
  (Env _ outh _ _) <- ask
  liftIO $ for_ msgs
    (\msg -> T.hPutStrLn outh msg)

readRules :: App s Rules
readRules = do
  (Env inh _ _ _) <- ask
  inf <- liftIO $ BL.hGetContents inh
  case A.eitherDecode inf of
    Left str -> do
      app_log debugError "Error during readRules: "
      app_logLn debugError (T.pack str)
      error "abort"
    Right rules -> return rules
