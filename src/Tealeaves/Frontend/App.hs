{-# language OverloadedStrings #-}
{-# language TupleSections     #-}

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
import Tealeaves.Frontend.DecoratedMonad
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
newtype AppT e w m a = AppT { runAppT :: e -> m (w, a) }

type App w = AppT Environment w IO

instance (Functor m) => Functor (AppT e w m) where
  fmap f (AppT action) = AppT $ \e -> (\(w, a) -> (w, f a)) <$> action e

instance (Monoid w, Monad m) => Applicative (AppT e w m) where
  pure = \a -> AppT $ \_ -> return (mempty, a)
  (AppT mf) <*> (AppT ma) = AppT $ \e ->
    do (w0, f) <- mf e
       (w1, a) <- ma e
       return (w0 <> w1, f a)

instance (Monoid w, Monad m) => Monad (AppT e w m) where
  return = \a -> AppT $ \_ -> return (mempty, a)
  AppT ma >>= f = AppT $ \e ->
    do (w0, a) <- ma e
       (w1, b) <- runAppT (f a) e
       return (w0 <> w1, b)

instance (Monoid w) => HasLogging (App w) Text where
  log = app_log

instance (Monoid w) => MonadTrans (AppT e w) where
  lift = \ma -> AppT $ \_ -> (mempty, ) <$> ma

instance (Monoid w, MonadIO m) => MonadIO (AppT e w m) where
  liftIO = lift . liftIO

instance (Monoid w, Monad m) => DecoratedMonad w (AppT e w m) where
  bindd f (AppT action_a) =
    AppT $ \e -> do (w0, a) <- action_a e
                    (w1, b) <- runAppT (f w0 a) e
                    return (w0 <> w1, b)
  add_context w1 = AppT $ \_ -> return (w1, ())

--add_context w1 (AppT action) = AppT $ \e w0 -> action e (w0 <> w1)
{-
-- | 'evalRWST' with its arguments flipped
runNestedAppT :: (Monad m, Monoid w, Monoid w') =>
                 s' ->
                 AppT w' s' m b ->
                 AppT w s m (b, w')
runNestedAppT s action = do
  env <- ask
  lift $ runAppT env s action
-}

instance (Monoid w, Monad m) => MonadReader e (AppT e w m) where
  ask = AppT $ \e -> return (mempty, e)
  local modify (AppT action) = AppT $ \e -> action (modify e)

evalAppT :: (Monad m, Monoid w) => e -> AppT e w m a -> m a
evalAppT env action = snd <$> runAppT action env

execAppT :: (Monad m, Monoid w) => e -> AppT e w m a -> m w
execAppT env action = fst <$> runAppT action env

stackOf :: (Monad m, Monoid w, Monoid w') => AppT e w m a -> AppT e w' m w
stackOf action = do
  env <- ask
  lift $ execAppT env action

runApp :: (Monoid w) => Environment -> App w a -> IO a
runApp = evalAppT

runAppWith :: (Monoid w) => Configuration -> App w a -> IO a
runAppWith (Config inf out log dbg) action = do
  withThreeFiles inf ReadMode out WriteMode log WriteMode
    (\hi ho hl -> runApp (Env hi ho hl dbg) action)

-- | Print a log message to the log file.
app_log :: (Monoid w) => Int -> Text -> App w ()
app_log lvl msg = do
  threshold <- asks env_debug
  logh <- asks env_logh
  when (threshold >= lvl) $
    liftIO (T.hPutStr logh msg)

-- | Print a log message to the log file, appending a newline.
app_logLn :: (Monoid w) => Int -> Text -> App w ()
app_logLn lvl msg = app_log lvl (msg <> "\n")

-- | Print a 'Text' value to the output file.
app_write :: (Monoid w) => Text -> App w ()
app_write msg = do
  outh <- asks env_outh
  liftIO (T.hPutStr outh msg)

-- | Print several 'Text' values to the output file.
app_writes :: (Monoid w) => [Text] -> App w ()
app_writes msgs = do
  for_ msgs app_write

-- | Print a 'Text' value to the output file, appending a newline.
app_writeLn :: (Monoid w) => Text -> App w ()
app_writeLn msg = app_write (msg <> "\n")

-- | Print several 'Text' values to the output file, appending a newline to all except the last.
app_writeLns :: [Text] -> App [Text] ()
app_writeLns msgs = sequenceA_ (go msgs)
  where go msgs = case msgs of
          [] -> [return ()]
          (x : []) -> app_write x : []
          (x : xs) -> app_writeLn x : go xs

-- | Attempt to parse the user's 'Rules' from the input file.
readRules :: (Monoid w) => App w Rules
readRules = do
  inh <- asks env_inh
  inf <- liftIO $ BL.hGetContents inh
  case A.eitherDecode inf of
    Left str -> do
      app_log debugError "Error during readRules: "
      app_logLn debugError (T.pack str)
      error "abort"
    Right rules -> return rules

push :: a -> App [a] ()
push a = do
  add_context [a]
