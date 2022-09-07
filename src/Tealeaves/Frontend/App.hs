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
newtype AppT e s w m a = AppT { runAppT :: e -> s -> w -> m (s, w, a) }

getContext :: (Monoid w, Monad m) => AppT e s w m w
getContext = AppT $ \_e s w -> return (s, mempty, w)

getsContext :: (Monoid w, Monad m) => (w -> b) -> AppT e s w m b
getsContext = \f -> AppT $ \_e s w -> return (s, mempty, f w)

type App w = AppT Environment () w IO

instance (Functor m) => Functor (AppT e s w m) where
  fmap f (AppT action) = AppT $ \e s w -> (\(s, w, a) -> (s, w, f a)) <$> action e s w

instance (Monoid w, Monad m) => Applicative (AppT e s w m) where
  pure = \a -> AppT $ \_e s _w -> return (s, mempty, a)
  (AppT mf) <*> (AppT ma) = AppT $ \e s0 w0 ->
    do (s1, w1, f) <- mf e s0 w0
       (s2, w2, a) <- ma e s1 (w0 <> w1)
       return (s2, w1 <> w2, f a)

instance (Monoid w, Monad m) => Monad (AppT e s w m) where
  return = \a -> AppT $ \_e s _w -> return (s, mempty, a)
  AppT ma >>= f = AppT $ \e s0 w0 ->
    do (s1, w1, a) <- ma e s0 w0
       (s2, w2, b) <- runAppT (f a) e s1 (w0 <> w1)
       return (s2, w1 <> w2, b)

instance (Monoid w) => HasLogging (App w) Text where
  log = app_log

instance (Monoid w) => MonadTrans (AppT e s w) where
  lift = \ma -> AppT $ \_e s _w -> (s, mempty, ) <$> ma

instance (Monoid w, MonadIO m) => MonadIO (AppT e s w m) where
  liftIO = lift . liftIO

instance (Monoid w, Monad m) => DecoratedMonad w (AppT e s w m) where
  bindd f (AppT action_a) =
    AppT $ \e s0 w0 -> do (s1, w1, a) <- action_a e s0 w0
                          (s2, w2, b) <- runAppT (f (w0 <> w1) a) e s1 (w0 <> w1)
                          return (s2, w1 <> w2, b)
  add_context w1 = AppT $ \_e s _w0 -> return (s, w1, ())

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

instance (Monoid w, Monad m) => MonadReader e (AppT e s w m) where
  ask = AppT $ \e s _w -> return (s, mempty, e)
  local modify (AppT action) = AppT $ \e -> action (modify e)

instance (Monoid w, Monad m) => MonadState s (AppT e s w m) where
  get = AppT $ \_e s _w -> return (s, mempty, s)
  put = \s -> AppT $ \_e _s _w -> return (s, mempty, ())

evalAppT :: (Monad m, Monoid w) => e -> s -> AppT e s w m a -> m a
evalAppT env st action = (\(_,_,a)->a) <$> runAppT action env st mempty

execAppT :: (Monad m, Monoid w) => e -> s -> AppT e s w m a -> m s
execAppT env st action = (\(s,_,_)->s) <$> runAppT action env st mempty

flushAppT :: (Monad m, Monoid w) => e -> s -> AppT e s w m a -> m w
flushAppT env st action = (\(_,w,_)->w) <$> runAppT action env st mempty

stackOf :: (Monad m, Monoid w, Monoid w') => AppT e s w m a -> AppT e s w' m w
stackOf action = do
  env <- ask
  state <- get
  lift $ flushAppT env state action

stateOf :: (Monad m, Monoid w, Monoid w') => s -> AppT e s w m a -> AppT e s w' m s
stateOf st0 action = do
  env <- ask
  lift $ execAppT env st0 action

runApp :: (Monoid w) => Environment -> App w a -> IO a
runApp = \env -> evalAppT env ()

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

push :: (Monad m) => a -> AppT e s [a] m ()
push a = do
  add_context [a]
