{-# language OverloadedStrings #-}
{-# language TupleSections     #-}

{-# language FlexibleInstances      #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}
{-# language TypeSynonymInstances   #-}

module Tealeaves.Frontend.Buffer where

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

{-
newtype BufferT m w = BufferT { runBufferT :: w -> m w }

type Buffer m b = BufferT m [b]

instance (Monad m) => Functor (BufferT m) where
  fmap f (BufferT action) = BufferT $ \w ->

instance (Monad m, Monoid w) => Semigroup (BufferT m w) where
  BufferT b1 <> BufferT b2 =
    BufferT $ \buff0 -> do
    buff1 <- b1 buff0
    b2 (buff0 <> buff1)

instance (Monad m, Monoid w) => Monoid (BufferT m w) where
  mempty = BufferT $ \_ -> return mempty

-- | This is a low-level operation designed to lift monad actions into
-- a buffer action. This is mostly intended to allow us to do logging
-- in the @m@ monad while constructing some buffer.
embedInBuffer :: (Monad m, Monoid w) => m a -> BufferT m w
embedInBuffer action =
  BufferT $ \_ -> do
    action
    return mempty

-- | This is comparable to the operation 'toLazyByteString' in
-- 'Data.Bytestring.Builder'.
evalBuffer :: (Monad m, Monoid w) => BufferT m w -> m w
evalBuffer (BufferT action) = action mempty

push :: (Monad m) => b -> Buffer m b
push b = BufferT $ \_ -> return [b]

pushAll :: (Monad m) => [b] -> Buffer m b
pushAll b = BufferT $ \_ -> return b

forB :: (Monad m) =>
        Buffer m b ->
        (b -> Buffer m c) ->
        Buffer m c

forB (Buffer buffer1) iterate_body =
  BufferT $ \c0 -> do
    bs <- buffer1 mempty :: m [b]
    foldMap iterate_body bs
    -}

{-
{-
instance (Functor m) => Functor (AppT e s m) where
  fmap f (AppT action) = AppT $ \e s -> (\(s, a) -> (s, f a)) <$> action e s


instance (Monad m) => Applicative (AppT e s m) where
  pure = \a -> AppT $ \_e s -> return (s, a)
  (AppT mf) <*> (AppT ma) = AppT $ \e s0 ->
    do (s1, f) <- mf e s0
       (s2, a) <- ma e s1
       return (s2, f a)

instance (Monad m) => Monad (AppT e s m) where
  return = \a -> AppT $ \_e s -> return (s, a)
  AppT ma >>= f = AppT $ \e s0 ->
    do (s1, a) <- ma e s0
       (s2, b) <- runAppT (f a) e s1
       return (s2, b)
-}

instance HasLogging (App s) Text where
  log = app_log

instance MonadTrans (AppT e s) where
  lift = \ma -> AppT $ \_e s -> (s, ) <$> ma

instance (MonadIO m) => MonadIO (AppT e s m) where
  liftIO = lift . liftIO

instance (Monad m) => MonadReader e (AppT e s m) where
  ask = AppT $ \e s -> return (s, e)
  local modify (AppT action) = AppT $ \e -> action (modify e)

instance (Monad m) => MonadState s (AppT e s m) where
  get = AppT $ \_e s -> return (s, s)
  put = \s -> AppT $ \_e _s -> return (s, ())

-- | Run an application on an environment and initial state, returning
-- the final computed value.
evalAppT :: (Monad m) => e -> s -> AppT e s m a -> m a
evalAppT env st action = (\(_,a)->a) <$> runAppT action env st

-- | Run an application on an environment and initial state, returning
-- the final state
execAppT :: (Monad m) => e -> s -> AppT e s m a -> m s
execAppT env st action = (\(s,_)->s) <$> runAppT action env st

evalSubAppT :: (Monad m) => s -> AppT e s m a -> AppT e s' m a
evalSubAppT st action = do
  env <- ask
  lift $ evalAppT env st action

execSubAppT :: (Monad m) => s -> AppT e s m a -> AppT e s' m s
execSubAppT st action = do
  env <- ask
  lift $ execAppT env st action

-- | Run a sub-application which shares the same environment and
-- begins in the current state and an empty log of type @w@.  At the
-- end of the computation, the final state and value are discarded and
-- the final log is returned.
logOf :: (Monad m) => AppT e [x] m a -> AppT e s m [x]
logOf action = do
  reverse <$> execSubAppT mempty action

-- | Run a sub-application which shares the same environment and
-- begins in the current state and a fresh buffer of type @w@.  At the
-- end of the computation, the final buffer and value are discarded
-- and the final state is returned.
stateOf :: (Monad m) => s -> AppT e s m a -> AppT e s' m s
stateOf st0 action = do
  env <- ask
  lift $ execAppT env st0 action

runApp :: Environment -> s -> App s a -> IO a
runApp = \env st0 -> evalAppT env st0

runAppWith :: Configuration -> App () a -> IO a
runAppWith (Config inf out log dbg) action = do
  withThreeFiles inf ReadMode out WriteMode log WriteMode
    (\hi ho hl -> runApp (Env hi ho hl dbg) () action)

-- | Print a log message to the log file.
app_log :: Int -> Text -> App s ()
app_log lvl msg = do
  threshold <- asks env_debug
  logh <- asks env_logh
  when (threshold >= lvl) $
    liftIO (T.hPutStr logh msg)

-- | Print a log message to the log file, appending a newline.
app_logLn :: Int -> Text -> App s ()
app_logLn lvl msg = app_log lvl (msg <> "\n")

-- | Print a 'Text' value to the output file.
app_write :: Text -> App s ()
app_write msg = do
  outh <- asks env_outh
  liftIO (T.hPutStr outh msg)

-- | Print several 'Text' values to the output file.
app_writes :: [Text] -> App s ()
app_writes msgs = do
  for_ msgs app_write

-- | Print a 'Text' value to the output file, appending a newline.
app_writeLn :: Text -> App s ()
app_writeLn msg = app_write (msg <> "\n")

-- | Print several 'Text' values to the output file, appending a newline to each
app_writeLns :: [Text] -> App s ()
app_writeLns msgs = for_ msgs app_writeLn

-- | Attempt to parse the user's 'Rules' from the input file.
readRules :: App s Rules
readRules = do
  inh <- asks env_inh
  inf <- liftIO $ BL.hGetContents inh
  case A.eitherDecode inf of
    Left str -> do
      app_log debugError "Error during readRules: "
      app_logLn debugError (T.pack str)
      error "abort"
    Right rules -> do
      app_logLn debugInfo $ "Dumping rules: " <> T.pack (show rules)
      return rules

push :: (Monad m) => a -> AppT e [a] m ()
push a = do
  modify (a:)
-}
