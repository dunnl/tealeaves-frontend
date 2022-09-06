{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Tealeaves.Frontend.DecoratedMonad where

import Control.Monad.Writer.Class

class (Monoid w, Monad m) => DecoratedMonad w m | m -> w where
  bindd :: (w -> a -> m b) -> m a -> m b
  add_context :: w -> m ()

{-
instance (Monoid w) => DecoratedMonad w ((,) w) where
  bindd action (w0, a) =
    let (w1, b) = action w0 a
    in (w0 <> w1, b)
  add_context w1 = tell w1
-}

read_context :: (DecoratedMonad w m) => m a -> m (w, a)
read_context = bindd (\w a -> return (w, a))

read_context' :: (DecoratedMonad w m) => m a -> m w
read_context' = bindd (\w a -> return w)

--write_context :: (DecoratedMonad w m) => w -> m ()
--write_context = \w -> add_context w $ return ()

{-
This doesn't work. The "pass" operation doesn't really make sense.

instance (DecoratedMonad w m) => MonadWriter w m where
  writer = \(w, a) -> add_context w $ return a
  tell w = write_context
  listen = read_context
  pass = ...
-}


{-
{-# language UndecidableInstances #-}
instance (Monad m, Monoid w, MonadWriter w m) => DecoratedMonad w m where
  bindd action ma =
    listen ma >>= (\(a, w) -> action w a)
  add_context w ma = tell w >> ma
-}

{-
class (Monoid w, Monad m) => MonadDecorated w m | m -> w where
  decorated :: (w -> (a, w)) -> m a

read_context :: (MonadDecorated w m) => m w
read_context = decorated $ \w -> (w, w)

add_context :: (MonadDecorated w m) => w -> m ()
add_context \w1 -> decorated $ \w0 -> w0 <> w1

adding_context :: (MonadDecorated w m) => w -> m a -> m a
adding_context \w1 action -> decorated $ \w0 ->
  case action w0 <> w1
-}
