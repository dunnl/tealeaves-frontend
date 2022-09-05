{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

module Tealeaves.Frontend.DecoratedMonad where

class (Monoid w, Monad m) => DecoratedMonad w m | m -> w where
  bindd :: (w -> a -> m b) -> m a -> m b
  add_context :: w -> m a -> m a

instance (Monoid w) => DecoratedMonad w ((,) w) where
  bindd action (w0, a) =
    let (w1, b) = action w0 a
    in (w0 <> w1, b)
  add_context w1 (w0, a) = (w0 <> w1, a)

read_context :: (DecoratedMonad w m) => m a -> m (w, a)
read_context = bindd (\w a -> return (w, a))

write_context :: (DecoratedMonad w m) => w -> m ()
write_context = \w -> add_context w $ return ()


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
