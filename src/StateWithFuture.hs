{-|
Module      : StateWithFuture
Description : Implementation of the 'StateWithFuture' Monad
Copyright   : (c) Lawrence Dunn, 2022

This module implements stateful computations which, in addition to
reading and writing some state, may also depend on the /final/ value
of the state. Theoretically this is a @Reader s@ monad wrapped by a
@State s@ monad---a stateful computation whose return value is blocked
on reading the final state of the operation. Computing the operation's return value
from an initial state is possible (total) provided the sub-operation
which modifies the state does not itself depend on the final state
value---in other words, if this operation may be factorized
-}

{-# language FlexibleInstances      #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}

module StateWithFuture where

import Data.Functor.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Lazy
import Data.Traversable
import Data.Functor.Compose

-- | This is equivalent to 'StateT s m (Reader s a)'. However we give
-- this a newtype which helps avoid confusion, as the monad 'App' is
-- wraps @StateWithFuture@ with another @Reader@.
newtype StateWithFutureT s m a =
  StateWithFutureT { unStateWithFutureT :: s -> m (s -> a, s) }

-- | Convert an self-dependent stateful action to a normal
-- (non-self-dependent) one
tieStateWithFutureT :: Monad m =>
                       StateWithFutureT s m a ->
                       StateT s m a
tieStateWithFutureT (StateWithFutureT action) =
  StateT $ \snow -> do (blocked, sout) <- action snow
                       return $ (blocked sout, sout)

instance Functor m => Functor (StateWithFutureT s m) where
  fmap f (StateWithFutureT action) = StateWithFutureT $
    \snow -> let op (blocked, sout) = (f <$> blocked, sout)
             in fmap op (action snow)

instance (Monad m) => Applicative (StateWithFutureT s m) where
  pure a = StateWithFutureT $ \snow -> return (const a, snow)
  (StateWithFutureT f) <*> (StateWithFutureT a) = StateWithFutureT $
    \snow -> do (blockedF, sout0) <- f snow
                (blockedA, sout1) <- a sout0
                return (blockedF <*> blockedA, sout1)

instance (Monad m) => Monad (StateWithFutureT s m) where
  return a = StateWithFutureT $ \snow -> return (const a, snow)
  (>>=) (StateWithFutureT actA) f = StateWithFutureT $
    \snow -> do (blockedA, sAout) <- actA snow -- m (s, s -> a)
                let a = blockedA sAout -- feed a's final state to its blocked computation
                    StateWithFutureT actB = f a -- f : a -> StateWithFutureT s m b
                actB sAout

instance (Monad m) => MonadReader s (StateWithFutureT s m) where
  ask = StateWithFutureT $ \snow -> return (const snow, snow)
  local modify (StateWithFutureT action) = StateWithFutureT (action . modify)

instance (Monad m) => MonadState s (StateWithFutureT s m) where
  get = ask
  put = \put -> StateWithFutureT (\snow -> return (const (), put))

instance MonadTrans (StateWithFutureT s) where
  lift = \ma -> StateWithFutureT $ \s ->
    do a <- ma
       return (const a, s)

instance (MonadIO m) => MonadIO (StateWithFutureT s m) where
  liftIO = lift . liftIO

-- | 'MonadReaderFinal' is isomorphic to 'MonadReader', effectively
-- allowing for multiple instances of this class in a single monad.
-- This is just like 'MonadReader', except instead of reading @r@ as
-- an "environment" known from the beginning of the computation, we
-- interpret this as some unknown "final" value we are dependent
-- on.
class Monad m => MonadReaderFinal r m | m -> r where
    {-# MINIMAL (askFinal | readerFinal), localFinal #-}
    -- | Retrieves the monad "final" value.
    askFinal   :: m r
    askFinal = readerFinal id

    -- | Executes a computation with a modified final value.
    localFinal :: (r -> r) -- ^ The function to modify the final value.
               -> m a      -- ^ The action to run with the modified value.
               -> m a

    -- | Retrieves a function of the final value.
    readerFinal :: (r -> a) -- ^ The selector function to apply to the final value.
              -> m a
    readerFinal f = do
      r <- askFinal
      return (f r)

-- | Computations in 'StateWithFuture s' can read the final value of
-- the state when computing their return values.
instance (Monad m) => MonadReaderFinal s (StateWithFutureT s m) where
  askFinal = StateWithFutureT $ \snow -> return (\sfinal -> sfinal, snow)
  localFinal modify = \(StateWithFutureT action) ->
    StateWithFutureT $ \snow -> do (blockedA, sout) <- action snow -- :: m (s, s -> a))
                                   return (blockedA . modify, sout) -- :: (s, s -> a))


runStateWithFutureT :: (Monad m) => StateWithFutureT s m b -> s -> m (b, s)
runStateWithFutureT = runStateT . tieStateWithFutureT

evalStateWithFutureT :: (Monad m) => StateWithFutureT s m b -> s -> m b
evalStateWithFutureT = evalStateT . tieStateWithFutureT

execStateWithFutureT :: (Monad m) => StateWithFutureT s m b -> s -> m s
execStateWithFutureT = execStateT . tieStateWithFutureT


type StateWithFuture s a = StateWithFutureT s Identity a

-- | Coerce a self-dependent stateful action into it's @newtype@.
stateWithFuture :: (s -> (s -> a, s)) -> StateWithFuture s a
stateWithFuture action = StateWithFutureT (return . action)


-- | Convert an self-dependent stateful action to a normal
-- (non-self-dependent) one
tieStateWithFuture :: StateWithFuture s a -> State s a
tieStateWithFuture (StateWithFutureT action) =
  state $ \snow -> let (blocked, sout) = runIdentity $ action snow
                   in (blocked sout, sout)

runStateOn :: s -> State s a -> (a, s)
runStateOn = flip runState

evalStateOn :: s -> State s a -> a
evalStateOn = flip evalState

execStateOn :: s -> State s a -> s
execStateOn = flip execState

runStateWithFuture :: StateWithFuture s b -> s -> (b, s)
runStateWithFuture = runState . tieStateWithFuture

evalStateWithFuture :: StateWithFuture s b -> s -> b
evalStateWithFuture = evalState . tieStateWithFuture

execStateWithFuture :: StateWithFuture s b -> s -> s
execStateWithFuture = execState . tieStateWithFuture

traverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> State s (t b)
traverseState f = tieStateWithFuture . traverse f

runTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> (t b, s)
runTraverseState f = runState . traverseState f

evalTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> t b
evalTraverseState f = evalState . traverseState f

execTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> s
execTraverseState f = execState . traverseState f

forState :: (Traversable t) => t a -> (a -> StateWithFuture s b) -> State s (t b)
forState t = tieStateWithFuture . for t

forWithFuture :: (Traversable t) => s -> t a -> (a -> StateWithFuture s b) -> (t b, s)
forWithFuture s t action = runState (forState t action) s

convert :: (Monoid w) =>
        (w -> a -> (w, b))
        -> a -> StateWithFuture w b
convert action = \a -> do
  wfinal <- askFinal
  let (wadd, b) = action wfinal a
  modify (<> wadd)
  return b

-- @w@ the final value of type 'w'
-- @a@ the value under focus by the traversal
-- @(w, b)@ a value to replace 'a' with and the weight applied towards the final value for 'w'
mapWithFinalMonoid :: (Monoid w, Traversable t) => (w -> a -> (w, b)) -> t a -> t b
mapWithFinalMonoid action ta = evalTraverseState (convert action) ta mempty
