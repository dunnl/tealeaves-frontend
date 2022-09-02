{-|
Module      : StateWithFuture
Description : Implementation of the 'StateWithFuture' Monad
Copyright   : (c) Lawrence Dunn, 2022

This module implements stateful computations which, in addition to
reading and writing some state, may also depend on the /final/ value
of the state. Theoretically this is a @Reader s@ monad wrapped by a
@State s@ monad---a stateful computation of an operation that is
blocked on reading the final state. Computing the operation's return
value from an initial state is possible (total) provided the
sub-operation which modifies the state does not itself depend on the
final state value---in other words, if this operation may be
factorized
-}

{-# language FlexibleInstances      #-}
{-# language MultiParamTypeClasses  #-}
{-# language FunctionalDependencies #-}

module StateWithFuture where

import Control.Monad.Reader.Class
import Control.Monad.State.Lazy
import Data.Traversable
import Data.Functor.Compose

-- | We wrap this in a @newtype@ to avoid conflicting instances of

newtype StateWithFuture s a =
  StateWithFuture { doStateWithFuture :: s -> (s, s -> a) }

-- | Coerce a self-dependent stateful action into it's @newtype@.
stateWithFuture :: (s -> (s, s -> a)) -> StateWithFuture s a
stateWithFuture = StateWithFuture

-- | Convert an self-dependent stateful action to a normal
-- (non-self-dependent) one
runStateWithFuture :: StateWithFuture s a ->
                      State s a
runStateWithFuture (StateWithFuture action) =
  state $ \snow -> let (sout, blocked) = action snow
                   in (blocked sout, sout)

instance Functor (StateWithFuture s) where
  fmap f (StateWithFuture action) = StateWithFuture $
    \snow -> let (sout, blocked) = action snow
             in (sout, f <$> blocked)

instance Applicative (StateWithFuture s) where
  pure a = StateWithFuture $ \snow -> (snow, const a)
  (StateWithFuture f) <*> (StateWithFuture a) = StateWithFuture $
    \snow -> let (sout0, blockedF) = f snow
                 (sout1, blockedA) = a sout0
             in  (sout1, blockedF <*> blockedA)

instance Monad (StateWithFuture s) where
  return a = StateWithFuture $ \snow -> (snow, const a)
  (>>=) (StateWithFuture actA) f = StateWithFuture $
    \snow -> let (sAout, blockedA) = actA snow
                 a = blockedA sAout -- feed a's final state to its blocked computation
                 StateWithFuture actB = f a
             in actB sAout

instance MonadReader s (StateWithFuture s) where
  ask = StateWithFuture $ \snow -> (snow, \_ -> snow)
  local modify (StateWithFuture action) = StateWithFuture (action . modify)

instance MonadState s (StateWithFuture s) where
  get = ask
  put = \put -> StateWithFuture (\snow -> (put, const ()))

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
instance MonadReaderFinal s (StateWithFuture s) where
  askFinal = StateWithFuture $ \snow -> (snow, \sfinal -> sfinal)
  localFinal modify = \(StateWithFuture action) ->
    StateWithFuture $ \snow -> let (sout, blockedA) = action snow
                               in (sout, blockedA . modify)

-- More convenient 'State' evaluation functions
runStateOn :: s -> State s a -> (a, s)
runStateOn = flip runState

evalStateOn :: s -> State s a -> a
evalStateOn = flip evalState

execStateOn :: s -> State s a -> s
execStateOn = flip execState

traverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> State s (t b)
traverseState f = runStateWithFuture . traverse f

runTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> (t b, s)
runTraverseState f = runState . traverseState f

evalTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> t b
evalTraverseState f = evalState . traverseState f

execTraverseState :: (Traversable t) => (a -> StateWithFuture s b) -> t a -> s -> s
execTraverseState f = execState . traverseState f

forState :: (Traversable t) => t a -> (a -> StateWithFuture s b) -> State s (t b)
forState t = runStateWithFuture . for t

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
