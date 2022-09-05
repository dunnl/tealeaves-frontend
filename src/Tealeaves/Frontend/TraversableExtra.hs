module Tealeaves.Frontend.TraversableExtra
  ( module Tealeaves.Frontend.TraversableExtra
  , module Data.Traversable
  , module Data.Foldable
  )
where

import Control.Monad.State
import Data.Traversable
import Data.Foldable

-- | Monadic @when@
whenM :: (Monad m) => m Bool -> m () -> m ()
whenM mb action = do
  b <- mb
  when b action

-- | Monadic @unless@
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM mb action = do
  b <- mb
  unless b action

runStateOn :: s -> State s a -> (a, s)
runStateOn = flip runState

evalStateOn :: s -> State s a -> a
evalStateOn = flip evalState

execStateOn :: s -> State s a -> s
execStateOn = flip execState

bind :: (Monad m) => (a -> m b) -> m a -> m b
bind f x = x >>= f

-- | 'mmapM' is a monadic traversal (a traversal whose applicative
-- functor is a monad) over a 'Traversable' value that is already
-- under the same monad. Tthe two layers of monadic effects are
-- combined with 'join'. This is a more monadic version of 'mapM' from
-- 'Data.Traversable'. This definition is equivalent to
-- @
-- mmapM f t = do
--   t' <- t
--   traverse f t'
-- @
-- or @join . dist . fmap f@
mmapM :: (Monad m, Traversable t) => (a -> m b) -> m (t a) -> m (t b)
mmapM f = bind (mapM f)

-- | A version of of 'mmapM' that ignores its return value---only the
-- applicative effects of @m@ are evaluated. A more monadic version of
-- 'forM_' from 'Data.Foldable.'
mmapM_ :: (Monad m, Foldable t) => (a -> m b) -> m (t a) -> m ()
mmapM_ f = bind (mapM_ f)

-- | 'mmapM' with its arguments flipped. (c.f. 'forM')
mforM :: (Monad m, Traversable t) => m (t a) -> (a -> m b) -> m (t b)
mforM = flip mmapM

-- | 'mmapM' with its arguments flipped. (c.f. 'forM_')
mforM_ :: (Monad m, Foldable t) => m (t a) -> (a -> m b) -> m ()
mforM_ = flip mmapM_

-- | A version of 'traverse' that iterates over traversable functors
-- that are also monads.  The iterator is generalized to return a
-- monadic value which is 'join'ed into the traversable monad after
-- "bubbling" up (distributing) the applicative functor @f@ over @m@.
-- @catTraverse@ is called @bindt@ in Tealeaves.
-- @catTraverse f@ is equivalent to @fmap join . sequenceA . fmap f@
catTraverse :: (Applicative f, Monad m, Traversable m) => (a -> f (m b)) -> m a -> f (m b)
catTraverse f = fmap join . traverse f
--catTraverse f = fmap join . sequenceA . fmap f

-- | 'catTraverse' with its arguments flipped. (c.f. @for@ and @traverse@)
catFor :: (Applicative f, Monad m, Traversable m) => m a -> (a -> f (m b)) -> f (m b)
catFor = flip catTraverse

-- | A combination of 'mmapM' and 'catTraverse'. @mcatTraverse f@ is equivalent to
-- @join . fmap (fmap join . sequenceA . fmap f)@
mcatTraverse :: (Monad m, Monad t, Traversable t) => (a -> m (t b)) -> m (t a) -> m (t b)
mcatTraverse f = bind (catTraverse f)
-- mcatTraverse f = join . fmap (fmap join . sequence . fmap f)

mcatFor :: (Monad m, Monad t, Traversable t) => m (t a) -> (a -> m (t b)) -> m (t b)
mcatFor = flip mcatTraverse
