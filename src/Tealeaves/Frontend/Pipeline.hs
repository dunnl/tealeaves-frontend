{-# language FlexibleInstances     #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language TupleSections     #-}
{-# language RankNTypes     #-}

{-|
Module      : Pipeline.hs
Description : Types and operations for effectful streams and pipes
Copyright   : (c) Lawrence Dunn, 2022
-}

module Tealeaves.Frontend.Pipeline
  ( module Tealeaves.Frontend.Pipeline
  , module Pipes
  ) where

import Control.Monad.State.Strict
import           Pipes
import qualified Pipes.Core as PC
import qualified Pipes.Lift as PL
import qualified Pipes.Prelude as P

import Tealeaves.Frontend.App

---- Streams and pipes
-- | @StreamS s a@ is is a stateful generator of @a@ values,
-- maintaining state @s@, wrapping an inner 'App' monad.
type StreamS s a = Producer a (StateT s (App ())) ()

-- | 'Stream' is isomorphic to @ListT (App ()) a@, but it has the
-- advantage we can compose it with 'Pipe's, rather than just using
-- 'P.for'.
type Stream a = Producer a (App ()) ()

-- | Polymorphic 'StreamS'
type StreamS' s a = Producer' a (StateT s (App ())) ()

-- | Polymorphic 'Stream'
type Stream' a = Producer' a (App ()) ()

-- | @PipelineS s a b@ is a stateful 'Pipe' action that can 'await's @a@
-- values, 'yield' @b@ values, maintain a state @s@, wrapping an inner
-- 'App' monad.
type PipelineS s a b = Pipe a b (StateT s (App ())) ()

-- | @PipelineS s a b@ is a pipe that can 'await's @a@ values and
-- 'yield' @b@ values, wrapping an inner 'App' monad.
type Pipeline a b = Pipe a b (App ()) ()

-- Polymorphic 'Pipe' doesn't actually exist
--type Pipe' a b = forall a' b'. Proxy a' a b' b

-- | Lift 'HasLogging' instances from the inner monad
instance (Monad m, HasLogging m msg) => HasLogging (Proxy a' a b' b m) msg where
  log = \i msg -> lift (Tealeaves.Frontend.App.log i msg)

-- | 'hoist' a non-stateful 'Proxy' action to a stateful one. This can
-- be used to compose stateful pipes with non-stateful ones.
hoistState :: (Monad m) => Proxy a' a b' b m r -> Proxy a' a b' b (StateT s m) r
hoistState = hoist lift

-- | Convert a stateful 'StreamS' into a list computed in the 'App'
-- monad, also returning the final state.
runStreamS :: s -> StreamS s a -> App () ([a], s)
runStreamS = \st0 -> flip runStateT st0 . P.toListM

-- | Convert a stateful 'StreamS' into a list computed in the 'App'
-- monad.
evalStreamS :: s -> StreamS s a -> App () [a]
evalStreamS = \st0 -> flip evalStateT st0 . P.toListM

-- | Convert a 'Stream' into a list computed in the 'App' monad.
runStream :: Stream a -> App () [a]
runStream = P.toListM

-- | Echo each input after annotating with a calculated value
mapAnnotate :: (Functor m) => (a -> b) -> Pipe a (a, b) m ()
mapAnnotate fn = P.map $ \a -> (a, fn a)

-- | Echo each input after annotating with a value calculated in the
-- inner monad
mapAnnotateM :: (Monad m) => (a -> m b) -> Pipe a (a, b) m ()
mapAnnotateM fn = P.mapM $ \a -> (a,) <$> fn a

-- | Replace every @yield a@ with @yield (fn a)@
pipeMap :: (a -> b) -> Pipeline x a -> Pipeline x b
pipeMap fn pipeline =
  Pipes.for pipeline (\a -> yield (fn a))

-- | Await an @a@, then run the pipeline computed from @a@, then
-- recurse when that pipeline terminates
forInput :: (a -> Pipeline a b) -> Pipeline a b
forInput = Pipes.for cat

-- | Await an @a@, then run the pipeline computed from @a@, replacing
-- every 'yield b' with 'yield (a, b)'. Then recurse when that
-- pipeline terminates.
forInputAnnotate :: (a -> Pipeline a b) -> Pipeline a (a, b)
forInputAnnotate fn = Pipes.for cat (\a -> pipeMap (a,) . fn $ a)

-- | Echo each input. Between successive inputs, echo @b@.
pipeIntersperse :: b -> Pipeline b b
pipeIntersperse bnew = do
  await >>= yield >> go
  where
    go = do
      b <- await
      yield bnew
      yield b
      go
