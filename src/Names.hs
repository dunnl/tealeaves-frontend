{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Names where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Lazy
import Data.Traversable

import Data.Functor.Compose

-- More convenient 'State' evaluation functions
evalStateOn = flip evalState

runStateOn = flip runState

execStateOn = flip execState

-- | A map type used to count occurrences of 'Text' values
type Names = Map Text Int

-- | 'getFreshSuffix' @t@ is the action which returns the current value
-- for @t@, defaulting to @0@, and then increments this value by one.
--
-- >>> runState (getFreshSuffix "term") M.empty
-- (0,fromList [("term",1)])
-- >>> runState (getFreshSuffix "term") (fromList [("term",1)])
-- (1,fromList [("term",2)])
getFreshSuffix :: Text -> State Names Int
getFreshSuffix name = do
  max <- gets (M.findWithDefault 0 name)
  modify' (M.insertWith (+) name 1)
  return max

-- | 'addFreshSuffix' @t@ obtains a fresh suffix @s@ and returns
-- their concatenation @t s@
addFreshSuffix :: Text -> State Names Text
addFreshSuffix s = do
  suf <- getFreshSuffix s
  return (s <> (T.pack $ show suf))

-- | 'addSuffices' iterates over a list of text values and appends a
-- fresh suffix to each one, keeping track of previous seen names.
addSuffices :: [Text] -> [Text]
addSuffices =
  evalStateOn M.empty .
  traverse addFreshSuffix

traverseWithFinalState :: (Traversable t) => (Names -> a -> State Names b) -> t a -> State Names (t b)
traverseWithFinalState action t =
  state $ (\st -> let (final_output, final_state) = runStateOn st (traverse (action final_state) t) in
               (final_output, st))

traverseWithFinalMonoid :: (Monoid w, Applicative f, Traversable t) => (w -> a -> (w, f b)) -> t a -> f (t b)
traverseWithFinalMonoid action t =
  let Compose (final_w, ftb) = traverse (Compose . action final_w) t
  in ftb

mapWithFinalMonoid :: (Monoid w, Traversable t) => (w -> a -> (w, b)) -> t a -> t b
mapWithFinalMonoid action t =
 let (final_w, tb) = traverse (action final_w) t
 in tb

class WrappedMonoid w where
  wmempty :: w
  wmappend :: w -> w -> w

newtype WrapManual w a = Wrapped { unwrap :: (w , a) }

instance (Functor (WrapManual w)) where
  fmap f (Wrapped (w, a)) = Wrapped (w, f a)

instance (WrappedMonoid w) => Applicative (WrapManual w) where
  pure a = Wrapped (wmempty, a)
  Wrapped (w0, f) <*> Wrapped (w1, b) = Wrapped (wmappend w0 w1, f b)

-- Compose (Wrapped w) (f)
traverseWithFinalMonoidManual :: (WrappedMonoid w, Applicative f, Traversable t) => (w -> a -> (w, f b)) -> t a -> f (t b)
traverseWithFinalMonoidManual action t =
  let Compose (Wrapped (final_w, ftb)) = traverse (Compose . Wrapped . action final_w) t
  in ftb

mapWithFinalMonoidManual :: (WrappedMonoid w, Traversable t) => (w -> a -> (w, b)) -> t a -> t b
mapWithFinalMonoidManual action t =
 let (final_w, tb) = unwrap $ traverse (Wrapped . action final_w) t
 in tb

-- | 'addFreshSuffix2' @map t@ iterates over a list of text values and appends a
-- fresh suffix to each one, keeping track of previous seen names.
addFreshSuffix2 :: Names -> -- ^ The /final/ count of each occurrence after performing an action
                   Text -> -- ^ Any symbol
                   State Names Text -- ^ Returns a freshly suffixed @t@ or just @t@ if @t@ occurs only once
addFreshSuffix2 final s = do
  suf <- getFreshSuffix s
  return $ if M.findWithDefault 0 s final <= 1
           then s else (s <> (T.pack $ show suf))

-- | 'addSuffices2' iterates over a list of text values and appends a
-- fresh suffix to each one, keeping track of previous seen names. However,
-- for symbols which only occur once, a suffix of value 0 is not appended.
addSuffices2 :: [Text] -> [Text]
addSuffices2 input =
  evalStateOn M.empty (traverseWithFinalState addFreshSuffix2 input)

instance WrappedMonoid Names where
  wmempty = M.empty
  wmappend = M.unionWith (+)

-- | 'addFreshSuffix3'
addTotal :: Names -> -- ^ The /final/ count of each occurrence after performing an action
            Text -> -- ^ Any symbol
            (Names, Text)
addTotal final s =
  let count = M.findWithDefault 0 s final
      out = if count  <= 1
            then s else (s <> (T.pack $ show count))
  in (M.singleton s 1, out)

-- | 'addSuffices3' iterates over a list of text values and appends a
-- fresh suffix to each one, keeping track of previous seen names. However,
-- for symbols which only occur once, a suffix of value 0 is not appended.
addTotals :: [Text] -> [Text]
addTotals =
  mapWithFinalMonoidManual addTotal
