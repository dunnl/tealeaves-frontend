module Names where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Lazy
import Data.Traversable

type Names = Map Text Int

evalStateOn = flip evalState
runStateOn = flip runState
execStateOn = flip execState

getFreshSuffix :: Text -> State Names Int
getFreshSuffix name = do
  max <- gets (M.findWithDefault 0 name)
  modify' (M.insertWith (+) name 1)
  return max

addFreshSuffix :: Text -> State Names Text
addFreshSuffix s = do
  suf <- getFreshSuffix s
  return (s <> (T.pack $ show suf))

addSuffices :: [Text] -> [Text]
addSuffices =
  evalStateOn M.empty .
  traverse addFreshSuffix

addFreshSuffix' :: Names -> Text -> State Names Text
addFreshSuffix' final s = do
  suf <- getFreshSuffix s
  return $ if M.findWithDefault 0 s final <= 1
           then s else (s <> (T.pack $ show suf))

addSuffices' :: [Text] -> [Text]
addSuffices' input =
  let (out, names_final) = runStateOn M.empty (traverse (addFreshSuffix' names_final) input)
  in out
