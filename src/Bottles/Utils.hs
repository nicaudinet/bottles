module Bottles.Utils
  ( headMaybe
  , tailSafe
  , untilM
  , shuffle
  ) where

import Control.Monad.Random (MonadRandom, getRandom, replicateM)
import Data.List (sortBy)
import Data.Ord (comparing)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

tailSafe :: [a] -> [a]
tailSafe [] = []
tailSafe (_:xs) = xs

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  idxs <- replicateM (length xs) getRandom
  pure (map fst (sortBy (comparing snd) (zip xs (idxs :: [Int]))))
