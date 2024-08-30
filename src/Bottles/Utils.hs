module Bottles.Utils
  ( headMaybe
  , (!?)
  , untilM
  ) where

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

(!?) :: [a] -> Int -> Maybe a
(!?) xs idx = lookup idx (zip [0..] xs)

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action
