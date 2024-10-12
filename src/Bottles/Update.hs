{-# LANGUAGE FlexibleContexts #-}

module Bottles.Update
  ( parsePour
  , update
  , gameOver
  ) where

import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Model (Bottle, BottleId, Bottles, GameError (..), Pour (..))
import Bottles.Utils (headMaybe)

--------------------
-- Update bottles --
--------------------

parsePour :: MonadError GameError m => String -> m Pour
parsePour line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- case readMaybe from of
      Nothing -> throwError (InvalidBottleId from)
      Just a -> pure a
    toBottle <- case readMaybe to of
      Nothing -> throwError (InvalidBottleId to)
      Just a -> pure a
    pure (Pour fromBottle toBottle)
  _ -> throwError (InvalidInput line)

getBottle :: MonadError GameError m => BottleId -> Bottles -> m Bottle
getBottle bottleId bs = do
  case M.lookup bottleId bs of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

update :: MonadError GameError m => Pour -> Bottles -> m Bottles
update (Pour from to) bs = do
  -- Get the two bottles
  fromBottle <- getBottle from bs
  toBottle <- getBottle to bs
  -- Check that from and to are different
  when (from == to) $
    throwError (FromAndToAreTheSame from)
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> throwError (FromBottleIsEmpty from)
    (x : xs) -> pure (x, concat xs)
  -- Check we're not just swapping bottles
  when (null fromTail && null toBottle) $
    throwError NoOpAction
  -- Check there's space in the to bottle
  when (length toBottle > 4 - length fromHead) $
    throwError (ToBottleIsTooFull to)
  -- Check the colors match
  let fromColor = head fromHead
  case headMaybe toBottle of
    Nothing -> pure ()
    Just toColor ->
      when (fromColor /= toColor) $
        throwError (ColorsDontMatch fromColor toColor)
  -- Pour a color from one bottle to the other
  pure . M.insert from fromTail . M.insert to (fromHead <> toBottle) $ bs

---------------
-- Game over --
---------------

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a, b, c, d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Bottles -> Bool
gameOver = all validBottle . M.elems
