module Bottles.Update
  ( update
  ) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, modify)
import Data.List (group)
import qualified Data.Map as M

import Bottles.Types (BottleId, Bottle, Action(..), GameError(..), Game)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getBottle :: BottleId -> Game Bottle
getBottle bottleId = do
  bottles <- get
  case M.lookup bottleId bottles of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

pour :: BottleId -> BottleId -> Game ()
pour from to = do
  -- Get the two bottles
  fromBottle <- getBottle from
  toBottle <- getBottle to
  -- Check that from and to are different
  when (from == to) $
    throwError (FromAndToAreTheSame from)
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> throwError (FromBottleIsEmpty from)
    (x:xs) -> pure (x, concat xs)
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
  modify (M.insert from fromTail)
  modify (M.insert to (fromHead <> toBottle))

update :: Action -> Game ()
update (Pour from to) = pour from to
