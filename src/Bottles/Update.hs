{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bottles.Update
  ( updateActions
  , updateBottles
  ) where

import Control.Applicative (liftA2)
import Control.Monad (when, filterM)
import Control.Monad.Except (throwError, tryError)
import Control.Monad.State (MonadState, get, gets, put)
import Data.Either (isRight)
import Data.List (group)
import qualified Data.Map as M

import Bottles.Types (BottleId, Bottle, Bottles, Action(..), Actions, GameState(..), GameError(..), Game)

--------------------
-- Update actions --
--------------------

putActions :: Actions -> Game ()
putActions as = do
  state <- get
  put state { actions = as }

sandbox :: MonadState s m => m a -> m a
sandbox action = do
  s <- get
  result <- action
  put s
  pure result

tryAction :: Action -> Game Bool
tryAction action = isRight <$> sandbox (tryError (updateBottles action))

availableActions :: Game Actions
availableActions = do
  bottleIds <- gets (M.keys . bottles)
  let allActions = liftA2 Pour bottleIds bottleIds
  let toMap = M.fromList . zip [0..]
  toMap <$> filterM tryAction allActions

updateActions :: Game ()
updateActions = availableActions >>= putActions

--------------------
-- Update bottles --
--------------------

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getBottle :: BottleId -> Game Bottle
getBottle bottleId = do
  state <- get
  case M.lookup bottleId state.bottles of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

modifyBottles :: (Bottles -> Bottles) -> Game ()
modifyBottles f = do
  state <- get
  put state { bottles = f state.bottles }

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
  modifyBottles (M.insert from fromTail)
  modifyBottles (M.insert to (fromHead <> toBottle))

updateBottles :: Action -> Game ()
updateBottles (Pour from to) = pour from to
