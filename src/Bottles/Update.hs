{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}

module Bottles.Update
  ( availableActions
  , play
  , gameOver
  ) where

import Control.Applicative (liftA2)
import Control.Monad (when)
import Control.Monad.Except (runExcept)
import Control.Monad.Error.Class (MonadError, throwError)
import Data.Either (isRight)
import Data.List (group)
import qualified Data.Map as M

import Bottles.Types
  ( BottleId, Bottle, Bottles, Action(..), Actions, GameState(..), GameError(..))

--------------------
-- Update actions --
--------------------

-- | Use the Excpet monad to run an action and check if it throws an error
tryAction :: Bottles -> Action -> Bool
tryAction bs a = isRight (runExcept (play a bs))

availableActions :: Bottles -> Actions
availableActions bs =
  let
    bottleIds = M.keys bs
    allActions = liftA2 Pour bottleIds bottleIds
  in
    M.fromList (zip [0..] (filter (tryAction bs) allActions))

--------------------
-- Update bottles --
--------------------

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getBottle :: MonadError GameError m => BottleId -> Bottles -> m Bottle
getBottle bottleId bs = do
  case M.lookup bottleId bs of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

pour
  :: MonadError GameError m
  => BottleId
  -> BottleId
  -> Bottles
  -> m Bottles
pour from to bs = do
  -- Get the two bottles
  fromBottle <- getBottle from bs
  toBottle <- getBottle to bs
  -- Check that from and to are different
  when (from == to) $
    throwError (FromAndToAreTheSame from)
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> throwError (FromBottleIsEmpty from)
    (x:xs) -> pure (x, concat xs)
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
  let f1 = M.insert from fromTail
  let f2 = M.insert to (fromHead <> toBottle)
  pure . f1 . f2 $ bs

play :: MonadError GameError m => Action -> Bottles -> m Bottles
play (Pour from to) = pour from to

---------------
-- Game over --
---------------

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a,b,c,d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: GameState -> Bool
gameOver = all validBottle . M.elems . bottles
