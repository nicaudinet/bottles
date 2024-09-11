{-# LANGUAGE FlexibleContexts #-}

module Bottles.Update
  ( possiblePours
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

import Bottles.Utils (headMaybe)
import Bottles.Model ( BottleId, Bottle, Bottles, Pour(..), GameError(..) )

--------------------
-- Update actions --
--------------------

-- | Use the Excpet monad to run an action and check if it throws an error
tryPour :: Pour -> Bottles -> Bool
tryPour p = isRight . runExcept . play p

possiblePours :: Bottles -> [Pour]
possiblePours bs =
  let
    bottleIds = M.keys bs
    allPours = liftA2 Pour bottleIds bottleIds
  in
    filter (flip tryPour bs) allPours

--------------------
-- Update bottles --
--------------------

getBottle :: MonadError GameError m => BottleId -> Bottles -> m Bottle
getBottle bottleId bs = do
  case M.lookup bottleId bs of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

play
  :: MonadError GameError m
  => Pour
  -> Bottles
  -> m Bottles
play (Pour from to) bs = do
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

---------------
-- Game over --
---------------

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a,b,c,d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Bottles -> Bool
gameOver = all validBottle . M.elems
