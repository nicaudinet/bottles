{-# LANGUAGE FlexibleContexts #-}

module Bottles.Update
  ( parsePuzzleSize
  , parsePour
  , update
  , gameOver
  ) where

import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Model (Bottle, Bottles, GameError (..), Pour (..), PuzzleSize (..))
import Bottles.Utils (headMaybe)

maybeThrow :: GameError -> Maybe a -> Either GameError a
maybeThrow err Nothing = Left err
maybeThrow _ (Just a) = Right a

whenThrow :: Bool -> GameError -> Either GameError ()
whenThrow True err = Left err
whenThrow False _ = Right ()

----------------------
-- Parse user input --
----------------------

parsePuzzleSize :: String -> Either String PuzzleSize
parsePuzzleSize "small" = pure Small
parsePuzzleSize "medium" = pure Medium
parsePuzzleSize "large" = pure Large
parsePuzzleSize _ = Left "Invalid puzzle size (choose small, medium, or large)"

parsePour :: String -> Either GameError Pour
parsePour line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- maybeThrow (InvalidBottleId from) (readMaybe from)
    toBottle <- maybeThrow (InvalidBottleId to) (readMaybe to)
    pure (Pour fromBottle toBottle)
  _ -> Left (InvalidInput line)

--------------------
-- Update bottles --
--------------------

-- Get the two bottles involved in the pour
getPourBottles :: Bottles -> Pour -> Either GameError (Bottle, Bottle)
getPourBottles bottles (Pour from to) = do
  fromBottle <- maybeThrow (BottleNotFound from) (M.lookup from bottles)
  toBottle <- maybeThrow (BottleNotFound to) (M.lookup to bottles)
  pure (fromBottle, toBottle)

-- Pour a bottle into another, check that the pour is valid
validate :: Pour -> Bottle -> Bottle -> Either GameError (Bottle, Bottle)
validate (Pour from to) fromBottle toBottle = do
  -- Check that from and to are different
  whenThrow (from == to) (FromAndToAreTheSame from)
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> Left (FromBottleIsEmpty from)
    (x : xs) -> Right (x, concat xs)
  -- Check we're not just swapping bottles
  whenThrow (null fromTail && null toBottle) NoOpAction
  -- Check there's space in the to bottle
  whenThrow (length toBottle > 4 - length fromHead) (ToBottleIsTooFull to)
  -- Check the colors match
  case headMaybe toBottle of
    Nothing -> Right ()
    Just toColor -> do
      let fromColor = head fromHead
      whenThrow (fromColor /= toColor) (ColorsDontMatch fromColor toColor)
  -- Return the split in the from bottle
  pure (fromHead, fromTail)

update :: Bottles -> Pour -> Either GameError Bottles
update bottles pour@(Pour from to) = do
  (fromBottle, toBottle) <- getPourBottles bottles pour
  (fromHead, fromTail) <- validate pour fromBottle toBottle
  let insertFrom = M.insert from fromTail
  let insertTo = M.insert to (fromHead <> toBottle)
  pure . insertFrom . insertTo $ bottles

---------------
-- Game over --
---------------

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a, b, c, d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Bottles -> Bool
gameOver = all validBottle . M.elems
