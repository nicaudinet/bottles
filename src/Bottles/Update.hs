{-# LANGUAGE FlexibleContexts #-}

module Bottles.Update
  ( parsePour
  , update
  , gameOver
  ) where

import Data.List (group)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Model (Bottle, Bottles, GameError (..), Pour (..))
import Bottles.Utils (headMaybe)

--------------------
-- Update bottles --
--------------------

maybeThrow :: GameError -> Maybe a -> Either GameError a
maybeThrow err Nothing = Left err
maybeThrow _ (Just a) = Right a

whenThrow :: Bool -> GameError -> Either GameError ()
whenThrow True err = Left err
whenThrow False _ = Right ()

parsePour :: String -> Either GameError Pour
parsePour line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- maybeThrow (InvalidBottleId from) (readMaybe from)
    toBottle <- maybeThrow (InvalidBottleId to) (readMaybe to)
    pure (Pour fromBottle toBottle)
  _ -> Left (InvalidInput line)

update :: Bottles -> Pour -> Either GameError Bottles
update bs (Pour from to) = do
  -- Get the bottles
  fromBottle <- maybeThrow (BottleNotFound from) (M.lookup from bs)
  toBottle <- maybeThrow (BottleNotFound to) (M.lookup to bs)
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
  let fromColor = head fromHead
  case headMaybe toBottle of
    Nothing -> Right ()
    Just toColor ->
      whenThrow (fromColor /= toColor) (ColorsDontMatch fromColor toColor)
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
