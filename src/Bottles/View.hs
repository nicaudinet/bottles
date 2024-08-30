{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bottles.View
  ( showBottles
  , showGame
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.List (intercalate, unfoldr)
import qualified Data.Map as M

import Bottles.Types
  (Color(..), Bottles, Action(..), Actions, GameState(..), Game)

data Square = Empty | Separator | Full Color
type Row = [Square]
type Grid = [Row]

bottlesToGrid :: Bottles -> Grid
bottlesToGrid bs = unfoldr makeRow (M.elems bs, 3)
  where
    makeRow :: ([[Color]], Int) -> Maybe (Row, ([[Color]], Int))
    makeRow (xs, n)
      | n < 0 = Nothing
      | otherwise =
        let
          getSquare x
            | n < length x = Full (reverse x !! n)
            | otherwise = Empty
          newRow = map getSquare xs
        in
          Just (newRow, (xs, n - 1))

-- xterm-256color color codes
-- https://stackabuse.com/how-to-print-colored-text-in-python/
showColor :: Color -> String
showColor color = "\x1b[48;5;" <> show (colorCode color) <> "m  \x1b[0m"
  where
    colorCode :: Color -> Int
    colorCode Yellow = 220
    colorCode LightBlue = 44
    colorCode DarkBlue = 21
    colorCode Brown = 130
    colorCode LightGreen = 47
    colorCode DarkGreen = 22
    colorCode Pink = 201
    colorCode White = 255
    colorCode Red = 196
    colorCode Orange = 208
    colorCode DarkRed = 88

showSquare :: Square -> String
showSquare Empty = "  "
showSquare Separator = "|"
showSquare (Full color) = showColor color

showRow :: Row -> String
showRow row =
  let
    squares = map showSquare row
    separator = showSquare Separator
  in separator <> intercalate separator squares <> separator

showIndices :: Int -> String
showIndices n =
  let
    pad = replicate (length (showSquare Separator)) ' '
    numbers = [ (if i < 10 then " " else "") <> show i | i <- [0..(n-1)] ]
  in
    pad <> intercalate pad numbers

showGrid :: Grid -> String
showGrid grid =
  let
    rows = intercalate "\n" (map showRow grid)
    idxs = showIndices (length (head grid))
  in
    rows <> "\n" <> idxs

showBottles :: Bottles -> String
showBottles = showGrid . bottlesToGrid

showAction :: Int -> Action -> String
showAction idx (Pour from to) = concat
  [ if idx < 10 then " " else ""
  , show idx
  , ": "
  , show from
  , " -> "
  , show to
  ]

showActions :: Actions -> String
showActions = intercalate "\n" . zipWith showAction [0..]

showGame :: Game ()
showGame = do
  state <- get
  liftIO $ do
    putStrLn "---"
    putStrLn (showBottles state.bottles)
    putStrLn "---"
    putStrLn (showActions state.actions)
    putStrLn "---"
