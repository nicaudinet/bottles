module Bottles.View
  ( showBottles
  , showPour
  , showGame
  ) where

import Bottles.Model (Bottle, Bottles, Color (..), Pour (..))
import Bottles.Utils (headMaybe, tailSafe)
import Data.List (intercalate, intersperse, unfoldr)
import qualified Data.Map as M

data Square = Empty | Separator | Full Color
type Line = [Square]

bottlesToGrid :: Bottles -> [Line]
bottlesToGrid = reverse . unfoldr makeLine . map reverse . M.elems
 where
  getSquare :: Bottle -> Square
  getSquare = maybe Empty Full . headMaybe

  makeLine :: [Bottle] -> Maybe (Line, [Bottle])
  makeLine xs
    | all null xs = Nothing
    | otherwise = Just (map getSquare xs, map tailSafe xs)

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

showLine :: Line -> String
showLine line =
  let sepLine = [Separator] <> intersperse Separator line <> [Separator]
   in concatMap showSquare sepLine

showIndices :: Int -> String
showIndices n =
  let
    pad = replicate (length (showSquare Separator)) ' '
    numbers = [(if i < 10 then " " else "") <> show i | i <- [0 .. (n - 1)]]
   in
    pad <> intercalate pad numbers

showLines :: [Line] -> String
showLines ls =
  let
    rows = intercalate "\n" (map showLine ls)
    idxs = showIndices (length (head ls))
   in
    rows <> "\n" <> idxs

showBottles :: Bottles -> String
showBottles = showLines . bottlesToGrid

showPour :: Int -> Pour -> String
showPour idx (Pour from to) =
  concat
    [ if idx < 10 then " " else ""
    , show idx
    , ": "
    , show from
    , " -> "
    , show to
    ]

showGame :: Bottles -> String
showGame bottles = "---\n" <> showBottles bottles <> "\n---"
