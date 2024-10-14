module Bottles.Create
  ( createPuzzle
  ) where

import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord (comparing)

import Bottles.Model (Bottles, Color, PuzzleSize (..))
import Bottles.Solver (solve)

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  idxs <- replicateM (length xs) getRandom
  pure (map fst (sortBy (comparing snd) (zip xs (idxs :: [Int]))))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | length xs < n = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)

colorPalette :: PuzzleSize -> [Color]
colorPalette Small = map toEnum [0 .. 3]
colorPalette Medium = map toEnum [0 .. 6]
colorPalette Large = map toEnum [0 .. 9]

randomBottles :: MonadRandom m => PuzzleSize -> m Bottles
randomBottles size = do
  let initColors = concat (replicate 4 (colorPalette size))
  randomColors <- shuffle initColors
  let bottles = chunksOf 4 randomColors <> [[], []]
  pure (M.fromList $ zip [0 ..] bottles)

createPuzzle :: MonadRandom m => PuzzleSize -> m Bottles
createPuzzle size = do
  bottles <- randomBottles size
  if isJust (solve bottles)
    then pure bottles
    else createPuzzle size
