module Bottles.Create
  ( PuzzleSize(..)
  , createPuzzle
  ) where

import Bottles.Types (Bottles)
import Bottles.Solver (solve)
import Control.Monad (replicateM)
import Control.Monad.Random (MonadRandom, getRandom)
import Data.List (sortBy)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Data.Ord (comparing)

data PuzzleSize = Small | Medium | Large

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
  idxs <- replicateM (length xs) getRandom
  pure (map fst (sortBy (comparing snd) (zip xs (idxs :: [Int]))))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null xs = []
  | length xs < n = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)

randomBottles :: MonadRandom m => Int -> m Bottles
randomBottles n = do
  let initColors = concat $ replicate 4 $ map toEnum [0..n-1]
  randomColors <- shuffle initColors
  let bottles = chunksOf 4 randomColors <> [[], []]
  pure (M.fromList $ zip [0..] bottles)

sizeToInt :: PuzzleSize -> Int
sizeToInt Small = 4
sizeToInt Medium = 7
sizeToInt Large = 10

createPuzzle :: MonadRandom m => PuzzleSize -> m Bottles
createPuzzle size = do
  bottles <- randomBottles (sizeToInt size)
  if isJust (solve bottles)
  then pure bottles
  else createPuzzle size
