module Bottles.Solver
  ( solver
  , solve
  ) where

import Bottles.Types (Bottles, Pour, Action(..), GameState(..), initGameState)
import Bottles.Utils (headMaybe)
import Bottles.Update (possiblePours, play, gameOver)
import Control.Monad (MonadPlus, mzero, msum)
import Control.Monad.Except (runExcept)

choose :: MonadPlus m => [a] -> m a
choose = msum . map pure

solver :: MonadPlus m => GameState -> m GameState
solver state
  | gameOver (bottles state) = pure state
  | null pours = mzero
  | otherwise = do
      pour <- choose pours
      case runExcept (play (Move pour) state) of
        Left _ -> mzero
        Right state' -> solver state'
  where
    pours :: [Pour]
    pours = possiblePours (bottles state)

solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solver . initGameState
