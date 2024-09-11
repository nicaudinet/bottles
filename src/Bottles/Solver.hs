module Bottles.Solver
  ( solver
  , solve
  ) where

import Bottles.Model (Bottles, Pour)
import Bottles.Utils (headMaybe)
import Bottles.Update (possiblePours, play, gameOver)
import Control.Monad (MonadPlus, mzero, msum)
import Control.Monad.Except (runExcept)

data SolverState = SolverState
  { bottles :: Bottles
  , history :: [Pour]
  }

choose :: MonadPlus m => [a] -> m a
choose = msum . map pure

solver :: MonadPlus m => SolverState -> m SolverState
solver state
  | gameOver (bottles state) = pure state
  | null pours = mzero
  | otherwise = do
      pour <- choose pours
      case runExcept (play pour (bottles state)) of
        Left _ -> mzero
        Right newBottles ->
          let newState = SolverState newBottles (pour : history state)
          in solver newState 
  where
    pours :: [Pour]
    pours = possiblePours (bottles state)

solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solver . flip SolverState []
