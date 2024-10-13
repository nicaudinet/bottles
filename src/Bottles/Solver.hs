module Bottles.Solver
  ( solver
  , solve
  ) where

import Bottles.Model (Bottles, Pour (..))
import Bottles.Update (gameOver, update)
import Bottles.Utils (headMaybe)
import Control.Applicative (liftA2)
import Control.Monad (MonadPlus, msum, mzero)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

data SolverState = SolverState
  { current :: Bottles
  , history :: [Pour]
  }

tryPour :: Bottles -> Pour -> Maybe (Pour, Bottles)
tryPour bs pour =
  case update bs pour of
    Left _ -> Nothing
    Right newBottles -> Just (pour, newBottles)

possiblePours :: Bottles -> [(Pour, Bottles)]
possiblePours bs =
  let pours = liftA2 Pour (M.keys bs) (M.keys bs)
   in catMaybes (map (tryPour bs) pours)

choose :: MonadPlus m => [a] -> m a
choose = msum . map pure

solver :: MonadPlus m => SolverState -> m SolverState
solver state
  | gameOver (current state) = pure state
  | null pours = mzero
  | otherwise = do
      (pour, newBottles) <- choose pours
      solver (SolverState newBottles (pour : history state))
 where
  pours :: [(Pour, Bottles)]
  pours = possiblePours (current state)

solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solver . flip SolverState []
