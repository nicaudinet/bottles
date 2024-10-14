module Bottles.Solver
  ( solver
  , solve
  ) where

import Bottles.Model (Bottles, Pour (..))
import Bottles.Update (gameOver, update)
import Bottles.Utils (headMaybe)
import Control.Applicative (liftA2)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data SolverState = SolverState
  { history :: [Pour]
  , current :: Bottles
  }

tryPour :: Bottles -> Pour -> Maybe (Pour, Bottles)
tryPour bottles pour =
  case update bottles pour of
    Left _ -> Nothing
    Right newBottles -> Just (pour, newBottles)

possiblePours :: Bottles -> [(Pour, Bottles)]
possiblePours bottles =
  let bottleIds = M.keys bottles
      pours = liftA2 Pour bottleIds bottleIds
   in mapMaybe (tryPour bottles) pours

solver :: SolverState -> [SolverState]
solver state
  | gameOver (current state) = pure state
  | otherwise = do
      (pour, newBottles) <- possiblePours (current state)
      solver (SolverState (pour : history state) newBottles)

solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solver . SolverState []
