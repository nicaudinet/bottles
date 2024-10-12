module Bottles.Solver
  ( solver
  , solve
  ) where

import Bottles.Model (Bottles, Pour (..))
import Bottles.Update (gameOver, update)
import Bottles.Utils (headMaybe)
import Control.Applicative (liftA2)
import Control.Monad (MonadPlus, msum, mzero)
import Control.Monad.Except (runExcept)
import qualified Data.Map as M
import Data.Maybe (isJust)

data SolverState = SolverState
  { bottles :: Bottles
  , history :: [Pour]
  }

-- | Use the Excpet monad to run an action and check if it throws an error
tryPour :: Bottles -> Pour -> Maybe Bottles
tryPour bs = either (const Nothing) Just . runExcept . flip update bs

possiblePours :: Bottles -> [Pour]
possiblePours bs =
  let pours = liftA2 Pour (M.keys bs) (M.keys bs)
   in filter (isJust . tryPour bs) pours

choose :: MonadPlus m => [a] -> m a
choose = msum . map pure

solver :: MonadPlus m => SolverState -> m SolverState
solver state
  | gameOver (bottles state) = pure state
  | null pours = mzero
  | otherwise = do
      pour <- choose pours
      case tryPour (bottles state) pour of
        Nothing -> mzero
        Just newBottles ->
          let newState = SolverState newBottles (pour : history state)
           in solver newState
 where
  pours :: [Pour]
  pours = possiblePours (bottles state)

solve :: Bottles -> Maybe [Pour]
solve = fmap (reverse . history) . headMaybe . solver . flip SolverState []
