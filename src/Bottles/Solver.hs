{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Bottles.Solver
  ( SolverState(..)
  , toSolverState
  , solver
  , solve
  ) where

import Bottles.Types (Bottles, Action, Actions)
import Bottles.Utils (headMaybe)
import Bottles.Update (availableActions, play, gameOver)
import Control.Monad (MonadPlus, mzero, msum)
import Control.Monad.Except (runExcept)

data SolverState = SolverState
  { bottles :: Bottles
  , history :: [Action]
  }
  deriving Show

toSolverState :: Bottles -> SolverState
toSolverState bs = SolverState { bottles = bs , history = [] }

choose :: MonadPlus m => [a] -> m a
choose = msum . map pure

solver :: MonadPlus m => SolverState -> m SolverState
solver state
  | gameOver state.bottles = pure state
  | null possibleActions = mzero
  | otherwise = do
      action <- choose possibleActions
      case runExcept (play action state.bottles) of
        Left _ -> mzero
        Right bs -> solver $
          SolverState
            { bottles = bs
            , history = action : state.history
            }
  where
    possibleActions :: Actions
    possibleActions = availableActions state.bottles

solve :: Bottles -> Maybe Actions
solve = fmap (reverse . history) . headMaybe . solver . toSolverState
