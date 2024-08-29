{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Bottles.Solver
  ( SolverState(..)
  , toSolverState
  , solver
  , solution
  ) where

import Bottles.Types (Bottles, Action, Actions, GameState(..))
import Bottles.Update (availableActions, play, gameOver)
import Control.Monad (MonadPlus, mzero, msum)
import Control.Monad.Except (runExcept)

data SolverState = SolverState
  { bottles :: Bottles
  , history :: [Action]
  }
  deriving Show

toSolverState :: GameState -> SolverState
toSolverState gs =
  SolverState
    { bottles = gs.bottles
    , history = []
    }

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

solution :: [SolverState] -> Maybe Actions
solution [] = Nothing
solution (a:_) = Just (reverse a.history)
