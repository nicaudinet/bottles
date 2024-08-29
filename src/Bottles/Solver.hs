{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Bottles.Solver
  ( Backtrack(..)
  , SolverState(..)
  , toSolverState
  , solver
  , solution
  , allSolutions
  ) where

import Bottles.Types (Bottles, Action, Actions, GameState(..))
import Bottles.Update (availableActions, play, gameOver)
import Control.Applicative ((<|>))
import Control.Monad.Except (runExcept)
import qualified Data.Map as M

data Backtrack a
  = Fail
  | Return a
  | Backtrack a :|: Backtrack a
  deriving Show

instance Functor Backtrack where
  fmap :: (a -> b) -> Backtrack a -> Backtrack b
  fmap f (Return a) = Return (f a)
  fmap _ Fail = Fail
  fmap f (a :|: b) = fmap f a :|: fmap f b

instance Applicative Backtrack where
  pure :: a -> Backtrack a
  pure = Return
  (<*>) :: Backtrack (a -> b) -> Backtrack a -> Backtrack b
  Fail <*> _ = Fail
  _ <*> Fail = Fail
  Return f <*> Return a = Return (f a)
  Return f <*> (a :|: b) = (Return f <*> a) :|: (Return f <*> b)
  (f :|: g) <*> a = (f <*> a) :|: (g <*> a)

instance Monad Backtrack where
  return :: a -> Backtrack a
  return = pure
  (>>=) :: Backtrack a -> (a -> Backtrack b) -> Backtrack b
  Return a >>= r = r a
  Fail >>= _ = Fail
  (a :|: b) >>= r = (a >>= r) :|: (b >>= r)

type Solver a = Backtrack a

data SolverState = SolverState
  { bottles :: Bottles
  , history :: [Action]
  }
  deriving Show

toSolverState :: GameState -> SolverState
toSolverState gs = SolverState { bottles = gs.bottles, history = [] }

select :: [a] -> Backtrack a
select [] = Fail
select (a:as) = foldr (:|:) (Return a) (map Return as)

solver :: SolverState -> Solver SolverState
solver state
  | gameOver state.bottles = Return state
  | M.null possibleActions = Fail
  | otherwise = do
      action <- select (M.elems possibleActions)
      case runExcept (play action state.bottles) of
        Left _ -> Fail
        Right bs -> solver (state { bottles = bs, history = action : state.history })
  where
    possibleActions :: Actions
    possibleActions = availableActions state.bottles

solution :: Solver SolverState -> Maybe [Action]
solution Fail = Nothing
solution (Return state) = Just (reverse state.history)
solution (a :|: b) = solution a <|> solution b

allSolutions :: Solver SolverState -> [SolverState]
allSolutions Fail = []
allSolutions (Return a) = [a]
allSolutions (a :|: b) = allSolutions a <> allSolutions b
