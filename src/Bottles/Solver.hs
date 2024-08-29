{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE InstanceSigs #-}

module Bottles.Solver
  ( Backtrack(..)
  , solver
  , allSolutions
  ) where

import Bottles.Types (GameState(..))
import Bottles.Update (availableActions, play, gameOver)
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

select :: [a] -> Backtrack a
select [] = Fail
select (a:as) = foldr (:|:) (Return a) (map Return as)

solver :: GameState -> Solver GameState
solver gs
  | gameOver gs = Return gs
  | M.null gs.actions = Fail
  | otherwise = do
      action <- select (M.elems gs.actions)
      case runExcept (play action gs.bottles) of
        Left _ -> Fail
        Right bs -> solver (gs { bottles = bs, actions = availableActions bs })

allSolutions :: Backtrack GameState -> [GameState]
allSolutions Fail = []
allSolutions (Return a) = [a]
allSolutions (a :|: b) = allSolutions a ++ allSolutions b
