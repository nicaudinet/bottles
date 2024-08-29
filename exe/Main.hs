{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Bottles.Types (Color(..), Bottles, Action, GameState(..), Game)
import Bottles.View (showBottles, showGame)
import Bottles.Parse (getAction)
import Bottles.Update (availableActions, play, gameOver)
import Bottles.Solver (SolverState(history), toSolverState, solver, solution, allSolutions)
import Bottles.Solver.Dot (printDot)

-------------
-- Puzzles --
-------------

puzzleTrivial :: Bottles
puzzleTrivial = M.fromList . zip [0..] $
  [ [ Red, Yellow, Yellow, Yellow ]
  , [ Yellow, Red, Red, Red ]
  , []
  , []
  ]

puzzleEasy :: Bottles
puzzleEasy = M.fromList . zip [0..] $
  [ [ Red, Red, Red, Yellow ]
  , [ Yellow, Yellow, Red, Yellow ]
  , []
  , []
  ]

puzzleHard :: Bottles
puzzleHard = M.fromList . zip [0..] $
  [ [ Yellow, DarkBlue, Brown, LightGreen ]
  , [ Pink, LightGreen, Brown, LightBlue ]
  , [ LightGreen, White, Pink, LightBlue ]
  , [ Red, DarkBlue, Red, LightBlue ]
  , [ White, Brown, White, Orange ]
  , [ DarkGreen, LightBlue, DarkGreen, Orange ]
  , [ LightGreen, Yellow, DarkGreen, Orange ]
  , [ White, Pink, Pink, DarkBlue ]
  , [ Yellow, DarkBlue, Red, Red ]
  , [ DarkGreen, Brown, Orange, Yellow ]
  , []
  , []
  ]

--------------------
-- Main game loop --
--------------------

updateActions :: Game ()
updateActions = do
  gs <- get
  let as = availableActions gs.bottles
  put (gs { actions = as })

updateBottles :: Action -> Game ()
updateBottles action = do
  gs <- get
  bs <- play action gs.bottles
  put (gs { bottles = bs })

step :: Game ()
step = handleError (liftIO . print) $ do
  updateActions
  showGame
  action <- getAction
  updateBottles action

runGame :: GameState -> Game () -> IO ()
runGame initState game = do
  finalState <- execStateT (runExceptT game) initState
  putStrLn (showBottles finalState.bottles)
  putStrLn "You win!"

choosePuzzle :: String -> Bottles
choosePuzzle "trivial" = puzzleTrivial
choosePuzzle "easy" = puzzleEasy
choosePuzzle "hard" = puzzleHard
choosePuzzle _ = error "Invalid puzzle type"

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action

main :: IO ()
main = do
  let puzzle = puzzleHard
  let as = availableActions puzzle
  let initGameState = GameState { bottles = puzzle, actions = as }
  let computeTree = solver (toSolverState initGameState)
  putStrLn "Solution:"
  print (solution computeTree)
  putStrLn "All solutions:"
  mapM_ (print . history) (allSolutions computeTree)
  void $ printDot "computeTree" computeTree

-- main :: IO ()
-- main = do
--   putStrLn "What puzzle do you want? (trivial/easy/hard) "
--   puzzle <- choosePuzzle <$> getLine
--   let as = availableActions puzzle
--   let initState = GameState { bottles = puzzle, actions = as }
--   runGame initState (untilM (gets gameOver) step)
