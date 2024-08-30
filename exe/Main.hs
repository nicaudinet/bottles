{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Types (Color(..), Bottles, Action, GameState(..), Game, GameError(..))
import Bottles.View (showBottles, showGame)
import Bottles.Update (availableActions, play, gameOver)
import Bottles.Solver (toSolverState, solver, solution)

-------------
-- Puzzles --
-------------

choosePuzzle :: String -> Bottles
choosePuzzle "trivial" = puzzleTrivial
choosePuzzle "easy" = puzzleEasy
choosePuzzle "hard" = puzzleHard
choosePuzzle _ = error "Invalid puzzle"

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
import Bottles.Utils ((!?), untilM)

--------------------
-- Main game loop --
--------------------

getAction :: Game Action
getAction = do
  line <- liftIO getLine
  idx <- case readMaybe line of
    Nothing -> throwError (InvalidInput line)
    Just a -> pure a
  as <- gets actions
  case as !? idx of
    Nothing -> throwError (ActionNotFound idx)
    Just a -> pure a

update :: Action -> Game ()
update action = do
  gs <- get
  bs <- play action gs.bottles
  put (gs { bottles = bs, actions = availableActions bs })

step :: Game ()
step = handleError (liftIO . print) $ do
  showGame
  action <- getAction
  update action

runGame :: GameState -> Game () -> IO ()
runGame initState game = do
  finalState <- execStateT (runExceptT game) initState
  putStrLn (showBottles finalState.bottles)
  putStrLn "You win!"

main :: IO ()
main = do
  putStrLn "What puzzle do you want to solve? (trivial/easy/hard)"
  puzzle <- choosePuzzle <$> getLine
  let as = availableActions puzzle
  let initState = GameState { bottles = puzzle, actions = as }
  putStrLn "---"
  putStrLn "What do you want to do? (1/2)"
  putStrLn "1: solve the puzzle yourself"
  putStrLn "2: let the computer solve the puzzle"
  putStrLn "---"
  choice <- getLine
  case choice of
    "1" -> runGame initState (untilM (gets (gameOver . bottles)) step)
    "2" -> do
      let computeTree = solver (toSolverState initState)
      putStrLn "---"
      putStrLn "Solution:"
      maybe (putStrLn "No solution found") (mapM_ print) (solution computeTree)
      -- putStrLn "All solutions:"
      -- mapM_ (print . history) (allSolutions computeTree)
    _ -> error "Invalid choice (choose 1 or 2)"
