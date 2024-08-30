{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad.State
import Control.Monad.Except
import Text.Read (readMaybe)

import Bottles.Types (Action, GameState(..), Game, GameError(..))
import Bottles.View (showBottles, showGame)
import Bottles.Update (availableActions, play, gameOver)
import Bottles.Utils ((!?), untilM)
import Bottles.Solver (solve)
import Bottles.Create (PuzzleSize(..), createPuzzle)

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

parsePuzzle :: String -> PuzzleSize
parsePuzzle "small" = Small
parsePuzzle "medium" = Medium
parsePuzzle "large" = Large
parsePuzzle _ = error "Invalid puzzle size (choose small, medium, or large)"

main :: IO ()
main = do
  putStrLn "What puzzle do you want to solve? (small/medium/large)"
  size <- parsePuzzle <$> getLine
  puzzle <- createPuzzle size
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
      putStrLn "---"
      putStrLn "Solution:"
      maybe
        (putStrLn "No solution found")
        (mapM_ print)
        (solve initState.bottles)
    _ -> error "Invalid choice (choose 1 or 2)"
