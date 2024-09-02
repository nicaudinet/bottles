module Main where

import Control.Monad.State
import Control.Monad.Except
import Text.Read (readMaybe)

import Bottles.Types (Action, Actions, Bottles, GameError(..))
import Bottles.View (showBottles, showGame)
import Bottles.Update (availableActions, play, gameOver)
import Bottles.Utils ((!?), untilM, shuffle)
import Bottles.Solver (solve)
import Bottles.Create (PuzzleSize(..), createPuzzle)

--------------------
-- Main game loop --
--------------------

type Game a = ExceptT GameError (StateT Bottles IO) a

getAction :: Actions -> Game Action
getAction actions = do
  line <- liftIO getLine
  idx <- maybe (throwError (InvalidInput line)) pure (readMaybe line)
  maybe (throwError (ActionNotFound idx)) pure (actions !? idx)

step :: Game ()
step = handleError (liftIO . print) $ do
  bottles <- get
  actions <- liftIO (shuffle (availableActions bottles))
  liftIO (putStrLn (showGame bottles actions))
  action <- getAction actions
  newBottles <- play action bottles 
  put newBottles

runGame :: Bottles -> Game () -> IO ()
runGame startBottles game = do
  endBottles <- execStateT (runExceptT game) startBottles
  putStrLn (showBottles endBottles)
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
  putStrLn "---"
  putStrLn "What do you want to do? (1/2)"
  putStrLn "1: solve the puzzle yourself"
  putStrLn "2: let the computer solve the puzzle"
  putStrLn "---"
  choice <- getLine
  case choice of
    "1" -> runGame puzzle (untilM (gets gameOver) step)
    "2" -> do
      putStrLn "---"
      putStrLn "Solution:"
      maybe
        (putStrLn "No solution found")
        (mapM_ print)
        (solve puzzle)
    _ -> error "Invalid choice (choose 1 or 2)"
