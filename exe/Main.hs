module Main where

import Control.Monad.State (StateT, execStateT, get, gets, put, liftIO)
import Control.Monad.Except (ExceptT, runExceptT, handleError)

import Bottles.Create (PuzzleSize(..), createPuzzle)
import Bottles.Parse (getPour)
import Bottles.Solver (solve)
import Bottles.Model (Bottles, GameError(..))
import Bottles.Update (possiblePours, play, gameOver)
import Bottles.Utils (untilM)
import Bottles.View (showBottles, showPour, showGame)

--------------------
-- Main game loop --
--------------------

type Game a = ExceptT GameError (StateT Bottles IO) a

step :: Game ()
step = handleError (liftIO . print) $ do
  bottles <- get
  let pours = possiblePours bottles
  liftIO $ putStrLn (showGame bottles pours)
  line <- liftIO getLine
  action <- getPour pours line
  nextState <- play action bottles
  put nextState

runGame :: Bottles -> Game () -> IO ()
runGame bottles game = do
  endBottles <- execStateT (runExceptT game) bottles
  putStrLn (showBottles endBottles)
  putStrLn "You win!"

parsePuzzleSize :: String -> PuzzleSize
parsePuzzleSize "small" = Small
parsePuzzleSize "medium" = Medium
parsePuzzleSize "large" = Large
parsePuzzleSize _ = error "Invalid puzzle size (choose small, medium, or large)"

main :: IO ()
main = do
  putStrLn "What puzzle do you want to solve? (small/medium/large)"
  size <- parsePuzzleSize <$> getLine
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
      case solve puzzle of
        Nothing -> putStrLn "No solution found"
        Just pours -> mapM_ putStrLn (zipWith showPour [0..] pours)
    _ -> error "Invalid choice (choose 1 or 2)"
