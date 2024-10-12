module Main where

import Control.Monad.Except (ExceptT, handleError, runExceptT)
import Control.Monad.State (StateT, execStateT, get, gets, liftIO, put)

import Bottles.Create (PuzzleSize (..), createPuzzle)
import Bottles.Model (Bottles, GameError (..))
import Bottles.Solver (solve)
import Bottles.Update (gameOver, parsePour, update)
import Bottles.Utils (untilM)
import Bottles.View (showBottles, showGame, showPour)

type Game a = ExceptT GameError (StateT Bottles IO) a

step :: Game ()
step = handleError (liftIO . print) $ do
  bottles <- get
  liftIO $ putStrLn (showGame bottles)
  line <- liftIO getLine
  action <- parsePour line
  nextState <- update action bottles
  put nextState

runGame :: Bottles -> IO ()
runGame bottles = do
  let game = untilM (gets gameOver) step
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
    "1" -> runGame puzzle
    "2" -> do
      putStrLn "---"
      putStrLn "Solution:"
      case solve puzzle of
        Nothing -> putStrLn "No solution found"
        Just pours -> mapM_ putStrLn (zipWith showPour [0 ..] pours)
    _ -> error "Invalid choice (choose 1 or 2)"
