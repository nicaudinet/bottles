module Main where

import Control.Monad.State (StateT, execStateT, get, gets, put, liftIO)
import Control.Monad.Except (ExceptT, runExceptT, handleError)

import Bottles.Create (PuzzleSize(..), createPuzzle)
import Bottles.Parse (getAction)
import Bottles.Solver (solve)
import Bottles.Types (Bottles, Action(..), GameError(..), GameState(..))
import Bottles.Update (possiblePours, play, gameOver)
import Bottles.Utils (untilM, shuffle)
import Bottles.View (showBottles, showPour, showGame)

--------------------
-- Main game loop --
--------------------

type Game a = ExceptT GameError (StateT GameState IO) a

step :: Game ()
step = handleError (liftIO . print) $ do
  state <- get
  let pours = possiblePours (bottles state)
  let actions = Backtrack : map Move pours
  liftIO $ putStrLn (showGame (bottles state) actions)
  line <- liftIO getLine
  action <- getAction pours line
  nextState <- play action state
  put nextState

runGame :: Bottles -> Game () -> IO ()
runGame start game = do
  let initState = GameState { bottles = start, history = [] }
  endState <- execStateT (runExceptT game) initState
  putStrLn (showBottles (bottles endState))
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
    "1" -> runGame puzzle (untilM (gets (gameOver . bottles)) step)
    "2" -> do
      putStrLn "---"
      putStrLn "Solution:"
      case solve puzzle of
        Nothing -> putStrLn "No solution found"
        Just pours -> mapM_ putStrLn (zipWith showPour [0..] pours)
    _ -> error "Invalid choice (choose 1 or 2)"
