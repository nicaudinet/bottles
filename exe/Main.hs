module Main where

import Bottles.Create (PuzzleSize (..), createPuzzle)
import Bottles.Model (Bottles)
import Bottles.Solver (solve)
import Bottles.Update (gameOver, parsePour, update)
import Bottles.View (showBottles, showGame, showPour)

step :: Bottles -> IO Bottles
step bottles = do
  putStrLn (showGame bottles)
  line <- getLine
  case (parsePour line >>= update bottles) of
    Right newBottles -> pure newBottles
    Left gameError -> do
      print gameError
      pure bottles

loop :: Bottles -> IO Bottles
loop bottles = do
  newBottles <- step bottles
  if gameOver newBottles
    then pure newBottles
    else loop newBottles

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
    "1" -> do
      endBottles <- loop puzzle
      putStrLn (showBottles endBottles)
      putStrLn "You win!"
    "2" -> do
      putStrLn "---"
      putStrLn "Solution:"
      case solve puzzle of
        Nothing -> putStrLn "No solution found"
        Just pours -> mapM_ putStrLn (zipWith showPour [0 ..] pours)
    _ -> error "Invalid choice (choose 1 or 2)"
