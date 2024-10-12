{-# LANGUAGE FlexibleContexts #-}

module Main where

import Bottles.Create (PuzzleSize (..), createPuzzle)
import Bottles.Model (Bottles, GameError (..))
import Bottles.Solver (solve)
import Bottles.Update (gameOver, parsePour, update)
import Bottles.View (showBottles, showGame, showPour)
import Control.Monad.Except (MonadError, handleError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)

handler :: MonadIO m => Bottles -> GameError -> m Bottles
handler bottles err = do
  liftIO (print err)
  pure bottles

step :: (MonadError GameError m, MonadIO m) => Bottles -> m Bottles
step bottles = handleError (handler bottles) $ do
  liftIO $ putStrLn (showGame bottles)
  line <- liftIO getLine
  action <- parsePour line
  update action bottles

loop :: (MonadError GameError m, MonadIO m) => Bottles -> m Bottles
loop bottles = do
  newBottles <- step bottles
  if gameOver newBottles
    then pure newBottles
    else loop newBottles

runGame :: Bottles -> IO ()
runGame bottles = do
  Right endBottles <- runExceptT (loop bottles)
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
