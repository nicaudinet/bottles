{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Bottles.Types (Color(..), Bottles, Action, GameState(..), Game)
import Bottles.View (showBottles, showGame)
import Bottles.Parse (getAction)
import Bottles.Update (availableActions, play, gameOver)
import Bottles.Solver (solver, allSolutions)

-------------
-- Puzzles --
-------------

exampleTrivial :: Bottles
exampleTrivial = M.fromList . zip [0..] $
  [ [ Red, Yellow, Yellow, Yellow ]
  , [ Yellow, Red, Red, Red ]
  , []
  ]

exampleEasy :: Bottles
exampleEasy = M.fromList . zip [0..] $
  [ [ Red, Red, Red, Yellow ]
  , [ Yellow, Yellow, Red, Yellow ]
  , []
  , []
  ]

exampleHard :: Bottles
exampleHard = M.fromList . zip [0..] $
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
choosePuzzle "trivial" = exampleTrivial
choosePuzzle "easy" = exampleEasy
choosePuzzle "hard" = exampleHard
choosePuzzle _ = error "Invalid puzzle type"

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action

main :: IO ()
main = do
  putStrLn "What puzzle do you want? (trivial/easy/hard) "
  puzzle <- choosePuzzle <$> getLine
  let as = availableActions puzzle
  let initState = GameState { bottles = puzzle, actions = as }
  runGame initState (untilM (gets gameOver) step)
  putStrLn "All possible solutions:"
  let solutions = allSolutions (solver initState)
  print (length solutions)
  mapM_ (putStrLn . showBottles . bottles) solutions
