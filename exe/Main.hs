module Main where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

import Bottles.Types (Color(..), Bottle, Bottles, Game)
import Bottles.View (showBottles, showGame)
import Bottles.Parse (getAction)
import Bottles.Update (update)

-------------
-- Puzzles --
-------------

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

step :: Game ()
step = handleError (liftIO . print) $ do
  showGame
  action <- getAction
  update action

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a,b,c,d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Game Bool
gameOver = gets (all validBottle . M.elems)

runGame :: Bottles -> Game () -> IO ()
runGame initBottles game = do
  bottles <- execStateT (runExceptT game) initBottles
  putStrLn (showBottles bottles)
  putStrLn "You win!"

choosePuzzle :: String -> Bottles
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
  putStrLn "What puzzle do you want? (easy/hard) "
  puzzle <- choosePuzzle <$> getLine
  runGame puzzle (untilM gameOver step)
