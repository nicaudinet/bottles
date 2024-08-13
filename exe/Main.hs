{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (when)
import Control.Monad.State
import Control.Monad.Except
import Data.List (group)
import qualified Data.Map as M
import Text.Read (readMaybe)

data Color
  = Yellow
  | LightBlue
  | DarkBlue
  | Brown
  | LightGreen
  | DarkGreen
  | Pink
  | Beige
  | Red
  | Orange
  deriving (Show, Eq, Ord, Enum)

type Bottle = [Color]
type BottleId = Int

type GameState = M.Map BottleId Bottle 

data GameError
  = InvalidInput [String]
  | InvalidBottleId String
  | BottleNotFound BottleId
  | FromBottleIsEmpty BottleId
  | ToBottleIsTooFull BottleId
  | ColorsDontMatch Color Color

type Game a = ExceptT GameError (StateT GameState IO) a

data Action = Pour BottleId BottleId


headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getUserInput :: Game (String, String)
getUserInput = do
  line <- liftIO getLine
  case words line of
    [x, y] -> pure (x, y)
    e -> throwError (InvalidInput e)

readBottleId :: String -> Game BottleId
readBottleId str =
  case readMaybe str of
    Just bottleId -> pure bottleId
    Nothing -> throwError (InvalidBottleId str)

getBottle :: BottleId -> Game Bottle
getBottle bottleId = do
  bottles <- get
  case M.lookup bottleId bottles of
    Just bottle -> pure bottle
    Nothing -> throwError (BottleNotFound bottleId)

pour :: BottleId -> BottleId -> Game ()
pour from to = do
  -- Get the two bottles
  fromBottle <- getBottle from
  toBottle <- getBottle to
  -- Check there are colors in the from bottle
  (fromHead, fromTail) <- case group fromBottle of
    [] -> throwError (FromBottleIsEmpty from)
    (x:xs) -> pure (x, concat xs)
  -- Check there's space in the to bottle
  when (length toBottle > 4 - length fromHead) $
    throwError (ToBottleIsTooFull to)
  -- Check the colors match
  let fromColor = head fromHead
  case headMaybe toBottle of
    Nothing -> pure ()
    Just toColor ->
      when (fromColor /= toColor) $
        throwError (ColorsDontMatch fromColor toColor)
  -- Pour a color from one bottle to the other
  modify (M.insert from fromTail)
  modify (M.insert to (fromHead <> toBottle))

showBottle :: BottleId -> Bottle -> String
showBottle bid bottle = show bid <> ": " <> show (map fromEnum bottle)

showBottles :: GameState -> String
showBottles bottles = unlines $
     ["---"]
  <> map (uncurry showBottle) (M.toList bottles)
  <> ["---"]

getAction :: Game Action
getAction = do
  (x, y) <- getUserInput
  from <- readBottleId x
  to <- readBottleId y
  pure (Pour from to)
  
update :: Action -> Game ()
update (Pour from to) = pour from to

handleGameError :: GameError -> Game ()
handleGameError (InvalidInput input) = liftIO $ putStrLn $
  "Invalid input " <> show input <> ". Try again."
handleGameError (InvalidBottleId bottleId) = liftIO $ putStrLn $
  "Invalid bottle id: " <> bottleId
handleGameError (BottleNotFound bid) = liftIO $ putStrLn $
  "Bottle not found: " <> show bid
handleGameError (FromBottleIsEmpty bottleId) = liftIO $ putStrLn $
  "Bottle " <> show bottleId <> " is empty, there's nothing to pour"
handleGameError (ToBottleIsTooFull bottleId) = liftIO $ putStrLn $
  "Bottle " <> show bottleId <> " is too full"
handleGameError (ColorsDontMatch c1 c2) = liftIO $ putStrLn $
  "Colors " <> show c1 <> " and " <> show c2 <> " don't match"

step :: Game ()
step = handleError handleGameError $ do
  bottles <- get
  liftIO (putStrLn (showBottles bottles))
  action <- getAction
  update action

runGame :: GameState -> Game () -> IO ()
runGame initState game = do
  bottles <- execStateT (runExceptT game) initState
  putStrLn (showBottles bottles)
  putStrLn "You win!"

exampleEasy :: GameState
exampleEasy = M.fromList . zip [0..] $
  [ [ Red, Red, Red, Yellow ]
  , [ Yellow, Yellow, Red, Yellow ]
  , []
  , []
  ]

exampleHard :: GameState
exampleHard = M.fromList . zip [0..] $
  [ [ Yellow, DarkBlue, Brown, LightGreen ]
  , [ Pink, LightGreen, Brown, LightBlue ]
  , [ LightGreen, Beige, Pink, LightBlue ]
  , [ Red, DarkBlue, Red, LightBlue ]
  , [ Beige, Brown, Beige, Orange ]
  , [ DarkGreen, LightBlue, DarkGreen, Orange ]
  , [ LightGreen, Yellow, DarkGreen, Orange ]
  , [ Beige, Pink, Pink, DarkBlue ]
  , [ Yellow, DarkBlue, Red, Red ]
  , [ DarkGreen, Brown, Orange, Yellow ]
  , []
  , []
  ]

colorCounts :: GameState -> M.Map Color Int
colorCounts = M.unionsWith (+) . map (M.fromListWith (+) . map (\c -> (c, 1))) . M.elems

validBottle :: Bottle -> Bool
validBottle [] = True
validBottle [a,b,c,d] = (a == b) && (b == c) && (c == d)
validBottle _ = False

gameOver :: Game Bool
gameOver = do
  bottles <- get
  pure $ all validBottle (M.elems bottles)

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action

main :: IO ()
main = do runGame exampleEasy (untilM gameOver step)
