module Main where

import Control.Applicative (liftA2)
import Control.Monad (when, filterM)
import Control.Monad.State
import Control.Monad.Except
import Data.Either (isRight)
import Data.List (group, intercalate)
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
  | White
  | Red
  | Orange
  deriving (Show, Eq, Ord, Enum)

type Bottle = [Color]
type BottleId = Int

type Bottles = M.Map BottleId Bottle 

data GameError
  = InvalidInput [String]
  | InvalidBottleId String
  | BottleNotFound BottleId
  | FromAndToAreTheSame BottleId
  | FromBottleIsEmpty BottleId
  | ToBottleIsTooFull BottleId
  | ColorsDontMatch Color Color

instance Show GameError where
  show (InvalidInput input) =
    "Invalid input " <> show input <> ". Try again."
  show (InvalidBottleId bottleId) =
    "Invalid bottle id: " <> bottleId
  show (BottleNotFound bid) =
    "Bottle not found: " <> show bid
  show (FromAndToAreTheSame bottleId) =
    "The 'from' bottle cannot be the same as the 'to' bottle: " <> show bottleId
  show (FromBottleIsEmpty bottleId) =
    "Bottle " <> show bottleId <> " is empty, there's nothing to pour"
  show (ToBottleIsTooFull bottleId) =
    "Bottle " <> show bottleId <> " is too full"
  show (ColorsDontMatch c1 c2) =
    "Colors " <> show c1 <> " and " <> show c2 <> " don't match"


type Game a = ExceptT GameError (StateT Bottles IO) a

data Action = Pour BottleId BottleId

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

untilM :: Monad m => m Bool -> m a -> m ()
untilM cond action = do
  c <- cond
  if c
  then pure ()
  else action >> untilM cond action

-----------
-- Parse --
-----------

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

getAction :: Game Action
getAction = do
  (x, y) <- getUserInput
  from <- readBottleId x
  to <- readBottleId y
  pure (Pour from to)

sandbox :: MonadState s m => m a -> m a
sandbox action = do
  s <- get
  result <- action
  put s
  pure result

tryAction :: Action -> Game Bool
tryAction action = isRight <$> sandbox (tryError (update action))

availableActions :: Game [Action]
availableActions = do
  bottleIds <- gets M.keys
  filterM tryAction (liftA2 Pour bottleIds bottleIds)

------------
-- Update --
------------

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
  -- Check that from and to are different
  when (from == to) $
    throwError (FromAndToAreTheSame from)
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
  
update :: Action -> Game ()
update (Pour from to) = pour from to

----------
-- View --
----------

-- xterm-256color color codes
-- https://stackabuse.com/how-to-print-colored-text-in-python/
showColor :: Color -> String
showColor color = "\x1b[48;5;" <> show (colorCode color) <> "m  \x1b[0m"
  where
    colorCode :: Color -> Int
    colorCode Yellow = 220
    colorCode LightBlue = 44
    colorCode DarkBlue = 21
    colorCode Brown = 130
    colorCode LightGreen = 47
    colorCode DarkGreen = 22
    colorCode Pink = 201
    colorCode White = 255
    colorCode Red = 196
    colorCode Orange = 208

showBottle :: BottleId -> Bottle -> String
showBottle bid bottle = concat
  [ if bid < 10 then " " else ""
  , show bid
  , ": "
  , concat (replicate (4 - length bottle) "  ")
  , concatMap showColor bottle
  ]

showBottles :: Bottles -> String
showBottles = intercalate "\n\n" . map (uncurry showBottle) . M.toList

showAction :: (Int, Action) -> String
showAction (n, (Pour from to)) = concat
  [ if n < 10 then " " else ""
  , show n
  , ": "
  , show from
  , " -> "
  , show to
  ]

showActions :: [Action] -> String 
showActions = intercalate "\n" . map showAction . zip [0..]

showGame :: Game ()
showGame = do
  bottles <- get
  actions <- availableActions
  liftIO $ do
    putStrLn "---"
    putStrLn (showBottles bottles)
    putStrLn "---"
    putStrLn (showActions actions)
    putStrLn "---"

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
runGame initState game = do
  bottles <- execStateT (runExceptT game) initState
  putStrLn (showBottles bottles)
  putStrLn "You win!"

choosePuzzle :: String -> Bottles
choosePuzzle "easy" = exampleEasy
choosePuzzle "hard" = exampleHard
choosePuzzle _ = error "Invalid input"

main :: IO ()
main = do
  putStrLn "What puzzle do you want? (easy/hard) "
  puzzle <- choosePuzzle <$> getLine
  runGame puzzle (untilM gameOver step)
