module Bottles.Parse
  ( availableActions
  , getAction
  ) where

import Control.Applicative (liftA2)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError, tryError)
import Control.Monad.State (MonadState, get, gets, put)
import Data.Either (isRight)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Types (BottleId, Action(..), GameError(..), Game)
import Bottles.Update (update)

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
