{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bottles.Parse
  ( getAction
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Control.Monad.State (get)
import qualified Data.Map as M
import Text.Read (readMaybe)

import Bottles.Types (Action(..), GameState(..), GameError(..), Game)

getAction :: Game Action
getAction = do
  state <- get
  line <- liftIO getLine
  actionId <- case readMaybe line of
    Nothing -> throwError (InvalidInput line)
    Just actionId -> pure actionId
  case M.lookup actionId state.actions of
    Nothing -> throwError (ActionNotFound actionId)
    Just action -> pure action
