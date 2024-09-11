{-# LANGUAGE FlexibleContexts #-}

module Bottles.Parse
  ( getAction
  ) where

import Bottles.Types (Pour(..), Action(..), GameError(..))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

parseAction :: MonadError GameError m => String -> m Action
parseAction "B" = pure Backtrack
parseAction line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- case readMaybe from of
      Nothing -> throwError (InvalidBottleId from)
      Just a -> pure a
    toBottle <- case readMaybe to of
      Nothing -> throwError (InvalidBottleId to)
      Just a -> pure a
    pure (Move (Pour fromBottle toBottle))
  _ -> throwError (InvalidInput line)

getAction :: MonadError GameError m => [Pour] -> String -> m Action
getAction validPours line = do
  action <- parseAction line
  case action of
    Backtrack -> pure action
    Move pour ->
      if elem pour validPours
      then pure action
      else throwError (InvalidAction action)
