{-# LANGUAGE FlexibleContexts #-}

module Bottles.Parse
  ( getPour
  ) where

import Bottles.Model (Pour(..), GameError(..))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

parsePour :: MonadError GameError m => String -> m Pour
parsePour line = case splitOn "->" line of
  [from, to] -> do
    fromBottle <- case readMaybe from of
      Nothing -> throwError (InvalidBottleId from)
      Just a -> pure a
    toBottle <- case readMaybe to of
      Nothing -> throwError (InvalidBottleId to)
      Just a -> pure a
    pure (Pour fromBottle toBottle)
  _ -> throwError (InvalidInput line)

getPour :: MonadError GameError m => [Pour] -> String -> m Pour
getPour validPours line = do
  pour <- parsePour line
  if elem pour validPours
  then pure pour
  else throwError (InvalidPour pour)
