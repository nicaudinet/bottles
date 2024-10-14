module Bottles.Model
  ( Color (..)
  , BottleId
  , Bottle
  , Bottles
  , Pour (..)
  , GameError (..)
  , PuzzleSize (..)
  ) where

import qualified Data.Map as M

data Color
  = Yellow
  | White
  | Red
  | LightBlue
  | LightGreen
  | Pink
  | Brown
  | DarkGreen
  | Orange
  | DarkBlue
  | DarkRed
  deriving (Show, Eq, Ord, Enum)

type Bottle = [Color]
type BottleId = Int
type Bottles = M.Map BottleId Bottle

data Pour = Pour BottleId BottleId
  deriving Eq

data GameError
  = InvalidPuzzleType String
  | InvalidBottleId String
  | InvalidInput String
  | InvalidPour Pour
  | BottleNotFound BottleId
  | FromAndToAreTheSame BottleId
  | FromBottleIsEmpty BottleId
  | ToBottleIsTooFull BottleId
  | ColorsDontMatch Color Color
  | NoOpAction

instance Show GameError where
  show (InvalidPuzzleType puzzleType) =
    "Invalid puzzle type " <> show puzzleType <> ". Try again."
  show (InvalidBottleId bid) =
    "Invalid bottle id " <> show bid <> ". Try again."
  show (InvalidInput input) =
    "Invalid input " <> show input <> ". Try again."
  show (InvalidPour (Pour from to)) =
    "Invalid pour from " <> show from <> " to " <> show to <> ". Try again."
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
  show NoOpAction =
    "Action does nothing useful"

data PuzzleSize = Small | Medium | Large
