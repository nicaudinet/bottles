module Bottles.Types
  ( Color(..)
  , BottleId
  , Bottle
  , Bottles
  , GameError(..)
  , Game
  , Action(..)
  ) where

import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M

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
  = InvalidPuzzleType String
  | InvalidInput [String]
  | InvalidBottleId String
  | BottleNotFound BottleId
  | FromAndToAreTheSame BottleId
  | FromBottleIsEmpty BottleId
  | ToBottleIsTooFull BottleId
  | ColorsDontMatch Color Color

instance Show GameError where
  show (InvalidPuzzleType puzzleType) =
    "Invalid puzzle type " <> show puzzleType <> ". Try again."
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
