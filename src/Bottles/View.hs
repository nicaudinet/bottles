{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Bottles.View
  ( showBottles
  , showGame
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get)
import Data.List (intercalate)
import qualified Data.Map as M

import Bottles.Types (Color(..), BottleId, Bottle, Bottles, ActionId, Action(..), Actions, GameState(..), Game)

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

showAction :: ActionId -> Action -> String
showAction aid (Pour from to) = concat
  [ if aid < 10 then " " else ""
  , show aid
  , ": "
  , show from
  , " -> "
  , show to
  ]

showActions :: Actions -> String
showActions = intercalate "\n" . map (uncurry showAction) . M.toList

showGame :: Game ()
showGame = do
  state <- get
  liftIO $ do
    putStrLn "---"
    putStrLn (showBottles state.bottles)
    putStrLn "---"
    putStrLn (showActions state.actions)
    putStrLn "---"
