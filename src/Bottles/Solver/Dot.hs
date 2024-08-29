module Bottles.Solver.Dot
  ( toDotString
  , printDot
  ) where

import Bottles.Solver (Backtrack(..), SolverState(..))
import Bottles.Types (Color)
import Control.Monad.State (State, get, put, execState)
import qualified Data.Map as M
import System.Process (callProcess)

type NodeId = Int
type Nodes = M.Map NodeId String
type Edges = M.Map NodeId NodeId
data DotState = DotState
  { nodes :: Nodes
  , edges :: Edges
  , count :: NodeId
  }
  deriving Show
type Dot a = State DotState a
type ComputationTree = Backtrack SolverState

addNode :: String -> Dot NodeId
addNode label = do
  s <- get
  let n = count s
  put $ DotState
    { nodes = M.insert n label (nodes s)
    , edges = edges s
    , count = n + 1
    }
  pure n

addEdge :: NodeId -> NodeId -> Dot ()
addEdge from to = do
  s <- get
  put $ DotState
    { nodes = nodes s
    , edges = M.insert to from (edges s)
    , count = count s
    }

showBottle :: [Color] -> String
showBottle [] = "_"
showBottle (c:_) = [head (show c)]

showSolverState :: SolverState -> String
showSolverState = concatMap showBottle . M.elems . bottles

toDot :: ComputationTree -> Dot NodeId
toDot Fail = addNode "Fail"
toDot (Return a) = addNode (showSolverState a)
toDot (a :|: b) = do
  parent <- addNode ":|:"
  left <- toDot a
  right <- toDot b
  addEdge parent left
  addEdge parent right
  pure parent

nodeToString :: NodeId -> String -> String
nodeToString n label = show n ++ " [label=\"" ++ label ++ "\"];"

edgeToString :: NodeId -> NodeId -> String
edgeToString to from = show from ++ " -> " ++ show to ++ ";"

dotStateToString :: DotState -> String
dotStateToString s = unlines
  [ "digraph G {"
  , unlines . M.elems . M.mapWithKey nodeToString . nodes $ s
  , unlines . M.elems . M.mapWithKey edgeToString . edges $ s
  , "}"
  ]

toDotString :: ComputationTree -> String
toDotString = dotStateToString . flip execState initDotState . toDot
  where initDotState = (DotState M.empty M.empty 0)

printDot :: FilePath -> ComputationTree -> IO FilePath
printDot fp cmpTree = do
  let dotFile = fp <> ".dot"
      pngFile = fp <> ".svg"
  writeFile dotFile (toDotString cmpTree)
  callProcess "dot" ["-Tsvg", "-o", pngFile, dotFile]
  pure pngFile
