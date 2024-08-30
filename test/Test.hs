module Main (main) where

-------------
-- Puzzles --
-------------

puzzleTrivial :: Bottles
puzzleTrivial = M.fromList . zip [0..] $
  [ [ Red, Yellow, Yellow, Yellow ]
  , [ Yellow, Red, Red, Red ]
  , []
  , []
  ]

puzzleEasy :: Bottles
puzzleEasy = M.fromList . zip [0..] $
  [ [ Red, Red, Red, Yellow ]
  , [ Yellow, Yellow, Red, Yellow ]
  , []
  , []
  ]

puzzleHard :: Bottles
puzzleHard = M.fromList . zip [0..] $
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

puzzleImpossible :: Bottles
puzzleImpossible = M.fromList . zip [0..] $
  [ [ Red, White, LightBlue, LightGreen ]
  , [ LightGreen, Red, White, LightBlue ]
  , [ LightBlue, LightGreen, Red, White ]
  , [ White, LightBlue, LightGreen, Red ]
  , []
  , []
  ]


main :: IO ()
main = putStrLn "Test suite not yet implemented."
