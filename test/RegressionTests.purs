module Test.RegressionTests where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe, isJust)
import Sudoku (Variant(..))
import Sudoku.Internal.Grid (Grid, readGrid)
import Sudoku.Internal.Key (mkKey)
import Sudoku.Internal.Solver as Internal
import Test.QuickCheck (Result, (<?>))

regressionTests :: Array Result
regressionTests =
  [ -- solveWordoku returned Nothing for this puzzle due to a bug in cellSetFromPuzzle
    -- when a character appeared only in dot positions (not as a clue), the key had < 9 chars
    let
      puzzle = "snbga......ro.bg......srnb.r..n....bg.i.baonr.b...o..gi.n.....s.o..gsi..agsirn.o."
      key = hush $ mkKey "adsorbing"
    in isJust (solveWordoku key UniqueDiagonal puzzle) <?> "solveWordoku returned Nothing for known puzzle"
  ]

solveWordoku :: Maybe _ -> Variant -> String -> Maybe Grid
solveWordoku mKey v str = do
  key <- mKey
  grid <- hush $ readGrid key str
  Internal.solve v grid
