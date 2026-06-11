module Test.RegressionTests where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe, isJust)
import Sudoku (Variant(..))
import Sudoku.Internal (cellSetFromPuzzle)
import Sudoku.Internal.Grid (Grid, readGrid)
import Sudoku.Internal.Solver as Internal
import Test.QuickCheck (Result, (<?>))

regressionTests :: Array Result
regressionTests =
  [ -- solveWordoku returned Nothing for this puzzle due to a bug in the solver
    let puzzle = "snbga......ro.bg......srnb.r..n....bg.i.baonr.b...o..gi.n.....s.o..gsi..agsirn.o."
    in isJust (solveWordoku UniqueDiagonal puzzle) <?> "solveWordoku returned Nothing for known puzzle"
  ]

solveWordoku :: Variant -> String -> Maybe Grid
solveWordoku v str = (Internal.solve v)
  =<< (hush <<< flip readGrid str)
  =<< (hush $ cellSetFromPuzzle str)
