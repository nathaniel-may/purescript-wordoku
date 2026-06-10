module Test.RegressionTests where

import Prelude

import Data.Array (null)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Sudoku (Variant(..))
import Sudoku.Internal (Grid, cellSetFromPuzzle, readGrid)
import Sudoku.Internal.Solver as Internal
import Test.QuickCheck (Result, (<?>))

regressionTests :: Array Result
regressionTests =
  [ -- solveWordoku returned [] for this puzzle due to a bug in the solver
    let puzzle = "snbga......ro.bg......srnb.r..n....bg.i.baonr.b...o..gi.n.....s.o..gsi..agsirn.o."
    in (not $ null $ solveWordoku UniqueDiagonal puzzle) <?> "solveWordoku returned empty grid for known puzzle"
  ]

solveWordoku :: Variant -> String -> Grid
solveWordoku v str = fromMaybe [] $ (Internal.solve v)
  =<< (hush <<< flip readGrid str)
  =<< (hush $ cellSetFromPuzzle str)
