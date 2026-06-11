module Test.RegressionTests where

import Prelude

import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Sudoku (Variant(..))
import Sudoku.Internal (diagonalString)
import Sudoku.Internal.Grid (gridString, readGrid)
import Sudoku.Internal.Key (mkKey)
import Sudoku.Internal.Solver as Internal
import Test.QuickCheck (Result, (<?>))

regressionTests :: Array Result
regressionTests =
  [ -- 'd' does not appear in any clue position of this puzzle, which previously caused
    -- cellSetFromPuzzle to produce a Left (only 8 chars found), silently becoming an
    -- empty grid via fromMaybe []. The diagonal of the solved puzzle must be the key word.
    let
      word = "signboard"
      puzzle = "snbga......ro.bg......srnb.r..n....bg.i.baonr.b...o..gi.n.....s.o..gsi..agsirn.o."
      key = hush $ mkKey word
      diag = do
        k <- key
        grid <- hush $ readGrid k puzzle
        solved <- Internal.solve UniqueDiagonal grid
        pure $ diagonalString $ gridString k solved
    in
      diag == Just word <?> "solveWordoku diagonal mismatch for known puzzle"
  ]
