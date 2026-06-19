module Test.DisplayTests where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Sudoku.Display (displayedPuzzleString)
import Sudoku.Encoding (DecodedKey(..), denormalize)
import Sudoku.Internal.Grid (Grid, gridString, readGrid)
import Sudoku.Internal.Key (sudokuKey)
import Test.QuickCheck (Result, (<?>))

-- A known 81-character normalized (0-9/'.') *fully solved* grid string,
-- used only to construct a `Grid` fixture via readGrid -- the only way to
-- produce a `Grid` value (its constructor is private). This is the
-- documented solution to the example puzzle in `Sudoku.Solver`'s own
-- module-doc (`Sudoku.Solver.solve`'s usage example), reused here rather
-- than hand-deriving a fresh fixture.
puzzleString :: String
puzzleString = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"

fixtureSolvedString :: String
fixtureSolvedString = "415873962836129475972564183184935726763482519529716834657348291348291657291657348"

fixtureSolvedGrid :: Grid
fixtureSolvedGrid = case readGrid sudokuKey fixtureSolvedString of
  Right g -> g
  Left err -> unsafeCrashWith ("DisplayTests fixture is broken: " <> err)

displayTests :: Array Result
displayTests =
  [ (displayedPuzzleString SudokuKey false puzzleString Nothing == puzzleString)
      <?> "displayedPuzzleString: showing clues with no solution should return the puzzle unchanged"

  , (displayedPuzzleString SudokuKey false puzzleString (Just fixtureSolvedGrid) == puzzleString)
      <?> "displayedPuzzleString: showing clues always returns the puzzle, even once a solution exists"

  , (displayedPuzzleString SudokuKey true puzzleString (Just fixtureSolvedGrid) == fixtureSolvedString)
      <?> "displayedPuzzleString: showing solution should denormalize the cached grid via gridString+denormalize"

  , (displayedPuzzleString SudokuKey true puzzleString Nothing == puzzleString)
      <?> "displayedPuzzleString: showing solution with no solution yet (in-flight or failed) falls back to the puzzle"

  , let
      key = WordokuKey "countries"
    in
      ( displayedPuzzleString key true "anything" (Just fixtureSolvedGrid)
          == denormalize key (gridString sudokuKey fixtureSolvedGrid)
      )
        <?> "displayedPuzzleString: composition with a non-Sudoku key denormalizes correctly"
  ]
