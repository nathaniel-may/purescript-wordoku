module Test.DisplayTests where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafeCrashWith)
import Sudoku.Display (addClueButtonDisabled, applyClues, clueOrder, displayedPuzzleString)
import Sudoku.Encoding (DecodedKey(..), keyToString)
import Sudoku.Internal.Grid (Grid, readGrid)
import Sudoku.Internal.Key (Key, mkKey, sudokuKey)
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

-- A Wordoku-keyed (9 distinct lowercase letters) fixture, used to verify
-- displayedPuzzleString correctly parses/renders the cached Grid using the
-- puzzle's *own* key rather than assuming a digit-keyed Grid (this is
-- exactly the bug this regression test guards against).
wordokuKey :: DecodedKey
wordokuKey = WordokuKey "countries"

wordokuKeyValue :: Key
wordokuKeyValue = case mkKey (keyToString wordokuKey) of
  Right k -> k
  Left err -> unsafeCrashWith ("DisplayTests wordokuKey fixture is broken: " <> err)

-- This is `fixtureSolvedString` (above) with every digit relabeled to the
-- corresponding letter of "countries" by position (`1`->`c`, `2`->`o`, ...,
-- `9`->`s`) -- a uniform value relabeling that preserves the
-- one-of-each-value-per-row/column/box property, so this is guaranteed to
-- be a validly solved Wordoku grid given `fixtureSolvedString` is valid.
wordokuSolvedString :: String
wordokuSolvedString = "ncteiusroeurcosnitsiotrnceucensutioriruneotcstosicreunrtiuneoscuneoscrtioscrtiune"

wordokuSolvedGrid :: Grid
wordokuSolvedGrid = case readGrid wordokuKeyValue wordokuSolvedString of
  Right g -> g
  Left err -> unsafeCrashWith ("DisplayTests wordokuSolvedGrid fixture is broken: " <> err)

displayTests :: Array Result
displayTests =
  [ (displayedPuzzleString SudokuKey false puzzleString Nothing == puzzleString)
      <?> "displayedPuzzleString: showing clues with no solution should return the puzzle unchanged"

  , (displayedPuzzleString SudokuKey false puzzleString (Just fixtureSolvedGrid) == puzzleString)
      <?> "displayedPuzzleString: showing clues always returns the puzzle, even once a solution exists"

  , (displayedPuzzleString SudokuKey true puzzleString (Just fixtureSolvedGrid) == fixtureSolvedString)
      <?> "displayedPuzzleString: showing solution should render the cached grid via gridString with the puzzle's own key"

  , (displayedPuzzleString SudokuKey true puzzleString Nothing == puzzleString)
      <?> "displayedPuzzleString: showing solution with no solution yet (in-flight or failed) falls back to the puzzle"

  , (displayedPuzzleString wordokuKey true "anything" (Just wordokuSolvedGrid) == wordokuSolvedString)
      <?> "displayedPuzzleString: a non-digit (Wordoku) key parses/renders the cached grid using that same key, not sudokuKey"

  , (clueOrder puzzleString == clueOrder puzzleString)
      <?> "clueOrder: calling it twice with the same puzzle yields the same ordering"

  , (Array.sort (clueOrder puzzleString) == Array.sort blankIdxs)
      <?> "clueOrder: returns exactly the blank indices of the input"

  , (Array.nub (clueOrder puzzleString) == clueOrder puzzleString && Array.length (clueOrder puzzleString) == Array.length blankIdxs)
      <?> "clueOrder: is a permutation with no duplicates"

  , (applyClues SudokuKey 0 puzzleString fixtureSolvedGrid == puzzleString)
      <?> "applyClues: revealing 0 clues leaves the puzzle unchanged"

  , (applyClues SudokuKey blankCount puzzleString fixtureSolvedGrid == fixtureSolvedString)
      <?> "applyClues: revealing every blank equals the full solution string"

  , (countDiffs puzzleString (applyClues SudokuKey 1 puzzleString fixtureSolvedGrid) == 1)
      <?> "applyClues: revealing 1 clue differs from the puzzle in exactly one character position"

  , (applyClues SudokuKey 9999 puzzleString fixtureSolvedGrid == applyClues SudokuKey blankCount puzzleString fixtureSolvedGrid)
      <?> "applyClues: out-of-range high n clamps to blankCount"

  , (applyClues SudokuKey (-5) puzzleString fixtureSolvedGrid == puzzleString)
      <?> "applyClues: negative n clamps to 0"

  , (applyClues SudokuKey 1 puzzleString fixtureSolvedGrid == applyClues SudokuKey 1 puzzleString fixtureSolvedGrid)
      <?> "applyClues: deterministic across repeated calls with the same arguments"

  , (addClueButtonDisabled Nothing 0 puzzleString == true)
      <?> "addClueButtonDisabled: no solution cached yet"

  , (addClueButtonDisabled (Just fixtureSolvedGrid) 0 puzzleString == false)
      <?> "addClueButtonDisabled: clues remain to reveal"

  , (addClueButtonDisabled (Just fixtureSolvedGrid) blankCount puzzleString == true)
      <?> "addClueButtonDisabled: all clues already revealed"

  , (applyClues wordokuKey wordokuBlankCount wordokuAllBlankPuzzle wordokuSolvedGrid == wordokuSolvedString)
      <?> "applyClues: a non-digit (Wordoku) key works through applyClues too"
  ]
  where
  blankIdxs :: Array Int
  blankIdxs = Array.filter (\i -> Array.index (toCharArray puzzleString) i == Just '.') (Array.range 0 (Array.length (toCharArray puzzleString) - 1))

  blankCount :: Int
  blankCount = Array.length (Array.filter (_ == '.') (toCharArray puzzleString))

  countDiffs :: String -> String -> Int
  countDiffs a b = Array.length (Array.filter (\(Tuple x y) -> x /= y) (Array.zip (toCharArray a) (toCharArray b)))

  wordokuAllBlankPuzzle :: String
  wordokuAllBlankPuzzle = fromCharArray (Array.replicate (Array.length (toCharArray wordokuSolvedString)) '.')

  wordokuBlankCount :: Int
  wordokuBlankCount = Array.length (toCharArray wordokuAllBlankPuzzle)
