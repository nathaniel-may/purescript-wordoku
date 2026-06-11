module Sudoku.Generator
  ( module Exports
  , generate
  , generateWithWorkers
  , randomWord
  ) where

import Prelude

import Data.Array (foldl, index, length, zip)
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Sudoku.Internal (Variant(..), diagonalOf)
import Sudoku.Internal.Grid (gridString, readNumberGrid)
import Sudoku.Internal.Key (sudokuKey)
import Sudoku.Internal.Solver as Internal
import Sudoku.Wordlist (wordlist)
import Sudoku.Workers (WorkerPool, raceGenerateSudoku)

import Sudoku.Encoding (DecodedKey(..), keyToString)
import Sudoku.Internal (emptySudoku) as Exports
import Sudoku.Internal.Generator (Difficulty(..), Game(..), Opts, generateSudoku, numChars) as Exports
import Sudoku.Internal.Generator (Game(..), Opts, numChars)

generate :: Opts -> Effect { puzzle :: String, key :: DecodedKey }
generate opts = case opts.values of
  Sudoku -> do
    p <- game opts
    pure { puzzle: p, key: SudokuKey }
  Colorku -> do
    p <- game opts
    let
      dk = ColorkuKey
      p' = mapValues (Map.fromFoldable $ numChars `zip` toCharArray (keyToString dk)) p
    pure { puzzle: p', key: dk }
  Wordoku -> do
    g <- game opts
    w <- randomWord unit
    let
      dk = WordokuKey w
      p = mapValues (wordMap w g) g
    pure { puzzle: p, key: dk }
  where
  game :: Opts -> Effect String
  game o = Exports.generateSudoku o.variant o.difficulty

generateWithWorkers :: WorkerPool -> Int -> Opts -> Aff { puzzle :: String, key :: DecodedKey }
generateWithWorkers pool numWorkers opts = case opts.values of
  Exports.Sudoku -> do
    p <- raceGenerateSudoku pool numWorkers opts.variant opts.difficulty
    pure { puzzle: p, key: SudokuKey }
  Exports.Colorku -> do
    p <- raceGenerateSudoku pool numWorkers opts.variant opts.difficulty
    let
      dk = ColorkuKey
      p' = mapValues (Map.fromFoldable $ Exports.numChars `zip` toCharArray (keyToString dk)) p
    pure { puzzle: p', key: dk }
  Exports.Wordoku -> do
    p <- raceGenerateSudoku pool numWorkers opts.variant opts.difficulty
    w <- liftEffect $ randomWord unit
    let
      dk = WordokuKey w
      p' = mapValues (wordMap w p) p
    pure { puzzle: p', key: dk }

wordMap :: String -> String -> Map Char Char
wordMap word sudoku = Map.fromFoldable $ toCharArray (diagonalOf solved) `zip` toCharArray word
  where
  solved = fromMaybe sudoku $ (gridString sudokuKey) <$> (Internal.solve UniqueDiagonal =<< hush (readNumberGrid sudoku))

-- keys become values
mapValues :: Map Char Char -> String -> String
mapValues m str = foldl
  (\s c -> s <> (singleton <<< fromMaybe '.' $ Map.lookup c m))
  ""
  (toCharArray str)

randomWord :: Unit -> Effect String
randomWord _ =
  fromMaybe "" -- random access won't fail

    <<< index wordlist
    <$> randomInt 0 (length wordlist - 1)
