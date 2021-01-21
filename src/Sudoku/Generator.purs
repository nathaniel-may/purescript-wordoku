module Sudoku.Generator 
    ( module Exports
    , generate
    , randomWord
    ) 
where

import Prelude

import Data.Array (foldl, index, length, zip)
import Data.Either (hush)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Effect (Effect)
import Effect.Random (randomInt)
import Sudoku.Internal (Variant(..), diagonalOf, gridString, readNumberGrid)
import Sudoku.Internal.Generator (Game(..), Opts, colorChars, generateSudoku, numChars)
import Sudoku.Internal.Solver as Internal
import Sudoku.Wordlist (wordlist)

import Sudoku.Internal (emptySudoku) as Exports
import Sudoku.Internal.Generator (Difficulty(..), Game(..), Opts) as Exports
-- import Sudoku.Internal.Solver (Cell(..), CellSet(..), Grid, SearchResult(..), Variant(..), gridString, numbers, readGrid, readNumberGrid, replace2D, solve, solveUnique)
-- import Sudoku.Wordlist (wordlist)

generate :: Opts -> Effect String
generate opts = case opts.values of
    Sudoku  -> game opts
    Colorku -> mapValues (Map.fromFoldable $ numChars `zip` colorChars) <$> game opts
    Wordoku -> do
        g <- game opts
        w <- randomWord unit
        pure $ mapValues (wordMap w g) g
    where

    game :: Opts -> Effect String
    game opts = generateSudoku opts.variant opts.difficulty

    wordMap :: String -> String -> Map Char Char
    wordMap word sudoku = Map.fromFoldable $ toCharArray (diagonalOf solved) `zip` toCharArray word
        where solved = fromMaybe sudoku $ map gridString $ Internal.solve UniqueDiagonal <<< fromMaybe [] <<< hush <<< readNumberGrid $ sudoku

    -- keys become values
    mapValues :: Map Char Char -> String -> String
    mapValues m str = foldl 
        (\s c -> s <> (singleton <<< fromMaybe '.' $ Map.lookup c m))
        ""
        (toCharArray str)

randomWord :: Unit -> Effect String
randomWord _ = fromMaybe "" -- random access won't fail
    <<< index wordlist 
    <$> randomInt 0 (length wordlist - 1)
