module Sudoku.Internal.Generator where

import Prelude

import Data.Array (foldl, index, length, uncons, zip, (..))
import Data.Either (hush)
import Data.Enum (class Enum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (singleton, toCharArray)
import Effect (Effect)
import Effect.Random (randomInt)
import Sudoku.Internal (Cell(..), CellSet(..), SearchResult(..), Variant(..), diagonalOf, emptySudoku, numbers, randomArray)
import Sudoku.Internal.Grid (Grid, gridString, readGrid, readNumberGrid, replace2D)
import Sudoku.Internal.Solver (solve, solveUnique)
import Sudoku.Wordlist (wordlist)

type Opts =
  { variant :: Variant
  , values :: Game
  , difficulty :: Difficulty
  }

data Game = Sudoku | Wordoku | Colorku

derive instance valuesEq :: Eq Game
derive instance valuesOrd :: Ord Game
instance valuesEnum :: Enum Game where
  succ Sudoku = Just Wordoku
  succ Wordoku = Just Colorku
  succ Colorku = Nothing
  pred Sudoku = Nothing
  pred Wordoku = Just Sudoku
  pred Colorku = Just Wordoku

instance showValues :: Show Game where
  show Sudoku = "Sudoku"
  show Wordoku = "Wordoku"
  show Colorku = "Colorku"

data Difficulty = Beginner | Casual | Tricky | Difficult | Challenge

derive instance difficultyEq :: Eq Difficulty
derive instance difficultyOrd :: Ord Difficulty
instance difficultyEnum :: Enum Difficulty where
  succ Beginner = Just Casual
  succ Casual = Just Tricky
  succ Tricky = Just Difficult
  succ Difficult = Just Challenge
  succ Challenge = Nothing
  pred Beginner = Nothing
  pred Casual = Just Beginner
  pred Tricky = Just Casual
  pred Difficult = Just Tricky
  pred Challenge = Just Difficult

instance showDifficulty :: Show Difficulty where
  show Beginner = "Beginner"
  show Casual = "Casual"
  show Tricky = "Tricky"
  show Difficult = "Difficult"
  show Challenge = "Challenge"

generate :: Opts -> Effect String
generate opts = case opts.values of
  Sudoku -> game opts
  Colorku -> mapValues (Map.fromFoldable $ numChars `zip` colorChars) <$> game opts
  Wordoku -> do
    g <- game opts
    w <- randomWord unit
    pure $ mapValues (wordMap w g) g
  where

  game :: Opts -> Effect String
  game opts' = generateSudoku opts'.variant opts'.difficulty

  wordMap :: String -> String -> Map Char Char
  wordMap word sudoku = Map.fromFoldable $ toCharArray (diagonalOf solved) `zip` toCharArray word
    where
    solved = fromMaybe sudoku $ gridString <$> (solve UniqueDiagonal =<< hush (readNumberGrid sudoku))

  -- keys become values
  mapValues :: Map Char Char -> String -> String
  mapValues m str = foldl
    (\s c -> s <> (singleton <<< fromMaybe '.' $ Map.lookup c m))
    ""
    (toCharArray str)

generateSudoku :: Variant -> Difficulty -> Effect String
generateSudoku restrictDiag difficulty = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
  cellSet <- mixCellSet numbers
  let mFilled = solve restrictDiag =<< (hush $ readGrid cellSet emptySudoku)
  randIdxs <- randomArray (0 .. 80)
  pure $ (reduceBy cellSet (81 - diffNum restrictDiag difficulty) randIdxs) =<< mFilled
  where
  reduceBy :: CellSet -> Int -> Array Int -> Grid -> Maybe Grid
  reduceBy cs count idxs grid =
    if (count <= 0) then Just grid
    else do
      { head: idx, tail: rands } <- uncons idxs
      let nextGrid = removeAt cs idx grid
      case solveUnique restrictDiag nextGrid of
        Unique _ ->
          case reduceBy cs (count - 1) rands nextGrid of
            Nothing -> reduceBy cs count rands grid
            Just grid' -> Just grid'
        _ -> reduceBy cs count rands grid

  removeAt :: CellSet -> Int -> Grid -> Grid
  removeAt (CellSet _ allValues) idx grid =
    replace2D idx (Possible allValues) grid

  toStringOrLoop :: Maybe Grid -> Effect String
  toStringOrLoop Nothing = generateSudoku restrictDiag difficulty
  toStringOrLoop (Just grid) = pure $ gridString grid

mixCellSet :: CellSet -> Effect CellSet
mixCellSet (CellSet empty xs) = map (CellSet empty) (randomArray xs)

numChars :: Array Char
numChars = [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

colorChars :: Array Char
colorChars = [ 'R', 'O', 'Y', 'L', 'G', 'B', 'I', 'P', 'V' ]

randomWord :: Unit -> Effect String
randomWord _ =
  fromMaybe "" -- random access won't fail

    <<< index wordlist
    <$> randomInt 0 (length wordlist - 1)

diffNum :: Variant -> Difficulty -> Int
diffNum _ Beginner = 40
diffNum _ Casual = 34
diffNum _ Tricky = 30
diffNum _ Difficult = 26
-- the UniqueDiagonal variant restricts the search space enough to make
-- the constraints that puzzles must have unique solutions _easier_ to find
-- than a is the case in a Standard puzzle.
diffNum UniqueDiagonal Challenge = 22
-- the Standard variant requires significantly more computation to find unique
-- solutions at smaller clue numbers, so we slightly increase the number
-- of clues to improve runtimes
diffNum Standard Challenge = 24
