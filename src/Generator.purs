module Generator where

import Prelude

import Data.Array (cons, deleteAt, drop, elem, index, foldl, length, replicate, take, uncons, zip, (..))
import Data.Enum (class Enum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Solver (Cell(..), CellSet(..), Grid, SearchResult(..), gridString, index2D, numbers, readGrid, readNumberGrid, replace2D, solve, solveUnique)
import Wordlist (wordlist)

type Opts = { 
      restrictDiag :: Boolean 
    , values       :: Game
    , difficulty   :: Difficulty }

data Game = Sudoku | Wordoku | Colorku
derive instance valuesEq :: Eq Game
derive instance valuesOrd :: Ord Game
instance valuesEnum :: Enum Game where
    succ Sudoku = Just Wordoku
    succ Wordoku    = Just Colorku
    succ Colorku  = Nothing
    pred Sudoku = Nothing
    pred Wordoku    = Just Sudoku
    pred Colorku  = Just Wordoku
instance showValues :: Show Game where
    show Sudoku  = "Sudoku"
    show Wordoku = "Wordoku"
    show Colorku = "Colorku"

data Difficulty = Beginner | Casual | Tricky | Difficult | Challenge
derive instance difficultyEq :: Eq Difficulty
derive instance difficultyOrd :: Ord Difficulty
instance difficultyEnum :: Enum Difficulty where
    succ Beginner  = Just Casual
    succ Casual    = Just Tricky
    succ Tricky    = Just Difficult
    succ Difficult = Just Challenge
    succ Challenge = Nothing
    pred Beginner  = Nothing
    pred Casual    = Just Beginner
    pred Tricky    = Just Casual
    pred Difficult = Just Tricky
    pred Challenge = Just Difficult
instance showDifficulty :: Show Difficulty where
    show Beginner  = "Beginner"
    show Casual    = "Casual"
    show Tricky    = "Tricky"
    show Difficult = "Difficult"
    show Challenge = "Challenge"

generate :: Opts -> Effect String
generate = generate' where

    generate' :: Opts -> Effect String
    generate' opts = case opts.values of
        Sudoku  -> game opts
        Colorku -> mapValues (Map.fromFoldable $ numChars `zip` colorChars) <$> game opts
        Wordoku -> do
            g <- game opts
            w <- randomWord unit
            pure $ mapValues (wordMap w g) g

    game :: Opts -> Effect String
    game opts = generateSudoku2 opts.restrictDiag opts.difficulty

    wordMap :: String -> String -> Map Char Char
    wordMap word sudoku = Map.fromFoldable $ toCharArray (diagonalOf solved) `zip` toCharArray word
        where solved = fromMaybe sudoku $ map gridString $ solve true <<< fromMaybe [] <<< readNumberGrid $ sudoku

    -- keys become values
    mapValues :: Map Char Char -> String -> String
    mapValues m str = foldl 
        (\s c -> s <> (singleton <<< fromMaybe '.' $ Map.lookup c m))
        ""
        (toCharArray str)

-- generateSudoku :: Boolean -> Difficulty -> Effect String
-- generateSudoku restrictDiag difficulty = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
--     cellSet <- mixCellSet numbers
--     let mFilled = solve restrictDiag =<< (readGrid cellSet emptySudoku)
--     randIdxs <- randomArray (0..80)
--     pure $ (reduceBy cellSet (81 - diffNum difficulty) randIdxs) =<< mFilled 
--     where
--         reduceBy :: CellSet -> Int -> Array Int -> Grid -> Maybe Grid
--         reduceBy cs count idxs grid = if count > 64 then Nothing else do
--             let result = solveUnique restrictDiag grid
--             { head: idx, tail: rands } <- uncons idxs
--             case result of
--                 -- check that the given grid has a unique solution. If it doesn't, backtracking won't help.
--                 Unique _ -> 
--                     if (count <= 0) 
--                     then Just grid
--                     -- try removing the next one
--                     else case reduceBy cs (count - 1) rands (removeAt cs idx grid) of
--                         -- backtrack
--                         Nothing -> reduceBy cs count rands grid
--                         Just grid' -> Just grid'
--                 -- backtracking won't help if the board doesn't already have a unique solution
--                 (NotUnique _ _) -> Nothing
--                 NoSolution -> Nothing

--         removeAt :: CellSet -> Int -> Grid -> Grid
--         removeAt (CellSet _ allValues) idx grid = 
--             replace2D idx (Possible allValues) grid
        
--         toStringOrLoop :: Maybe Grid -> Effect String
--         toStringOrLoop Nothing = generateSudoku restrictDiag difficulty
--         toStringOrLoop (Just grid) = pure $ gridString grid

generateSudoku2 :: Boolean -> Difficulty -> Effect String
generateSudoku2 restrictDiag difficulty = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
    cellSet <- mixCellSet numbers
    let mEmpty = readGrid cellSet emptySudoku
    let mSolved = solve restrictDiag =<< mEmpty
    randIdxs <- randomArray (0..80)
    pure $ do
        empty <- mEmpty
        solved <- mSolved
        keepOnly (diffNum difficulty) randIdxs empty solved
    where
        keepOnly :: Int -> Array Int -> Grid -> Grid -> Maybe Grid
        keepOnly count idxs empty solved = 
            if length idxs < count
            then Nothing
            else case solveUnique restrictDiag start of
                Unique grid -> Just start
                _ -> keepOnly count (drop 1 idxs) empty solved
            where 
                start :: Grid
                start = foldl (\grid idx -> keep idx grid solved) empty (take count idxs)

        keep :: Int -> Grid -> Grid -> Grid
        keep idx grid solved = fromMaybe grid $ do
            keeping <- index2D solved idx
            pure $ replace2D idx keeping grid
        
        toStringOrLoop :: Maybe Grid -> Effect String
        toStringOrLoop Nothing = generateSudoku2 restrictDiag difficulty
        toStringOrLoop (Just grid) = pure $ gridString grid

diagonalOf :: String -> String
diagonalOf str = foldl onlyDiag "" $ toCharArray str `zip` (0..80) where

    onlyDiag :: String -> Tuple Char Int -> String
    onlyDiag s (Tuple c i) = if i `elem` idxs then s <> singleton c else s

    idxs :: Array Int
    idxs = map (\x -> 10 * x) (0..8)

mixCellSet :: CellSet -> Effect CellSet
mixCellSet (CellSet empty xs) = map (CellSet empty) (randomArray xs)

numChars :: Array Char
numChars = ['1','2','3','4','5','6','7','8','9']

colorChars :: Array Char
colorChars = ['R','O','Y','L','G','B','I','P','V']

randomWord :: Unit -> Effect String
randomWord _ = fromMaybe "" -- random access won't fail
    <<< index wordlist 
    <$> randomInt 0 (length wordlist - 1)
    
-- fisher yates
randomArray :: ∀ a. Array a -> Effect (Array a)
randomArray input = do
    k <- randomInt 0 (length input - 1)
    fromMaybe (pure []) do -- indicies will always match
        v <- index input k
        arr <- deleteAt k input
        pure $ cons v <$> (randomArray arr)

diffNum :: Difficulty -> Int
diffNum Beginner  = 40
diffNum Casual    = 34
diffNum Tricky    = 30
diffNum Difficult = 26
diffNum Challenge = 22

emptySudoku :: String
emptySudoku = fromCharArray $ replicate 81 '.'
