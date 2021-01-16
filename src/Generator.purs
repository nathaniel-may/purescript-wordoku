module Generator where

import Prelude

import Data.Array (cons, deleteAt, elem, index, foldl, length, replicate, uncons, zip, (..))
import Data.Either (hush)
import Data.Enum (class Enum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, joinFiber)
import Effect.Random (randomInt)
import Lib (mkAff, sequenceJoin)
import Solver (Cell(..), CellSet(..), Grid, SearchResult(..), gridString, mkCellSet, readGrid, readNumberGrid, replace2D, solve, solveUnique)
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

generate :: Opts -> Aff String
generate = generate' where

    generate' :: Opts -> Aff String
    generate' opts = case opts.values of
        Sudoku  -> game opts
        Colorku -> mapValues (Map.fromFoldable $ numbers `zip` colors) <$> game opts
        Wordoku -> do
            fiberG <- forkAff $ game opts
            fiberW <- forkAff <<< mkAff $ randomWord unit
            g <- joinFiber fiberG
            w <- joinFiber fiberW
            pure $ mapValues (wordMap w g) g

    game :: Opts -> Aff String
    game opts = generateSudoku opts.restrictDiag opts.difficulty

    wordMap :: String -> String -> Map Char Char
    wordMap word sudoku = Map.fromFoldable $ toCharArray (diagonalOf solved) `zip` toCharArray word
        where solved = fromMaybe sudoku $ map gridString $ solve true <<< fromMaybe [] <<< readNumberGrid $ sudoku

    -- keys become values
    mapValues :: Map Char Char -> String -> String
    mapValues m str = foldl 
        (\s c -> s <> (singleton <<< fromMaybe '.' $ Map.lookup c m))
        ""
        (toCharArray str)

generateSudoku :: Boolean -> Difficulty -> Aff String
generateSudoku restrictDiag difficulty = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
    fiberA <- forkAff <<< mkAff $ randomArray numbers
    fiberB <- forkAff <<< mkAff $ randomArray (0..80)
    randNums <- joinFiber fiberA
    randIdxs <- joinFiber fiberB
    let mCellSet = hush $ mkCellSet '.' randNums
    let mFilled = solve restrictDiag =<< ((\set -> readGrid set emptySudoku) =<< mCellSet)
    sequenceJoin $ do
        cs <- mCellSet
        filled <- mFilled
        pure $ reduceBy cs (81 - diffNum difficulty) randIdxs filled
    where
        reduceBy :: CellSet -> Int -> Array Int -> Grid -> Aff (Maybe Grid)
        reduceBy cs count idxs grid = if count > 64 then pure Nothing else reduced
            where
                reduced :: Aff (Maybe Grid)
                reduced = sequenceJoin $ map (\ht -> next ht.head ht.tail result) (uncons idxs)

                result :: SearchResult
                result = solveUnique restrictDiag grid

                next :: Int -> Array Int -> SearchResult -> Aff (Maybe Grid)
                next idx rands result = case result of
                    -- backtracking won't help if the board doesn't already have a unique solution
                    (NotUnique _ _) -> pure Nothing
                    NoSolution      -> pure Nothing
                    -- check that the given grid has a unique solution. If it doesn't, backtracking won't help.
                    Unique _ -> 
                        if (count <= 0) 
                        then pure $ Just grid
                        -- try removing the next one
                        else (backtrack rands) =<< (reduceBy cs (count - 1) rands (removeAt cs idx grid))

                backtrack :: Array Int -> Maybe Grid -> Aff (Maybe Grid)
                backtrack rands mGrid = case mGrid of
                    Nothing    -> reduceBy cs count rands grid
                    y@(Just _) -> pure y


        removeAt :: CellSet -> Int -> Grid -> Grid
        removeAt (CellSet _ allValues) idx grid = 
            replace2D idx (Possible allValues) grid
        
        toStringOrLoop :: Maybe Grid -> Aff String
        toStringOrLoop Nothing = generateSudoku restrictDiag difficulty
        toStringOrLoop (Just grid) = pure $ gridString grid

diagonalOf :: String -> String
diagonalOf str = foldl onlyDiag "" $ toCharArray str `zip` (0..80) where

    onlyDiag :: String -> Tuple Char Int -> String
    onlyDiag s (Tuple c i) = if i `elem` idxs then s <> singleton c else s

    idxs :: Array Int
    idxs = map (\x -> 10 * x) (0..8)

numbers :: Array Char
numbers = ['1','2','3','4','5','6','7','8','9']

colors :: Array Char
colors = ['R','O','Y','L','G','B','I','P','V']

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
