module Generator where

import Data.Enum
import Prelude

import Data.Array (cons, deleteAt, index, length, replicate, uncons, (..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Random (randomInt)
import Halogen.HTML (samp)
import Solver (CellSet(..), Cell(..), Grid, mkCellSet, readGrid, replace2D, gridString, solve, solveUnique)
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
generate opts = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
    mCellSet <- hush <$> cellSet opts.values
    let mFilled = solve opts.restrictDiag =<< ((\set -> readGrid set emptySudoku) =<< mCellSet)
    randIdxs <- randomArray (0..80)
    pure $ do 
        cs <- mCellSet
        filled <- mFilled
        reduceBy cs (81 - diffNum opts.difficulty) randIdxs filled
    where
        reduceBy :: CellSet -> Int -> Array Int -> Grid -> Maybe Grid
        reduceBy cs count idxs grid = if count > 64 then Nothing else do
            lr <- solveUnique opts.restrictDiag grid
            { head: idx, tail: rands } <- uncons idxs
            -- check that the given grid has a unique solution. If it doesn't, backtracking won't help.
            case lr of 
                Left _ -> 
                    if (count <= 0) 
                    then Just grid
                    -- try removing the next one
                    else case reduceBy cs (count - 1) rands (removeAt cs idx grid) of
                        -- backtrack
                        Nothing -> reduceBy cs count rands grid
                        Just grid' -> Just grid'
                -- backtracking won't help if the board doesn't already have a unique solution
                Right _ -> Nothing

        removeAt :: CellSet -> Int -> Grid -> Grid
        removeAt (CellSet _ allValues) idx grid = 
            replace2D idx (Possible allValues) grid
        
        toStringOrLoop :: Maybe Grid -> Effect String
        toStringOrLoop Nothing = generate opts
        toStringOrLoop (Just grid) = pure $ gridString grid

cellSet :: Game -> Effect (Either String CellSet)
cellSet Sudoku = mkCellSet '.' <$> randomArray ['1','2','3','4','5','6','7','8','9']
cellSet Colorku = mkCellSet '.' <$> randomArray ['R','O','Y','L','G','B','I','P','V']
cellSet Wordoku = mkCellSet '.' <$> ((randomArray <<< toCharArray) =<< (randomWord unit))
    
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
diffNum Beginner  = 60
diffNum Casual    = 50
diffNum Tricky    = 40
diffNum Difficult = 30
diffNum Challenge = 22

emptySudoku :: String
emptySudoku = fromCharArray $ replicate 81 '.'
