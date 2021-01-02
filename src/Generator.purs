module Generator where

import Prelude

import Data.Array (cons, deleteAt, index, length, replicate, uncons, (..))
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Random (randomInt)
import Solver (CellSet(..), Cell(..), Grid, mkCellSet, readGrid, replace2D, gridString, solve, solveUnique)

type Opts = { 
      restrictDiag :: Boolean 
    , values :: Values
    , difficulty :: Difficulty }

data Values = Numbers | Word String | Colors
derive instance valueEq :: Eq Values

data Difficulty = Beginner | Casual | Tricky | Difficult | Challenge | Inhuman
derive instance difficultyEq :: Eq Difficulty

-- TODO diagonal
generate :: Opts -> Effect String
generate opts = toStringOrLoop =<< do -- may need to generate another puzzle if the difficulty cannot be achieved. Highly unlikely.
    mCellSet <- hush <$> cellSet opts.values
    let mFilled = solve =<< ((\set -> readGrid set emptySudoku) =<< mCellSet)
    randIdxs <- randomArray (0..80)
    pure $ do 
        cs <- mCellSet
        filled <- mFilled
        reduceBy cs (81 - diffNum opts.difficulty) randIdxs filled
    where
        reduceBy :: CellSet -> Int -> Array Int -> Grid -> Maybe Grid
        reduceBy cs count idxs grid = if count > 64 then Nothing else do
            lr <- solveUnique grid
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

cellSet :: Values -> Effect (Either String CellSet)
cellSet Numbers = mkCellSet '.' <$> randomArray ['1','2','3','4','5','6','7','8','9']
cellSet Colors = mkCellSet '.' <$> randomArray ['R','O','Y','L','G','B','I','P','V']
cellSet (Word word) = mkCellSet '.' <$> randomArray (toCharArray word)

-- fisher yates
randomArray :: ∀ a. Array a -> Effect (Array a)
randomArray input = do
    k <- randomInt 0 (length input - 1)
    fromMaybe (pure []) do -- indicies will always match
        v <- index input k
        arr <- deleteAt k input
        pure $ cons v <$> (randomArray arr)

diffNum :: Difficulty -> Int
diffNum Beginner  = 62
diffNum Casual    = 53
diffNum Tricky    = 44
diffNum Difficult = 35
diffNum Challenge = 26
diffNum Inhuman   = 17

emptySudoku :: String
emptySudoku = fromCharArray $ replicate 81 '.'

