module Generator where

import Prelude

import Data.Array (cons, deleteAt, index, length)
import Data.Maybe (Maybe, fromMaybe)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Random (randomInt)
import Solver (CellSet, mkCellSet)

type Opts = { 
      restrictDiag :: Boolean 
    , values :: Values
    , difficulty :: Difficulty }

data Values = Numbers | Word String | Colors
derive instance valueEq :: Eq Values

data Difficulty = Beginner | Casual | Tricky | Difficult | Challenge | Inhuman
derive instance difficultyEq :: Eq Difficulty

-- generate :: Opts -> Effect String
-- generate = ???

-- fisher yates
randomArray :: ∀ a. Array a -> Effect (Array a)
randomArray input = do
    k <- randomInt 0 (length input)
    fromMaybe (pure []) do -- indicies will always match
        v <- index input k
        arr <- deleteAt k input
        pure $ cons v <$> (randomArray arr)

cellSet :: Values -> Maybe CellSet
cellSet Numbers = mkCellSet '.' ['1','2','3','4','5','6','7','8','9']
cellSet Colors = mkCellSet '.' ['R','O','Y','L','G','B','I','P','V']
cellSet (Word word) = mkCellSet '.' (toCharArray word)

diffNum :: Difficulty -> Int
diffNum Beginner  = 62
diffNum Casual    = 53
diffNum Tricky    = 44
diffNum Difficult = 35
diffNum Challenge = 26
diffNum Inhuman   = 17



