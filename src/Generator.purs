module Generator where

import Prelude

import Data.Maybe (Maybe)
import Data.String.CodeUnits (toCharArray)
import Solver (CellSet, mkCellSet)

data Values = Numbers | Word String | Colors
derive instance valueEq :: Eq Values

data Difficulty = Easy | Medium | Hard | VeryHard | Inhumane
derive instance difficultyEq :: Eq Difficulty

cellSet :: Values -> Maybe CellSet
cellSet Numbers = mkCellSet '.' ['1','2','3','4','5','6','7','8','9']
cellSet Colors = mkCellSet '.' ['R','O','Y','L','G','B','I','P','V']
cellSet (Word word) = mkCellSet '.' (toCharArray word)

type Opts = { 
      restrictDiag :: Boolean 
    , values :: Values
    , difficulty :: Difficulty }

-- generate :: Opts -> String