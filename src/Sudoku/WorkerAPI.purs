module Sudoku.WorkerAPI (generateStringly) where

import Prelude
import Effect (Effect)
import Sudoku.Internal (Variant(..))
import Sudoku.Internal.Generator (Difficulty(..), generateSudoku)

-- NOTE: Fallbacks to Standard/Beginner handle typos silently.
generateStringly :: String -> String -> Effect String
generateStringly variantStr difficultyStr = 
  generateSudoku (parseVariant variantStr) (parseDifficulty difficultyStr)
  where
    parseVariant "UniqueDiagonal" = UniqueDiagonal
    parseVariant _ = Standard

    parseDifficulty "Casual" = Casual
    parseDifficulty "Tricky" = Tricky
    parseDifficulty "Difficult" = Difficult
    parseDifficulty "Challenge" = Challenge
    parseDifficulty _ = Beginner
