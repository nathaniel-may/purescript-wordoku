module Sudoku.WorkerAPI (generateStringly) where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Sudoku.Internal (Variant(..))
import Sudoku.Internal.Generator (Difficulty(..), generateSudoku)

-- | Returns a record with either the result or an error message.
generateStringly :: String -> String -> Effect { result :: String, error :: String }
generateStringly variantStr difficultyStr =
  case parseVariant variantStr, parseDifficulty difficultyStr of
    Just v, Just d -> do
      res <- generateSudoku v d
      pure { result: res, error: "" }
    Nothing, _ -> pure { result: "", error: "Invalid variant: " <> variantStr }
    _, Nothing -> pure { result: "", error: "Invalid difficulty: " <> difficultyStr }
  where
    parseVariant "UniqueDiagonal" = Just UniqueDiagonal
    parseVariant "Standard"       = Just Standard
    parseVariant _                = Nothing

    parseDifficulty "Casual"      = Just Casual
    parseDifficulty "Tricky"      = Just Tricky
    parseDifficulty "Difficult"   = Just Difficult
    parseDifficulty "Challenge"   = Just Challenge
    parseDifficulty "Beginner"    = Just Beginner
    parseDifficulty _             = Nothing
