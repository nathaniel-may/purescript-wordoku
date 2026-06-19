module Sudoku.WorkerAPI (generateStringly, solveStringly) where

import Prelude
import Effect (Effect)
import Data.Array (all)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Sudoku.Internal (Variant(..))
import Sudoku.Internal.Generator (Difficulty(..), generateSudoku)
import Sudoku.Internal.Key (mkKey)
import Sudoku.Solver (solve)

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
  parseVariant "Standard" = Just Standard
  parseVariant _ = Nothing

  parseDifficulty "Casual" = Just Casual
  parseDifficulty "Tricky" = Just Tricky
  parseDifficulty "Difficult" = Just Difficult
  parseDifficulty "Challenge" = Just Challenge
  parseDifficulty "Beginner" = Just Beginner
  parseDifficulty _ = Nothing

-- | Solves a single puzzle.
-- | Args, in order: keyStr, normalizedPuzzleStr.
-- | `keyStr` is `keyToString decodedKey` (a 9-character string -- digits for
-- | Sudoku, "ROYLGBIPV" for Colorku, 9 distinct lowercase letters for
-- | Wordoku). Variant is derived from the key's shape: a key made entirely
-- | of 9 distinct lowercase a-z characters means Wordoku (UniqueDiagonal);
-- | anything else (digits, or the fixed Colorku key) means Standard. This
-- | mirrors `fromState`'s `Game -> Variant` rule in Main.purs, applied via
-- | key shape instead of Game, since the wire protocol carries only
-- | { id, key, puzzle } (see design doc "Variant derivation").
-- | `normalizedPuzzleStr` is the 81-character 0-9/'.' alphabet string.
-- | Returns a non-empty `error` for: an invalid key, a malformed grid
-- | (propagated from Sudoku.Solver.solve's Left), or no solution found
-- | (Right Nothing) -- all three are failures for this feature's purposes.
solveStringly :: String -> String -> Effect { result :: String, error :: String }
solveStringly keyStr normalizedPuzzleStr =
  case mkKey keyStr of
    Left err -> pure { result: "", error: "Invalid key: " <> err }
    Right key ->
      pure $ case solve key (variantForKey keyStr) normalizedPuzzleStr of
        Left err -> { result: "", error: err }
        Right Nothing -> { result: "", error: "puzzle has no solution" }
        Right (Just sol) -> { result: sol, error: "" }
  where
  variantForKey :: String -> Variant
  variantForKey s
    | isWordokuKeyShape s = UniqueDiagonal
    | otherwise = Standard

  isWordokuKeyShape :: String -> Boolean
  isWordokuKeyShape s = all isLowerAZ (toCharArray s)

  isLowerAZ :: Char -> Boolean
  isLowerAZ c = c >= 'a' && c <= 'z'
