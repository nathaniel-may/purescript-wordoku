module Test.SolverTests where

import Prelude

import Data.Array (all, zip)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Sudoku (Difficulty(..), Game(..), Variant(..), generate)
import Sudoku.Encoding (keyToString)
import Sudoku.Internal.Key (mkKey)
import Sudoku.Solver (solve)
import Test.QuickCheck (Result(..), (<?>))

-- | For each of Sudoku/Wordoku/Colorku, generates a puzzle, solves it via
-- | the same string-based `solve`/key-parsing path the worker's
-- | `solveStringly` uses, and verifies:
-- |  1. the solution is fully filled (no '.' cells remaining), and
-- |  2. the solution agrees with the puzzle at every clue position.
solverCompletenessTests :: Effect (Array Result)
solverCompletenessTests = do
  r1 <- checkCompleteness Standard Sudoku
  r2 <- checkCompleteness UniqueDiagonal Wordoku
  r3 <- checkCompleteness Standard Colorku
  pure [ r1, r2, r3 ]

checkCompleteness :: Variant -> Game -> Effect Result
checkCompleteness variant game = do
  { puzzle, key } <- generate { difficulty: Tricky, variant, values: game }
  pure $ case mkKey (keyToString key) of
    Left err -> Failed ("checkCompleteness: invalid key from generate: " <> err)
    Right k -> case solve k variant puzzle of
      Right (Just solution) ->
        let
          allFilled = all (_ /= '.') (toCharArray solution)
          cluesPreserved = all (\(Tuple p s) -> p == '.' || p == s) (zip (toCharArray puzzle) (toCharArray solution))
        in
          (allFilled && cluesPreserved)
            <?> ("solve produced an incomplete or inconsistent solution for " <> show game)
      Right Nothing -> Failed ("solve found no solution for a generated " <> show game <> " puzzle")
      Left err -> Failed ("solve returned an error for a generated " <> show game <> " puzzle: " <> err)
