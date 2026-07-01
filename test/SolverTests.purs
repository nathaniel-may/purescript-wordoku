module Test.SolverTests where

import Prelude

import Data.Array (all, zip)
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Sudoku (Difficulty(..), Game(..), Variant(..), generate)
import Sudoku.Encoding (DecodedKey(..), denormalize, keyToString)
import Sudoku.Internal (SearchResult(..))
import Sudoku.Internal.Grid (readGrid)
import Sudoku.Internal.Key (colorkuKey, mkKey)
import Sudoku.Internal.Solver as Internal
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

-- | A minimal, hand-verifiable example of a puzzle with more than one valid solution:
-- | https://sudoku.nathanielmay.com/colorku/challenge/317809260896102300254763189183297546569384712742516893675938421938421600421675938ROYLGBIPV
-- | (74 clues, missing only 7 cells — the ambiguity can be checked by hand). solveUnique
-- | must detect this as NotUnique rather than returning the first solution it finds.
solverUniquenessTests :: Array Result
solverUniquenessTests =
  [ let
      normalizedPuzzle = "317809260896102300254763189183297546569384712742516893675938421938421600421675938"
      puzzle = denormalize ColorkuKey normalizedPuzzle
      result = hush (readGrid colorkuKey puzzle) <#> Internal.solveUnique Standard
    in
      case result of
        Just (NotUnique _ _) -> true <?> "expected more than one solution"
        Just (Unique _) -> false <?> "expected NotUnique, but solveUnique found only one solution"
        Just NoSolution -> false <?> "expected NotUnique, but solveUnique found no solution"
        Nothing -> false <?> "failed to read grid for puzzle"
  ]
