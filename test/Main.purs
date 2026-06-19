module Test.Main where

import Prelude

import Data.Array (all, elem, filter, find, groupAll, length, null, (..))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush, isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Sudoku (Difficulty(..), Game(..), Variant(..), generate)
import Sudoku.Encoding (keyToString)
import Sudoku.Internal (SearchResult(..), diagonalString)
import Sudoku.Internal.Grid (Grid, gridString, readGrid)
import Sudoku.Internal.Generator (diffNum)
import Sudoku.Internal.Key (Key, mkKey, sudokuKey)
import Sudoku.Internal.Solver as Internal
import Sudoku.Wordlist (wordlist)
import Test.DisplayTests (displayTests)
import Test.EncodingTests (encodingTests)
import Test.RegressionTests (regressionTests)
import Test.RoutingTests (routingTests)
import Test.SolverTests (solverCompletenessTests)
import Test.StaleSolveTests (staleSolveTests)
import Test.QuickCheck (Result(..), quickCheck', (<?>))

main :: Effect Unit
main = do
  props <- allProps
  solverResults <- solverCompletenessTests
  void $ traverse (quickCheck' 1) (props <> solverResults) -- Run each Effect property once (the iteration is handled inside)
  let unitTests = encodingTests <> routingTests <> regressionTests <> displayTests <> staleSolveTests
  void $ traverse (quickCheck' 1) unitTests

allProps :: Effect (Array Result)
allProps = sequence
  [ -- solved wordokus have a word from the wordlist on the diagonal
    do
      results <- traverse
        ( \_ -> do
            { puzzle: str, key } <- generate { difficulty: Beginner, variant: UniqueDiagonal, values: Wordoku }
            let mKey = hush $ mkKey (keyToString key)
            let
              diag = fromMaybe "" $ do
                k <- mKey
                grid <- solveWithKey k UniqueDiagonal str
                pure $ diagonalString $ gridString k grid
            pure $
              if diag `elem` wordlist then Right unit
              else Left ("puzzle=" <> str <> " diagonal=" <> diag)
        )
        (1 .. 100)
      pure $ case find isLeft results of
        Nothing -> Success
        Just (Left msg) -> Failed ("Wordoku diagonal test failed: " <> msg)
        Just (Right _) -> Failed "impossible"

  , -- solved puzzles have 81 values and 9 of each value.
    do
      results <- traverse
        ( \_ -> do
            { puzzle: str } <- generate { difficulty: Beginner, variant: Standard, values: Sudoku }
            let solvedStr = fromMaybe "" $ (gridString sudokuKey) <$> solveStr Standard str
            let total81 = 81 == (String.length solvedStr)
            let all9 = all (\x -> x == 9) $ map NonEmptyArray.length (groupAll $ toCharArray solvedStr)
            pure $ total81 && all9
        )
        (1 .. 10)
      pure $ (all identity results) <?> "Solution validity test (81 cells, 9 of each) failed in one or more iterations"

  , -- Small smoke test for uniqueness and clue count for Standard Challenge which takes a long time to generate
    do
      results <- checkInvariants Standard Challenge Sudoku
      pure $ (not $ isLeft results) <?> ("Standard Challenge: " <> (fromMaybe "" $ hushLeft results))

  , -- Verifies uniqueness and clue count for Standard Difficult which takes far less time to generate than Challenge.
    do
      results <- traverse (\_ -> checkInvariants Standard Difficult Sudoku) (1 .. 20)
      let mFirstFailure = find isLeft results
      pure $ (null $ filter isLeft results) <?> ("Standard Difficult: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

  , -- Small smoke test for uniqueness and clue count for UniqueDiagonal Challenge
    do
      results <- traverse (\_ -> checkInvariants UniqueDiagonal Challenge Sudoku) (1 .. 2)
      let mFirstFailure = find isLeft results
      pure $ (null $ filter isLeft results) <?> ("UniqueDiagonal Challenge: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

  , -- Verifies uniqueness and clue count across multiple difficulties (smoke test)
    do
      r1 <- checkInvariants Standard Beginner Sudoku
      r2 <- checkInvariants UniqueDiagonal Tricky Wordoku
      r3 <- checkInvariants Standard Difficult Colorku
      let results = [ r1, r2, r3 ]
      let mFirstFailure = find isLeft results
      pure $ (null $ filter isLeft results) <?> ("Cross-difficulty smoke test failed: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))
  ]

-- Helper to verify a generated puzzle meets all invariants
checkInvariants :: Variant -> Difficulty -> Game -> Effect (Either String Unit)
checkInvariants v d g = do
  { puzzle: str, key } <- generate { difficulty: d, variant: v, values: g }
  let clueCount = length $ filter (\c -> c /= '.') (toCharArray str)
  let expectedClues = diffNum v d

  let
    mKey :: Either String Key
    mKey = mkKey (keyToString key)

  let
    uniqueness = case mKey of
      Left err -> Left $ "Key error: " <> err
      Right k -> case readGrid k str of
        Left err -> Left $ "Read grid error: " <> err
        Right grid -> case Internal.solveUnique v grid of
          Unique _ -> Right unit
          NotUnique _ _ -> Left "Puzzle is not unique"
          NoSolution -> Left "Puzzle has no solution"

  pure $
    if clueCount /= expectedClues then Left $ "Clue count mismatch: expected " <> show expectedClues <> ", got " <> show clueCount
    else uniqueness

hushLeft :: ∀ a b. Either a b -> Maybe a
hushLeft (Left x) = Just x
hushLeft _ = Nothing

solveStr :: Variant -> String -> Maybe Grid
solveStr v str = (Internal.solve v) =<< (hush $ readGrid sudokuKey str)

solveWithKey :: Key -> Variant -> String -> Maybe Grid
solveWithKey key v str = (Internal.solve v) =<< (hush $ readGrid key str)
