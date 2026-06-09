module Test.Main where

import Prelude

import Data.Array (all, elem, filter, find, foldl, groupAll, length, null, (..))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush, isLeft)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Sudoku (Difficulty(..), Game(..), Variant(..), generate)
import Sudoku.Encoding (keyToString)
import Sudoku.Internal.Solver (diagonalOf)
import Sudoku.Internal.Solver as Internal
import Sudoku.Internal (Grid, SearchResult(..), cellSetFromPuzzle, gridString, mkCellSet, readGrid, readNumberGrid)
import Sudoku.Internal.Generator (diffNum)
import Sudoku.Wordlist (wordlist)
import Test.EncodingTests (encodingTests)
import Test.RoutingTests (routingTests)
import Test.QuickCheck (Result, quickCheck', (<?>))


main :: Effect Unit
main = do 
    props <- allProps
    void $ traverse (quickCheck' 1) props -- Run each Effect property once (the iteration is handled inside)
    let unitTests = encodingTests <> routingTests
    void $ traverse (quickCheck' 1) unitTests

allProps :: Effect (Array Result)
allProps = sequence [test1, test2, test3, test4, test5]

-- Helper to verify a generated puzzle meets all invariants
checkInvariants :: Variant -> Difficulty -> Game -> Effect (Either String Unit)
checkInvariants v d g = do
    { puzzle: str, key } <- generate { difficulty: d, variant: v, values: g }
    let clueCount = length $ filter (\c -> c /= '.') (toCharArray str)
    let expectedClues = diffNum v d
    
    let mCellSet = case mkCellSet '.' (toCharArray (keyToString key)) of
            Left err -> Left $ "Cell set error: " <> err
            Right cs -> Right cs
    
    let uniqueness = case mCellSet of
            Left err -> Left err
            Right cs -> case readGrid cs str of
                Left err -> Left $ "Read grid error: " <> err
                Right grid -> case Internal.solveUnique v grid of
                    Unique _ -> Right unit
                    NotUnique _ _ -> Left "Puzzle is not unique"
                    NoSolution -> Left "Puzzle has no solution"
    
    pure $ if clueCount /= expectedClues
           then Left $ "Clue count mismatch: expected " <> show expectedClues <> ", got " <> show clueCount
           else uniqueness

-- solved wordokus have a word from the wordlist on the diagonal
test1 :: Effect Result
test1 = do
    results <- traverse (\_ -> do
        { puzzle: str } <- generate { difficulty: Beginner, variant: UniqueDiagonal, values: Wordoku }
        let solved = solveWordoku UniqueDiagonal str
        let diag = (foldl (<>) "") <<< (map show) <<< diagonalOf $ solved
        pure $ diag `elem` wordlist
    ) (1..10)
    pure $ (all identity results) <?> "Wordoku diagonal test failed in one or more iterations"
    
-- solved puzzles have 81 values and 9 of each value.
test2 :: Effect Result
test2 = do
    results <- traverse (\_ -> do
        { puzzle: str } <- generate { difficulty: Beginner, variant: Standard, values: Sudoku }
        let solvedStr = gridString $ solveStr Standard str
        let total81 = 81 == (String.length solvedStr)
        let all9 = all (\x -> x == 9) $ map NonEmptyArray.length (groupAll $ toCharArray solvedStr)
        pure $ total81 && all9
    ) (1..10)
    pure $ (all identity results) <?> "Solution validity test (81 cells, 9 of each) failed in one or more iterations"

-- Verifies uniqueness and clue count for Standard Challenge (the hard case)
test3 :: Effect Result
test3 = do
    results <- traverse (\_ -> checkInvariants Standard Challenge Sudoku) (1..5)
    let mFirstFailure = find isLeft results
    pure $ (null $ filter isLeft results) <?> ("Standard Challenge: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

-- Verifies uniqueness and clue count for UniqueDiagonal Challenge
test4 :: Effect Result
test4 = do
    results <- traverse (\_ -> checkInvariants UniqueDiagonal Challenge Sudoku) (1..25)
    let mFirstFailure = find isLeft results
    pure $ (null $ filter isLeft results) <?> ("UniqueDiagonal Challenge: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

-- Verifies uniqueness and clue count across multiple difficulties (smoke test)
test5 :: Effect Result
test5 = do
    -- Note: Wordoku uniqueness is checked here via checkInvariants, 
    -- and also indirectly via test4 (which uses the same underlying generator logic).
    r1 <- checkInvariants Standard Beginner Sudoku
    r2 <- checkInvariants UniqueDiagonal Tricky Wordoku
    r3 <- checkInvariants Standard Difficult Colorku
    let results = [r1, r2, r3]
    let mFirstFailure = find isLeft results
    pure $ (null $ filter isLeft results) <?> ("Cross-difficulty smoke test failed: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

hushLeft :: ∀ a b. Either a b -> Maybe a
hushLeft (Left x) = Just x
hushLeft _ = Nothing

solveStr :: Variant -> String -> Grid
solveStr v str = fromMaybe [] $ (Internal.solve v) =<< (hush $ readNumberGrid str)

-- Wordoku solver helper that infers cellset (may still have issues if letters are missing)
solveWordoku :: Variant -> String -> Grid
solveWordoku v str = fromMaybe [] $ (Internal.solve v)
    =<< (hush <<< flip readGrid str)
    =<< (hush $ cellSetFromPuzzle str)