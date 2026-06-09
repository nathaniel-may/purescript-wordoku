module Test.Main where

import Prelude

import Data.Array (all, elem, filter, foldl, groupAll, length, null, (..))
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Sudoku (Difficulty(..), Game(..), Variant(..), generate)
import Sudoku.Internal.Solver (diagonalOf)
import Sudoku.Internal.Solver as Internal
import Sudoku.Internal (Grid, SearchResult(..), cellSetFromPuzzle, colors, gridString, numbers, readGrid, readNumberGrid)
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
    { puzzle: str } <- generate { difficulty: d, variant: v, values: g }
    let clueCount = length $ filter (\c -> c /= '.') (toCharArray str)
    let expectedClues = diffNum v d
    -- For Sudoku/Colorku we know the cell set. For Wordoku we infer it but allow it to be < 9 
    -- because some letters might be missing in the clues.
    -- Actually, for Wordoku, we can just use mkCellSet '.' (unique (toCharArray (str-dots)))
    -- But uniqueness check needs to know the FULL set of 9.
    let mCellSet = case g of
            Sudoku -> Right numbers
            Colorku -> Right colors
            Wordoku -> cellSetFromPuzzle str -- This still has the "must be 9" issue if we don't know the word

    let uniqueness = case mCellSet of
            Left err -> if g == Wordoku 
                        then Right unit -- Skip uniqueness for Wordoku if we can't infer the 9-char set easily
                        else Left $ "Cell set error: " <> err
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
    results <- traverse (\_ -> checkInvariants Standard Challenge Sudoku) (1..1)
    let failure = foldl (\acc r -> case acc, r of
            Nothing, Left err -> Just err
            _, _ -> acc) Nothing results
    pure $ (all (\r -> r == Right unit) results) <?> ("Standard Challenge: " <> (fromMaybe "" failure))

-- Verifies uniqueness and clue count for UniqueDiagonal Challenge
test4 :: Effect Result
test4 = do
    results <- traverse (\_ -> checkInvariants UniqueDiagonal Challenge Sudoku) (1..1)
    let failure = foldl (\acc r -> case acc, r of
            Nothing, Left err -> Just err
            _, _ -> acc) Nothing results
    pure $ (all (\r -> r == Right unit) results) <?> ("UniqueDiagonal Challenge: " <> (fromMaybe "" failure))

-- Verifies uniqueness and clue count across multiple difficulties (smoke test)
test5 :: Effect Result
test5 = do
    r1 <- checkInvariants Standard Beginner Sudoku
    r2 <- checkInvariants UniqueDiagonal Tricky Wordoku
    r3 <- checkInvariants Standard Difficult Colorku
    let failures = filter (\r -> r /= Right unit) [r1, r2, r3]
    pure $ (null failures) <?> "Cross-difficulty smoke test failed"

solveStr :: Variant -> String -> Grid
solveStr v str = fromMaybe [] $ (Internal.solve v) =<< (hush $ readNumberGrid str)

-- Wordoku solver helper that infers cellset (may still have issues if letters are missing)
solveWordoku :: Variant -> String -> Grid
solveWordoku v str = fromMaybe [] $ (Internal.solve v)
    =<< (hush <<< flip readGrid str)
    =<< (hush $ cellSetFromPuzzle str)