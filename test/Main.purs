module Test.Main where

import Prelude

import Data.Array (all, elem, foldl, group')
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.String as String
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Generator (Difficulty(..), Game(..), generate)
import Solver (Grid, Variant(..), cellSetFromPuzzle, diagonalOf, gridString, readGrid, solve)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Wordlist (wordlist)

main :: Effect Unit
main = do 
    tests <- allTests
    void $ traverse quickCheck tests

allTests :: Effect (Array Result)
allTests = sequence [test1, test2]

-- solved wordokus have a word from the wordlist on the diagonal
test1 :: Effect Result
test1 = do
    str <- generate { difficulty: Beginner, variant: UniqueDiagonal, values: Wordoku }
    -- solved relies on all 9 characters being present in the generated puzzle. (not given)
    let solved = solveStr UniqueDiagonal str
    let diag = (foldl (<>) "") <<< (map show) <<< diagonalOf $ solved
    let failureMsg = "Wordoku diagonal failed:\n" <> diag <> "\n" <> str <> "\n"
    pure $ (diag `elem` wordlist) <?> failureMsg
    
-- solved puzzles have 81 values and 9 of each value.
test2 :: Effect Result
test2 = do
    str <- generate { difficulty: Beginner, variant: Standard, values: Sudoku }
    let solvedStr = gridString $ solveStr Standard str
    let total81 = 81 == (String.length solvedStr)
    let all9 = all (\x -> x == 9) $ map NonEmptyArray.length (group' $ toCharArray solvedStr)
    let failureMsg = (if total81 then "" else "Solution should have 81 values. It has " <> (show $ String.length solvedStr) <> ". ") <> (if all9 then "" else "Each value doesn't show up 9 times in a solved puzzle.")
    pure $ (total81 && all9) <?> failureMsg

solveStr :: Variant -> String -> Grid
solveStr v str = fromMaybe [] $ (solve v)
    =<< (flip readGrid str)
    =<< (hush $ cellSetFromPuzzle str)