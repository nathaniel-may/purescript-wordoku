module Test.Main where

import Prelude

import Data.Array (elem, foldl)
import Data.Either (hush)
import Data.Maybe (fromMaybe)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Generator (Difficulty(..), Game(..), generate)
import Solver (cellSetFromPuzzle, diagonalOf, readGrid, solve)
import Test.QuickCheck (Result, quickCheck, (<?>))
import Wordlist (wordlist)

main :: Effect Unit
main = do 
    tests <- allTests
    void $ traverse quickCheck tests

allTests :: Effect (Array Result)
allTests = sequence [test1]

test1 :: Effect Result
test1 = do
    str <- generate { difficulty: Beginner, restrictDiag: true, values: Wordoku }
    -- solved relies on all 9 characters being present in the generated puzzle. (not given)
    let solved = fromMaybe [] $ (solve true) =<< (flip readGrid str) =<< (hush $ cellSetFromPuzzle str)
    let diag = (foldl (<>) "") <<< (map show) <<< diagonalOf $ solved
    let failureMsg = "Wordoku diagonal failed:\n" <> diag <> "\n" <> str <> "\n"
    pure $ (diag `elem` wordlist) <?> failureMsg
    