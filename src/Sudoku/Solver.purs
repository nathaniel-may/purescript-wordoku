{-
Translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/


-}

module Sudoku.Solver where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Sudoku.Internal.Solver (CellSet, SearchResult, Variant, gridString, readGrid)
import Sudoku.Internal.Solver as Internal

{- 
Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.

usage: TODO use real example
> puzzle = "..."
> solve numbers Standard puzzle
> Right "123"
-} 
solve :: CellSet -> Variant -> String -> Either String (Maybe String)
solve cs v input = do
    puzzle <- readGrid cs input
    let solution = Internal.solve v puzzle
    pure $ map gridString solution

{- 
Takes in a puzzle and finds all solutions.
-} 
solveAll :: CellSet -> Variant -> String -> Either String (Array String)
solveAll cs v input = do
    puzzle <- readGrid cs input
    let solution = Internal.solveAll v puzzle
    pure $ map gridString solution

{- 
Takes in a puzzle, determines if it has a solution, and if that solution is unique:
- Nothing = puzzle has no solution
- Just (Left x) = Grid x is the only unique solution
- Just (Right (Tuple x y)) = x and y are both valid solutions so this puzzle does not have a unique solution

To determine uniqueness, it must attempt to visit every solution in the space and find all but one invalid
or exit early when it finds a second solution. For a fast single solution use `solve`.
-} 
solveUnique :: CellSet -> Variant -> String -> Either String SearchResult
solveUnique cs v input = do
    puzzle <- readGrid cs input
    pure $ Internal.solveUnique v puzzle