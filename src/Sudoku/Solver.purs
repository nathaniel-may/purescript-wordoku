-- | module for solving sudoku puzzles
-- | re-exports necessary basics from Internal
module Sudoku.Solver
    ( module Exports
    , solve
    , solveAll
    , solveUnique) 
where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Sudoku.Internal (SearchResult(..), Variant(..), colors, numbers) as Exports
import Sudoku.Internal (CellSet, Variant, SearchResult, gridString, readGrid)
import Sudoku.Internal.Solver as Internal


-- | Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.
-- | 
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solve numbers Standard puzzle
-- | > (Right (Just "415873962836129475972564183184935726763482519529716834657348291348291657291657348"))
solve :: CellSet -> Variant -> String -> Either String (Maybe String)
solve cs v input = do
    puzzle <- readGrid cs input
    let solution = Internal.solve v puzzle
    pure $ map gridString solution


-- | Takes in a puzzle and finds all solutions.
-- | 
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solveAll numbers Standard puzzle
-- | > (Right ["415873962836129475972564183184935726763482519529716834657348291348291657291657348"])
solveAll :: CellSet -> Variant -> String -> Either String (Array String)
solveAll cs v input = do
    puzzle <- readGrid cs input
    let solution = Internal.solveAll v puzzle
    pure $ map gridString solution
 
-- | Takes in a puzzle, determines if it has a solution, and if that solution is unique:
-- | - Nothing = puzzle has no solution
-- | - Just (Left x) = Grid x is the only unique solution
-- | - Just (Right (Tuple x y)) = x and y are both valid solutions so this puzzle does not have a unique solution
-- | 
-- | To determine uniqueness, it must attempt to visit every solution in the space and find all but one invalid
-- | or exit early when it finds a second solution. For a fast single solution use `solve`. 
-- | 
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solveUnique numbers Standard puzzle
-- | (Right (Unique "415873962836129475972564183184935726763482519529716834657348291348291657291657348"))
solveUnique :: CellSet -> Variant -> String -> Either String (SearchResult String)
solveUnique cs v input = do
    puzzle <- readGrid cs input
    let result = Internal.solveUnique v puzzle
    pure $ map gridString result