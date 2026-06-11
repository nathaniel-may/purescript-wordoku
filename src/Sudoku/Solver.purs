-- | module for solving sudoku puzzles
-- | re-exports necessary basics from Internal
module Sudoku.Solver
  ( module Exports
  , solve
  , solveAll
  , solveUnique
  ) where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe)
import Sudoku.Internal (SearchResult(..), Variant(..)) as Exports
import Sudoku.Internal (Variant, SearchResult)
import Sudoku.Internal.Grid (gridString, readGrid)
import Sudoku.Internal.Key (Key)
import Sudoku.Internal.Solver as Internal

-- | Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.
-- |
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solve sudokuKey Standard puzzle
-- | > (Right (Just "415873962836129475972564183184935726763482519529716834657348291348291657291657348"))
solve :: Key -> Variant -> String -> Either String (Maybe String)
solve key v input = do
  puzzle <- readGrid key input
  let solution = Internal.solve v puzzle
  pure $ map (gridString key) solution

-- | Takes in a puzzle and finds all solutions.
-- |
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solveAll sudokuKey Standard puzzle
-- | > (Right ["415873962836129475972564183184935726763482519529716834657348291348291657291657348"])
solveAll :: Key -> Variant -> String -> Either String (Array String)
solveAll key v input = do
  puzzle <- readGrid key input
  let solution = Internal.solveAll v puzzle
  pure $ map (gridString key) solution

-- | Takes in a puzzle, determines if it has a solution, and if that solution is unique.
-- |
-- | usage:
-- | > puzzle = "4.58.....8361..47.........3..49...26763.8.519529.1683........91..82.1.5..9..57348"
-- | > solveUnique sudokuKey Standard puzzle
-- | (Right (Unique "415873962836129475972564183184935726763482519529716834657348291348291657291657348"))
solveUnique :: Key -> Variant -> String -> Either String (SearchResult String)
solveUnique key v input = do
  puzzle <- readGrid key input
  let result = Internal.solveUnique v puzzle
  pure $ map (gridString key) result
