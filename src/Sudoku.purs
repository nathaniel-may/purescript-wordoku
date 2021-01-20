module Sudoku (module Exports) where

import Sudoku.Generator
    ( Difficulty(..)
    , Game(..)
    , Opts
    , emptySudoku 
    , generate
    ) as Exports

import Sudoku.Internal.Solver 
    ( Variant(..)
    ) as Exports

import Sudoku.Solver
    ( solve
    , solveUnique
    , solveAll
    ) as Exports