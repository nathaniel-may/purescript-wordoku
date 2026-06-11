module Sudoku.Internal.Grid
  ( Grid
  , readGrid
  , readNumberGrid
  , gridString
  , extract
  , replace2D
  , traverseRows
  , transpose
  , subGridsToRows
  , diagonalOf
  , replaceDiagonal
  ) where

import Prelude

import Data.Array (concat, filter, zip, (..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (quot)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Sudoku.Internal (Cell, CellSet, Row, chunksOf, index2D, mkCellSet, readCell, replaceAt, transpose) as Internal

newtype Grid = Grid (Array Internal.Row)

derive instance eqGrid :: Eq Grid

readGrid :: Internal.CellSet -> String -> Either String Grid
readGrid cellSet s =
  if CodePoints.length s /= 81 then Left "input must be exactly 81 characters long"
  else map Grid $ traverse (traverse $ Internal.readCell cellSet) (Internal.chunksOf 9 $ toCharArray s)

readNumberGrid :: String -> Either String Grid
readNumberGrid s = (\cs -> readGrid cs s) =<< Internal.mkCellSet '.' [ '1', '2', '3', '4', '5', '6', '7', '8', '9' ]

gridString :: Grid -> String
gridString (Grid rows) = joinWith "" $ map (joinWith "" <<< map show) rows

extract :: Grid -> Array Internal.Row
extract (Grid rows) = rows

replace2D :: Int -> Internal.Cell -> Grid -> Grid
replace2D i v (Grid rows) =
  let (Tuple x y) = Tuple (i `quot` 9) (i `mod` 9)
  in Grid $ Internal.replaceAt x (Internal.replaceAt y (const v)) rows

traverseRows :: (Internal.Row -> Maybe Internal.Row) -> Grid -> Maybe Grid
traverseRows f (Grid rows) = Grid <$> traverse f rows

transpose :: Grid -> Grid
transpose (Grid rows) = Grid (Internal.transpose rows)

subGridsToRows :: Grid -> Grid
subGridsToRows (Grid rows) = Grid $ (=<<)
  ( \rs ->
      let
        r0 = map (\r -> take3 0 r) rs
        r1 = map (\r -> take3 1 r) rs
        r2 = map (\r -> take3 2 r) rs
      in
        [ concat r0, concat r1, concat r2 ]
  )
  (Internal.chunksOf 3 rows)
  where
  take3 n = map snd <<< filter (\(Tuple i _) -> i `quot` 3 == n) <<< zip (0 .. 8)

diagIdxs :: Array Int
diagIdxs = map ((*) 10) (0 .. 8)

diagonalOf :: Grid -> Internal.Row
diagonalOf (Grid rows) = fromMaybe [] $ traverse (Internal.index2D rows) diagIdxs

replaceDiagonal :: Internal.Row -> Grid -> Grid
replaceDiagonal row grid = foldl (\g (Tuple cell i) -> replace2D i cell g) grid (row `zip` diagIdxs)
