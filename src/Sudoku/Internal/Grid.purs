module Sudoku.Internal.Grid
  ( Grid
  , readGrid
  , gridString
  , emptyGridWith
  , extract
  , replace2D
  , traverseRows
  , transpose
  , subGridsToRows
  , diagonalOf
  , replaceDiagonal
  ) where

import Prelude

import Data.Array (concat, filter, replicate, zip, (..))
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int (quot)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (singleton, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Sudoku.Internal (Cell(..), Row, Value, allValues, chunksOf, index2D, replaceAt, transpose) as Internal
import Sudoku.Internal.Key (Key, fromChar, toChar) as Key

newtype Grid = Grid (Array Internal.Row)

derive instance eqGrid :: Eq Grid

readGrid :: Key.Key -> String -> Either String Grid
readGrid key s =
  if CodePoints.length s /= 81 then Left "input must be exactly 81 characters long"
  else map Grid $ traverse (traverse $ readCell key) (Internal.chunksOf 9 $ toCharArray s)
  where
  readCell :: Key.Key -> Char -> Either String Internal.Cell
  readCell k c
    | c == '.' = Right $ Internal.Possible Internal.allValues
    | otherwise = case Key.fromChar k c of
        Just v -> Right $ Internal.Fixed v
        Nothing -> Left $ "character " <> show c <> " is not in the key"

gridString :: Key.Key -> Grid -> String
gridString key (Grid rows) = joinWith "" $ map (joinWith "" <<< map (cellToChar key)) rows
  where
  cellToChar :: Key.Key -> Internal.Cell -> String
  cellToChar k (Internal.Fixed v) = singleton $ Key.toChar k v
  cellToChar _ (Internal.Possible _) = "."

emptyGridWith :: Array Internal.Value -> Grid
emptyGridWith values = Grid $ replicate 9 (replicate 9 (Internal.Possible values))

extract :: Grid -> Array Internal.Row
extract (Grid rows) = rows

replace2D :: Int -> Internal.Cell -> Grid -> Grid
replace2D i v (Grid rows) =
  let
    (Tuple x y) = Tuple (i `quot` 9) (i `mod` 9)
  in
    Grid $ Internal.replaceAt x (Internal.replaceAt y (const v)) rows

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
