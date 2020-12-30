module Wordoku where

import Data.Array (drop, cons, null, take, (..))
import Data.Char.Unicode (digitToInt)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Prelude

exPuzzle :: String
exPuzzle = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

data Cell = Fixed Int | Possible (Set Int)
derive instance cellEq :: Eq Cell

type Row = Array Cell
type Grid = Array Row

allSet :: Set Int
allSet = Set.fromFoldable (1..9)

allBut :: Int -> Cell
allBut n = Possible $ Set.delete n allSet

readCell :: Char -> Maybe Cell
readCell '.' = Just $ Possible allSet
readCell str = (\x -> if x >= 1 && x <= 9 then Just (Fixed x) else Nothing) =<< digitToInt str

chunksOf :: ∀ a. Int -> Array a -> Array (Array a)
chunksOf n xs =
    if null (drop n xs)
    then pure xs
    else (take n xs) `cons` chunksOf n (drop n xs)

readGrid :: String -> Maybe Grid
readGrid s =
    if CodePoints.length s /= 81
    then Nothing
    else traverse (traverse readCell) (chunksOf 9 $ toCharArray s)