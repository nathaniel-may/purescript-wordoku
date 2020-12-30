module Wordoku where

import Prelude

import Data.Array (cons, drop, foldl, null, take, (..))
import Data.Array as Array
import Data.Char.Unicode (digitToInt)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)

exPuzzle :: String
exPuzzle = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

data Cell = Fixed Int | Possible (Set Int)
derive instance cellEq :: Eq Cell
instance cellShow :: Show Cell where
  show (Fixed i) = show i
  show (Possible set) = "."

type Row = Array Cell
type Grid = Array Row

showGrid :: ∀ a. Show a => Array (Array a) -> String
showGrid = joinWith "\n" <<< map (joinWith " " <<< map show)

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

----- solver fns -----

extractOne :: ∀ a. Set a -> Maybe a
extractOne set = foldl (\_ a -> Just a) Nothing set

-- TODO check usage of set vs array here
pruneCells :: Array Cell -> Maybe (Array Cell)
pruneCells cells = traverse pruneCell cells
  where
    fixed :: Cell -> Array Int
    fixed (Fixed x) = [x]
    fixed _ = []

    diff :: Array Int -> Array Int
    diff xs = xs `Array.difference` (fixed =<< cells)

    pruneCell :: Cell -> Maybe Cell
    pruneCell (Possible xs) = case (diff $ Array.fromFoldable xs) of
        []  -> Nothing
        [y] -> Just $ Fixed y
        ys  -> Just (Possible $ Set.fromFoldable ys)
    pruneCell x = Just x