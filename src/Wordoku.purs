module Wordoku where

import Prelude

import Data.Array (concat, cons, delete, drop, elem, filter, foldl, index, insertAt, null, take, uncons, zip, (:), (..))
import Data.Array as Array
import Data.Char.Unicode (digitToInt)
import Data.Int (quot)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)

exPuzzle :: String
exPuzzle = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

exSolution :: String
exSolution = "693784512487512936125963874932651487568247391741398625319475268856129743274836159"

data Cell = Fixed Int | Possible (Array Int)
derive instance cellEq :: Eq Cell
instance cellShow :: Show Cell where
  show (Fixed i) = show i
  show (Possible set) = "."

type Row = Array Cell
type Grid = Array Row

showGrid :: ∀ a. Show a => Array (Array a) -> String
showGrid = joinWith "\n" <<< map (joinWith " " <<< map show)

allBut :: Int -> Cell
allBut n = Possible $ delete n (1..9)

readCell :: Char -> Maybe Cell
readCell '.' = Just $ Possible (1..9)
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

showGridWithPossibilities :: Grid -> String
showGridWithPossibilities = (joinWith "\n") <<< map ((joinWith " ") <<< map showCell)
  where
    showCell (Fixed x)     = show x <> "          "
    showCell (Possible xs) =
      (\x -> x <> "]")
      <<< foldl (\acc x -> acc <> if x `elem` xs then show x else " ") "["
      $ (1..9)

----- solver fns -----

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
        ys  -> Just (Possible ys)
    pruneCell x = Just x

transpose :: ∀ a. Array (Array a) -> Array (Array a)
transpose l = case uncons l of
  Nothing -> []
  Just { head: l', tail: xss } -> case uncons l' of
    Nothing -> transpose xss
    Just { head: x, tail: xs } ->
      (x `cons` Array.mapMaybe Array.head xss)
        `cons`
      transpose (xs `cons` Array.mapMaybe Array.tail xss)

data Tuple3 a b c = Tuple3 a b c

zip3 :: ∀ a b c d. (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
zip3 f as bs cs = case Tuple3 (uncons as) (uncons bs) (uncons cs) of
    Tuple3 (Just { head: a, tail: ast }) (Just { head: b, tail: bst }) (Just { head: c, tail: cst }) ->
        (f a b c) : (zip3 f ast bst cst)
    _ -> []

subGridsToRows :: Grid -> Grid
subGridsToRows = (=<<) 
    (\rows -> let Tuple3 r0 r1 r2 = three $ map (chunksOf 3) rows
              in zip3 (\a b c -> a <> b <> c) r0 r1 r2) <<< chunksOf 3
    where
        three [x, y, z] = Tuple3 x y z
        three _         = Tuple3 [] [] []

-- TODO this is where to add the word diagonal constraint
pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid = traverse pruneCells grid -- prune cells as rows
  >>= map transpose <<< traverse pruneCells <<< transpose -- make columns into rows, prune and replace
  >>= map subGridsToRows <<< traverse pruneCells <<< subGridsToRows -- make subgrids rows, prune and replace

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid' where 
    fixM f x = f x >>= \x' -> if x' == x then pure x else fixM f x'

------ backtracking fns ------

on :: ∀ a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)

minimumBy :: ∀ a. (a -> a -> Ordering) -> Array a -> Maybe a
minimumBy f arr = (\z -> foldl (flip $ minBy f) z.head z.tail) <$> uncons arr where 
    minBy f' x y = case f' x y of 
        GT -> y
        EQ -> x
        LT -> x

nextGrids ::Grid -> Tuple Grid Grid
nextGrids grid = fromMaybe (Tuple grid grid) $ do -- fromMaybe default is ugly. Consider putting maybe in the return type
    min <- minimumBy (compare `on` (isPossible <<< snd)) choices
    (Tuple3 i first rest) <- fixCell min
    pure $ Tuple (replace2D i first grid) (replace2D i rest grid)

    where
        isPossible :: Cell -> Boolean
        isPossible (Possible _) = true
        isPossible _            = false

        choices :: Array (Tuple Int Cell)
        choices = filter (isPossible <<< snd)
            <<< zip (0..81)
            <<< concat
            $ grid

        fixCell :: Tuple Int Cell -> Maybe (Tuple3 Int Cell Cell)
        fixCell (Tuple i (Possible [x, y])) = Just $ Tuple3 i (Fixed x) (Fixed y)
        fixCell (Tuple i (Possible xs)) = (\x -> Tuple3 i (Fixed x.head) (Possible x.tail)) <$> uncons xs
        fixCell _ = Nothing

        replaceAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
        replaceAt idx f xs = fromMaybe xs $ (\x -> insertAt idx (f x) xs) =<< index xs idx

        replace2D :: ∀ a. Int -> a -> Array (Array a) -> Array (Array a)
        replace2D i v = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
            in replaceAt x (replaceAt y (const v))
        
        
