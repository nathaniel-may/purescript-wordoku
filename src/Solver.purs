{-
Translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/


-}

module Solver where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array (all, any, concat, cons, delete, deleteAt, elem, filter, index, insertAt, length, uncons, zip, (..))
import Data.Array as Array
import Data.Either (Either(..), hush)
import Data.Foldable (foldl, minimumBy)
import Data.Int (quot)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Lib (Tuple3(..), chunksOf, isUnique, on, transpose, unique, zip3)

data CellSet = CellSet Char (Array Char)
instance cellSetShow :: Show CellSet where
    show (CellSet empty allValues) = "CellSet " <> show empty <> " " <> show allValues

data Cell = Fixed Char | Possible (Array Char)
derive instance cellEq :: Eq Cell
instance cellShow :: Show Cell where
  show (Fixed i) = fromCharArray [i]
  show (Possible set) = "."

type Row = Array Cell
type Grid = Array Row

data Variant = Standard | UniqueDiagonal

-- does not use smart constructor
numbers :: CellSet
numbers = (CellSet '.' ['1','2','3','4','5','6','7','8','9'])

-- does not use smart constructor
colors :: CellSet
colors = (CellSet '.' ['R','O','Y','L','G','B','I','P','V'])

isPossible :: Cell -> Boolean
isPossible (Possible _) = true
isPossible _            = false

isFixed :: Cell -> Boolean
isFixed (Fixed _) = true
isFixed _         = false

showGrid :: ∀ a. Show a => Array (Array a) -> String
showGrid = joinWith "\n" <<< map (joinWith " " <<< map show)

gridString :: Grid -> String
gridString = joinWith "" <<< map (joinWith "" <<< map show)

allBut :: CellSet -> Char -> Cell
allBut (CellSet _ allValues) v = Possible $ delete v allValues

mkCellSet :: Char -> Array Char -> Either String CellSet
mkCellSet empty allValues 
    | length allValues /= 9 = Left "char set must have exactly 9 characters"
    | empty `elem` allValues = Left "the empty character cannot also be in the list of values"
    | length (unique allValues) /= length allValues = Left "all characters must be unique"
    | otherwise = Right (CellSet empty allValues)

cellSetFromPuzzle :: String -> Either String CellSet
cellSetFromPuzzle str = mkCellSet '.' <<< delete '.' $ foldl 
    (\arr c -> if c `elem` arr then arr else cons c arr) 
    [] 
    (toCharArray str)

readCell :: CellSet -> Char -> Maybe Cell
readCell (CellSet empty allValues) v = 
    if v == empty 
    then Just $ Possible allValues
    else if v `elem` allValues 
        then Just (Fixed v) 
        else Nothing

readGrid :: CellSet -> String -> Maybe Grid
readGrid cellSet s =
    if CodePoints.length s /= 81
    then Nothing
    else traverse (traverse $ readCell cellSet) (chunksOf 9 $ toCharArray s)

readNumberGrid :: String -> Maybe Grid
readNumberGrid s = (\cellSet -> readGrid cellSet s) =<< (hush $ mkCellSet '.' ['1','2','3','4','5','6','7','8','9'])

showGridWithPossibilities :: CellSet -> Grid -> String
showGridWithPossibilities (CellSet _ allValues) = (joinWith "\n") <<< map ((joinWith " ") <<< map showCell)
  where
    showCell (Fixed x)     = show x <> "          "
    showCell (Possible xs) =
      (\x -> x <> "]")
      <<< foldl (\acc x -> acc <> if x `elem` xs then show x else " ") "["
      $ allValues

----- solver fns -----

pruneCells :: Array Cell -> Maybe (Array Cell)
pruneCells cells = fixM pruneCellsByExclusives =<< fixM pruneCellsByFixed cells

subGridsToRows :: Grid -> Grid
subGridsToRows = (=<<)
    (\rows -> let Tuple3 r0 r1 r2 = three $ map (chunksOf 3) rows
              in zip3 (\a b c -> a <> b <> c) r0 r1 r2) <<< chunksOf 3
    where
        three [x, y, z] = Tuple3 x y z
        three _         = Tuple3 [] [] []

diagIdxs :: Array Int
diagIdxs = map ((*) 10) (0..8)

-- from translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-2/
exclusivePossibilities :: Row -> Array (Array Char)
exclusivePossibilities = Array.fromFoldable <<< Map.values
    <<< Map.filterWithKey (\is xs -> length is == length xs)
    <<< foldl (\acc (Tuple k v) -> Map.insertWith (<>) v [k] acc) Map.empty
    <<< (\m -> List.fromFoldable (Map.keys m) `List.zip` Map.values m)
    <<< Map.filter ((\x -> x < 4) <<< length)
    <<< foldl 
        (\acc (Tuple i xs) -> foldl (\acc' x -> Map.insertWith (<>) x [i] acc') acc xs) 
        Map.empty
    <<< bindFlipped (\tuple -> case tuple of -- bind as filter
        (Tuple i (Possible xs)) -> [Tuple i xs]
        (Tuple _ (Fixed _))     -> [])
    <<< zip (1..9)

makeCell :: Array Char -> Maybe Cell
makeCell []  = Nothing
makeCell [y] = Just $ Fixed y
makeCell ys  = Just $ Possible ys

pruneCellsByFixed :: Array Cell -> Maybe (Array Cell)
pruneCellsByFixed cells = traverse pruneCell cells
  where
    fixeds :: Array Char
    fixeds = cells >>= (\cell -> case cell of 
        Fixed x -> [x]
        _ -> [])

    pruneCell :: Cell -> Maybe Cell
    pruneCell (Possible xs) = makeCell (xs `Array.difference` fixeds)
    pruneCell x             = Just x

pruneCellsByExclusives :: Array Cell -> Maybe (Array Cell)
pruneCellsByExclusives cells = case exclusives of
  [] -> Just cells
  _  -> traverse pruneCell cells
  where
    exclusives :: Array (Array Char)
    exclusives    = exclusivePossibilities cells

    allExclusives :: Array Char
    allExclusives = concat exclusives

    pruneCell :: Cell -> Maybe Cell
    pruneCell cell@(Fixed _) = Just cell
    pruneCell cell@(Possible xs) = 
        if intersection `elem` exclusives
        then makeCell intersection
        else Just cell 
        where
            intersection :: Array Char
            intersection = xs `Array.intersect` allExclusives

pruneGrid' :: Variant -> Grid -> Maybe Grid
pruneGrid' UniqueDiagonal grid = pruneDiag =<< pruneGrid' Standard grid
pruneGrid' Standard grid = 
    -- prune cells as rows
    traverse pruneCells grid
    -- make columns into rows, prune and replace 
    >>= map transpose <<< traverse pruneCells <<< transpose
    -- make subgrids rows, prune and replace
    >>= map subGridsToRows <<< traverse pruneCells <<< subGridsToRows
    
pruneDiag :: Grid -> Maybe Grid
pruneDiag grid' = flip replaceDiagonal grid' <$> (pruneCells $ diagonalOf grid')

diagonalOf :: Grid -> Row
diagonalOf grid = fromMaybe [] $ traverse (index2D grid) diagIdxs

replaceDiagonal :: Row -> Grid -> Grid
replaceDiagonal row grid = foldl (\grid' (Tuple cell i) -> replace2D i cell grid') grid (row `zip` diagIdxs)

pruneGrid :: Variant -> Grid -> Maybe Grid
pruneGrid = fixM <<< pruneGrid'

fixM :: ∀ m a. Monad m => Eq a => (a -> m a) -> a -> m a
fixM f x = f x >>= \x' -> if x' == x then pure x else fixM f x'

------ backtracking fns ------

choices :: Cell -> Int
choices (Fixed _) = 0
choices (Possible xs) = length xs

nextGrids :: Grid -> Maybe (Tuple Grid Grid)
nextGrids grid = do
    min <- minimumBy (compare `on` (choices <<< snd)) possibilities
    (Tuple3 i first rest) <- fixCell min
    pure $ Tuple (replace2D i first grid) (replace2D i rest grid)

    where
        possibilities :: Array (Tuple Int Cell)
        possibilities = filter (isPossible <<< snd)
            <<< zip (0..81)
            <<< concat
            $ grid

        fixCell :: Tuple Int Cell -> Maybe (Tuple3 Int Cell Cell)
        fixCell (Tuple i (Possible [x, y])) = Just $ Tuple3 i (Fixed x) (Fixed y)
        fixCell (Tuple i (Possible xs)) = (\x -> Tuple3 i (Fixed x.head) (Possible x.tail)) <$> uncons xs
        fixCell _ = Nothing

replaceAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
replaceAt idx f xs = fromMaybe xs $ do
    x <- index xs idx
    -- intentional shaddowing to prevent accidental refrence to input
    xs <- deleteAt idx xs
    insertAt idx (f x) xs

-- replace an element by its index [0,80] in a 9x9 grid
replace2D :: ∀ a. Int -> a -> Array (Array a) -> Array (Array a)
replace2D i v = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
    in replaceAt x (replaceAt y (const v))

-- retrieve an element by its index [0,80] in a 9x9 grid
index2D :: ∀ a. Array (Array a) -> Int -> Maybe a
index2D arr i = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
    in (\arr' -> index arr' x) =<< (index arr y)

isGridFilled :: Grid -> Boolean
isGridFilled grid = all isFixed (concat grid)

isInvalidRow :: Row -> Boolean
isInvalidRow row =
    let fixeds = row >>= (\cell -> case cell of 
            Fixed x -> [x]
            _ -> [])
        emptyPossibles = (flip any) row (\cell -> case cell of 
            Possible [] -> true
            _ -> false)
    in emptyPossibles || (not isUnique $ fixeds) 

isGridInvalid :: Variant -> Grid -> Boolean
isGridInvalid UniqueDiagonal grid = 
    isInvalidRow (diagonalOf grid) 
    || isGridInvalid Standard grid
isGridInvalid Standard grid = 
    any isInvalidRow grid 
    || any isInvalidRow (transpose grid) 
    || any isInvalidRow (subGridsToRows grid)

{- 
Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.
-} 
solve :: Variant -> Grid -> Maybe Grid
solve v grid = solve' =<< (pruneGrid v $ grid) where
    solve' g
      | isGridInvalid v g = Nothing
      | isGridFilled g  = Just g
      | otherwise       = nextGrids g >>= (\(Tuple grid1 grid2) -> 
            case solve v grid1 of
                solution@(Just _) -> solution
                _ -> solve v grid2
        )

{- 
Takes in a puzzle and finds all solutions.
-} 
solveAll :: Variant -> Grid -> Array Grid
solveAll v grid = concat <<< Array.fromFoldable $ solveAll' <$> (pruneGrid v $ grid) where
    solveAll' :: Grid -> Array Grid
    solveAll' g
      | isGridInvalid v g = []
      | isGridFilled g  = [g]
      | otherwise = 
        concat <<< Array.fromFoldable $ nextGrids g <#> (\(Tuple grid1 grid2) -> 
            solveAll v grid1 <> solveAll v grid2
        )

data SearchResult
    = NoSolution
    | Unique Grid
    | NotUnique Grid Grid

data Search
    = NoSolution'
    | NotUnique' Grid Grid
    | AtLeast' Grid

{- 
Takes in a puzzle, determines if it has a solution, and if that solution is unique:
- Nothing = puzzle has no solution
- Just (Left x) = Grid x is the only unique solution
- Just (Right (Tuple x y)) = x and y are both valid solutions so this puzzle does not have a unique solution

To determine uniqueness, it must attempt to visit every solution in the space and find all but one invalid
or exit early when it finds a second solution. For a fast single solution use `solve`.
-} 
solveUnique :: Variant -> Grid -> SearchResult
solveUnique v grid = searchResult $ solveUnique' v grid where

    solveUnique' :: Variant -> Grid -> Search
    solveUnique' v g = fromMaybe NoSolution'
        $ solve2 v <$> (pruneGrid v g)

    solve2 :: Variant -> Grid -> Search
    solve2 v g
        | isGridInvalid v g = NoSolution'
        | isGridFilled g  = AtLeast' g
        | otherwise       = case nextGrids g of
            Nothing -> NoSolution'
            (Just (Tuple grid1 grid2)) -> case solveUnique' v grid1 of
                x@(NotUnique' _ _) -> x
                x -> x `mergeSearch` (solveUnique' v grid2)

    searchResult :: Search -> SearchResult
    searchResult NoSolution' = NoSolution
    searchResult (NotUnique' s1 s2) = NotUnique s1 s2
    searchResult (AtLeast' s) = Unique s

    mergeSearch :: Search -> Search -> Search
    mergeSearch NoSolution' y = y
    mergeSearch x NoSolution' = x
    mergeSearch x@(NotUnique' _ _) _ = x
    mergeSearch _ y@(NotUnique' _ _) = y
    mergeSearch (AtLeast' s1) (AtLeast' s2) = (NotUnique' s1 s2)