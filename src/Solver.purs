{-
Translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/


-}

module Solver where

import Prelude

import Data.Array (all, any, concat, delete, deleteAt, elem, filter, index, insertAt, length, uncons, zip, (:), (..))
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
import Lib (Tuple3(..), chunksOf, on, transpose, unique, zip3)

numberPuzzle :: String
numberPuzzle = "6......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

wordPuzzle :: String
wordPuzzle = ".nt...ps..y..shni....c.nyo..............o...i.h.s.tc........i.n.......c.....nyops"

exSolution :: String
exSolution = "693784512487512936125963874932651487568247391741398625319475268856129743274836159"

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

-- TODO check usage of set vs array here
pruneCells :: Array Cell -> Maybe (Array Cell)
pruneCells cells = fixM pruneCellsByExclusives =<< fixM pruneCellsByFixed cells

subGridsToRows :: Grid -> Grid
subGridsToRows = (=<<)
    (\rows -> let Tuple3 r0 r1 r2 = three $ map (chunksOf 3) rows
              in zip3 (\a b c -> a <> b <> c) r0 r1 r2) <<< chunksOf 3
    where
        three [x, y, z] = Tuple3 x y z
        three _         = Tuple3 [] [] []

-- from translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-2/
exclusivePossibilities :: Row -> Array (Array Char)
exclusivePossibilities = Array.fromFoldable <<< Map.values
    <<< Map.filterWithKey (\is xs -> length is == length xs)
    <<< foldl (\acc (Tuple k v) -> Map.insertWith (<>) v [k] acc) Map.empty
    <<< (\m -> List.fromFoldable (Map.keys m) `List.zip` Map.values m)
    <<< Map.filter ((\x -> x < 4) <<< length)
    <<< foldl
      (\acc tuple -> case tuple of
        (Tuple i (Possible xs)) -> foldl (\acc' x -> Map.insertWith (<>) x [i] acc') acc xs
        _ -> acc) -- won't get here since we previously filtered for `Possible` values
      Map.empty
    <<< filter (isPossible <<< snd)
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

-- TODO this is where to add the word diagonal constraint
pruneGrid' :: Grid -> Maybe Grid
pruneGrid' grid = traverse pruneCells grid -- prune cells as rows
  >>= map transpose <<< traverse pruneCells <<< transpose -- make columns into rows, prune and replace
  >>= map subGridsToRows <<< traverse pruneCells <<< subGridsToRows -- make subgrids rows, prune and replace

fixM :: ∀ m a. Monad m => Eq a => (a -> m a) -> a -> m a
fixM f x = f x >>= \x' -> if x' == x then pure x else fixM f x'

pruneGrid :: Grid -> Maybe Grid
pruneGrid = fixM pruneGrid'

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
    xs <- deleteAt idx xs
    insertAt idx (f x) xs

replace2D :: ∀ a. Int -> a -> Array (Array a) -> Array (Array a)
replace2D i v = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
    in replaceAt x (replaceAt y (const v))

isGridFilled :: Grid -> Boolean
isGridFilled grid = all isFixed (concat grid)

isGridInvalid :: Grid -> Boolean
isGridInvalid grid = any isInvalidRow grid
  || any isInvalidRow (transpose grid)
  || any isInvalidRow (subGridsToRows grid)
  where
    isInvalidRow :: Row -> Boolean
    isInvalidRow row =
        let fixeds = row >>= (\cell -> case cell of 
                Fixed x -> [x]
                _ -> [])
            emptyPossibles = (flip any) row (\cell -> case cell of 
                Possible [] -> true
                _ -> false)
      in emptyPossibles || hasDups fixeds

    hasDups :: ∀ a. Eq a => Array a -> Boolean
    hasDups l = hasDups' l []

    hasDups' :: ∀ a. Eq a => Array a -> Array a -> Boolean
    hasDups' arr seen = fromMaybe false $ f <$> uncons arr where
        f x = if x.head `elem` seen then true else hasDups' x.tail (x.head : seen)

{- 
Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.
-} 
solve :: Grid -> Maybe Grid
solve grid = solve' =<< pruneGrid grid where
    solve' g
      | isGridInvalid g = Nothing
      | isGridFilled g  = Just g
      | otherwise       = nextGrids g >>= (\(Tuple grid1 grid2) -> 
            case solve grid1 of
                solution@(Just _) -> solution
                _ -> solve grid2
        )

{- 
Takes in a puzzle and finds all solutions.
-} 
solveAll :: Grid -> Array Grid
solveAll grid = concat <<< Array.fromFoldable $ solveAll' <$> pruneGrid grid where
    solveAll' :: Grid -> Array Grid
    solveAll' g
      | isGridInvalid g = []
      | isGridFilled g  = [g]
      | otherwise       = 
        concat <<< Array.fromFoldable $ nextGrids g <#> (\(Tuple grid1 grid2) -> 
            solveAll grid1 <> solveAll grid2
        )

{- 
Takes in a puzzle, determines if it has a solution, and if that solution is unique:
- Nothing = puzzle has no solution
- Just (Left x) = Grid x is the only unique solution
- Just (Right (Tuple x y)) = x and y are both valid solutions so this puzzle does not have a unique solution

To determine uniqueness, it must attempt to visit every solution in the space and find all but one invalid
or exit early when it finds a second solution. For a fast single solution use `solve`.
-} 
solveUnique :: Grid -> Maybe (Either Grid (Tuple Grid Grid))
solveUnique grid = toSolution <<< solve2 =<< pruneGrid grid where
    solve2 :: Grid -> Tuple (Maybe Grid) (Maybe Grid)
    solve2 g
        | isGridInvalid g = Tuple Nothing Nothing
        | isGridFilled g  = Tuple (Just g) Nothing
        | otherwise       = case nextGrids g of
            Nothing -> Tuple Nothing Nothing
            (Just (Tuple grid1 grid2)) -> case solve2 grid1 of
                x@(Tuple (Just _) (Just _)) -> x
                y -> y `fillWith` (solve2 grid2)

    toSolution :: Tuple (Maybe Grid) (Maybe Grid) -> Maybe (Either Grid (Tuple Grid Grid))
    toSolution (Tuple Nothing  Nothing)  = Nothing
    toSolution (Tuple (Just x) Nothing)  = Just (Left x)
    toSolution (Tuple Nothing  (Just y)) = Just (Left y)
    toSolution (Tuple (Just x) (Just y)) = Just (Right (Tuple x y))
    
    fillWith :: Tuple (Maybe Grid) (Maybe Grid) -> Tuple (Maybe Grid) (Maybe Grid) -> Tuple (Maybe Grid) (Maybe Grid)
    fillWith (Tuple Nothing Nothing) y = y
    fillWith x (Tuple Nothing Nothing) = x
    fillWith x@(Tuple (Just _) (Just _)) _ = x
    fillWith _ y@(Tuple (Just _) (Just _)) = y
    fillWith (Tuple (Just a) Nothing) (Tuple (Just b) Nothing) = (Tuple (Just a) (Just b))
    fillWith (Tuple (Just a) Nothing) (Tuple Nothing (Just b)) = (Tuple (Just a) (Just b))
    fillWith (Tuple Nothing (Just a) ) (Tuple (Just b) Nothing) = (Tuple (Just a) (Just b))
    fillWith (Tuple Nothing (Just a) ) (Tuple Nothing (Just b)) = (Tuple (Just a) (Just b))
