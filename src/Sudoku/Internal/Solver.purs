-- | internal module for solving sudoku puzzles
-- | Logic translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/

module Sudoku.Internal.Solver where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array (all, any, concat, elem, filter, length, uncons, zip, (..))
import Data.Array as Array
import Data.Foldable (foldl, minimumBy)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)
import Sudoku.Internal (Cell(..), SearchResult(..), Tuple3(..), Value, Variant(..), isUnique, on)
import Sudoku.Internal.Grid (Grid, diagonalOf, extract, replace2D, replaceDiagonal, subGridsToRows, traverseRows, transpose)

data Search
  = NoSolution'
  | NotUnique' Grid Grid
  | AtLeast' Grid

isPossible :: Cell -> Boolean
isPossible (Possible _) = true
isPossible _ = false

isFixed :: Cell -> Boolean
isFixed (Fixed _) = true
isFixed _ = false

----- solver fns -----

pruneCells :: Array Cell -> Maybe (Array Cell)
pruneCells cells = fixM pruneCellsByExclusives =<< fixM pruneCellsByFixed cells

-- from translated from https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-2/
exclusivePossibilities :: Array Cell -> Array (Array Value)
exclusivePossibilities = Array.fromFoldable <<< Map.values
  <<< Map.filterWithKey (\is xs -> length is == length xs)
  <<< foldl (\acc (Tuple k v) -> Map.insertWith (<>) v [ k ] acc) Map.empty
  <<< (\m -> List.fromFoldable (Map.keys m) `List.zip` Map.values m)
  <<< Map.filter ((\x -> x < 4) <<< length)
  <<< foldl
    (\acc (Tuple i xs) -> foldl (\acc' x -> Map.insertWith (<>) x [ i ] acc') acc xs)
    Map.empty
  <<< bindFlipped
    ( \tuple -> case tuple of -- bind as filter
        (Tuple i (Possible xs)) -> [ Tuple i xs ]
        (Tuple _ (Fixed _)) -> []
    )
  <<< zip (1 .. 9)

makeCell :: Array Value -> Maybe Cell
makeCell [] = Nothing
makeCell [ y ] = Just $ Fixed y
makeCell ys = Just $ Possible ys

pruneCellsByFixed :: Array Cell -> Maybe (Array Cell)
pruneCellsByFixed cells = traverse pruneCell cells
  where
  fixeds :: Array Value
  fixeds = cells >>=
    ( \cell -> case cell of
        Fixed x -> [ x ]
        _ -> []
    )

  pruneCell :: Cell -> Maybe Cell
  pruneCell (Possible xs) = makeCell (xs `Array.difference` fixeds)
  pruneCell x = Just x

pruneCellsByExclusives :: Array Cell -> Maybe (Array Cell)
pruneCellsByExclusives cells = case exclusives of
  [] -> Just cells
  _ -> traverse pruneCell cells
  where
  exclusives :: Array (Array Value)
  exclusives = exclusivePossibilities cells

  allExclusives :: Array Value
  allExclusives = concat exclusives

  pruneCell :: Cell -> Maybe Cell
  pruneCell cell@(Fixed _) = Just cell
  pruneCell cell@(Possible xs) =
    if intersection `elem` exclusives then makeCell intersection
    else Just cell
    where
    intersection :: Array Value
    intersection = xs `Array.intersect` allExclusives

pruneGrid' :: Variant -> Grid -> Maybe Grid
pruneGrid' UniqueDiagonal grid = pruneDiag =<< pruneGrid' Standard grid
pruneGrid' Standard grid =
  -- prune cells as rows
  traverseRows pruneCells grid
    -- make columns into rows, prune and replace
    >>= map transpose <<< traverseRows pruneCells <<< transpose
    -- make subgrids rows, prune and replace
    >>= map subGridsToRows <<< traverseRows pruneCells <<< subGridsToRows

pruneDiag :: Grid -> Maybe Grid
pruneDiag grid' = flip replaceDiagonal grid' <$> (pruneCells $ diagonalOf grid')

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
  possibilities =
    filter (isPossible <<< snd)
      <<< zip (0 .. 81)
      <<< concat
      $ extract grid

  fixCell :: Tuple Int Cell -> Maybe (Tuple3 Int Cell Cell)
  fixCell (Tuple i (Possible [ x, y ])) = Just $ Tuple3 i (Fixed x) (Fixed y)
  fixCell (Tuple i (Possible xs)) = (\x -> Tuple3 i (Fixed x.head) (Possible x.tail)) <$> uncons xs
  fixCell _ = Nothing

isGridFilled :: Grid -> Boolean
isGridFilled grid = all isFixed (concat $ extract grid)

isInvalidRow :: Array Cell -> Boolean
isInvalidRow row =
  let
    fixeds :: Array Value
    fixeds = row >>=
      ( \cell -> case cell of
          Fixed x -> [ x ]
          _ -> []
      )
    emptyPossibles = (flip any) row
      ( \cell -> case cell of
          Possible [] -> true
          _ -> false
      )
  in
    emptyPossibles || (not isUnique $ fixeds)

isGridInvalid :: Variant -> Grid -> Boolean
isGridInvalid UniqueDiagonal grid =
  isInvalidRow (diagonalOf grid)
    || isGridInvalid Standard grid
isGridInvalid Standard grid =
  any isInvalidRow (extract grid)
    || any isInvalidRow (extract $ transpose grid)
    || any isInvalidRow (extract $ subGridsToRows grid)

{-
Takes in a puzzle, finds the first of possibly many solutions with a depth-first search of the solution space.
-}
solve :: Variant -> Grid -> Maybe Grid
solve v grid = solve' =<< (pruneGrid v $ grid)
  where
  solve' g
    | isGridInvalid v g = Nothing
    | isGridFilled g = Just g
    | otherwise = nextGrids g >>=
        ( \(Tuple grid1 grid2) ->
            case solve v grid1 of
              solution@(Just _) -> solution
              _ -> solve v grid2
        )

{-
Takes in a puzzle and finds all solutions.
-}
solveAll :: Variant -> Grid -> Array Grid
solveAll v grid = concat <<< Array.fromFoldable $ solveAll' <$> (pruneGrid v $ grid)
  where
  solveAll' :: Grid -> Array Grid
  solveAll' g
    | isGridInvalid v g = []
    | isGridFilled g = [ g ]
    | otherwise =
        concat <<< Array.fromFoldable $ nextGrids g <#>
          ( \(Tuple grid1 grid2) ->
              solveAll v grid1 <> solveAll v grid2
          )

{-
Takes in a puzzle, determines if it has a solution, and if that solution is unique:
- Nothing = puzzle has no solution
- Just (Left x) = Grid x is the only unique solution
- Just (Right (Tuple x y)) = x and y are both valid solutions so this puzzle does not have a unique solution

To determine uniqueness, it must attempt to visit every solution in the space and find all but one invalid
or exit early when it finds a second solution. For a fast single solution use `solve`.
-}
solveUnique :: Variant -> Grid -> SearchResult Grid
solveUnique v grid = searchResult $ solveUnique' grid
  where

  solveUnique' :: Grid -> Search
  solveUnique' g = fromMaybe NoSolution'
    $ solve2 <$> (pruneGrid v g)

  solve2 :: Grid -> Search
  solve2 g
    | isGridInvalid v g = NoSolution'
    | isGridFilled g = AtLeast' g
    | otherwise = case nextGrids g of
        Nothing -> NoSolution'
        (Just (Tuple grid1 grid2)) -> case solveUnique' grid1 of
          x@(NotUnique' _ _) -> x
          x -> x `mergeSearch` (solveUnique' grid2)

  searchResult :: Search -> SearchResult Grid
  searchResult NoSolution' = NoSolution
  searchResult (NotUnique' s1 s2) = NotUnique s1 s2
  searchResult (AtLeast' s) = Unique s

  mergeSearch :: Search -> Search -> Search
  mergeSearch NoSolution' y = y
  mergeSearch x NoSolution' = x
  mergeSearch x@(NotUnique' _ _) _ = x
  mergeSearch _ y@(NotUnique' _ _) = y
  mergeSearch (AtLeast' s1) (AtLeast' s2) = (NotUnique' s1 s2)
