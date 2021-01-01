module Wordoku where

import Prelude

import Data.Array (all, any, concat, cons, delete, deleteAt, drop, elem, filter, foldl, index, insertAt, length, null, take, uncons, zip, (:), (..))
import Data.Array as Array
import Data.Char.Unicode (digitToInt)
import Data.Either (Either(..))
import Data.Int (quot)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), snd)

exPuzzle :: String
exPuzzle = ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

exPuzzle2 :: String
exPuzzle2 = "6......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."

exSolution :: String
exSolution = "693784512487512936125963874932651487568247391741398625319475268856129743274836159"

data Cell = Fixed Int | Possible (Array Int)
derive instance cellEq :: Eq Cell
instance cellShow :: Show Cell where
  show (Fixed i) = show i
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

choices :: Cell -> Int
choices (Fixed _) = 0
choices (Possible xs) = length xs

on :: ∀ a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)

minimumBy :: ∀ a. (a -> a -> Ordering) -> Array a -> Maybe a
minimumBy f arr = (\z -> foldl (minBy f) z.head z.tail) <$> uncons arr where 
    minBy f' x y = case f' x y of -- keeps the first one if equal
        GT -> y
        EQ -> x
        LT -> x

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
                y -> y `fillWith` (solve2' grid2)

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
    
    