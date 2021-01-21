module Sudoku.Internal where

import Prelude
import Data.Array (cons, delete, deleteAt, drop, elem, index, insertAt, length, null, take, uncons, zip, (:), (..))
import Data.Array as Array
import Effect (Effect)
import Effect.Random (randomInt)
import Data.Either (Either(..))
import Data.Int (quot)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Foldable (foldl)
import Data.String (joinWith)
import Data.String.CodePoints as CodePoints
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)


------------------------------------------------------------------
-- functions and data types for reading and showing sudoku puzzles
------------------------------------------------------------------

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

data SearchResult a
    = NoSolution
    | Unique a
    | NotUnique a a
instance searchResultShow :: Show a => Show (SearchResult a) where
    show NoSolution = "NoSolution"
    show (Unique s) = "(Unique " <> show s <> ")"
    show (NotUnique s1 s2) = "(Unique " <> show s1 <> " " <> show s2 <> " " <> ")"
instance functorSearchResult :: Functor SearchResult where
    map f NoSolution      = NoSolution
    map f (Unique x)      = Unique (f x)
    map f (NotUnique x y) = NotUnique (f x) (f y)

data Search
    = NoSolution'
    | NotUnique' Grid Grid
    | AtLeast' Grid

-- does not use smart constructor
numbers :: CellSet
numbers = (CellSet '.' ['1','2','3','4','5','6','7','8','9'])

-- does not use smart constructor
colors :: CellSet
colors = (CellSet '.' ['R','O','Y','L','G','B','I','P','V'])

showGrid :: ∀ a. Show a => Array (Array a) -> String
showGrid = joinWith "\n" <<< map (joinWith " " <<< map show)

gridString :: Grid -> String
gridString = joinWith "" <<< map (joinWith "" <<< map show)

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

readCell :: CellSet -> Char -> Either String Cell
readCell (CellSet empty allValues) v = 
    if v == empty 
    then Right $ Possible allValues
    else if v `elem` allValues 
        then Right (Fixed v) 
        else Left $ "char " <> show v <> "is not one of " <> show allValues 

readGrid :: CellSet -> String -> Either String Grid
readGrid cellSet s =
    if CodePoints.length s /= 81
    then Left "input must be exactly 81 characters long"
    else traverse (traverse $ readCell cellSet) (chunksOf 9 $ toCharArray s)

readNumberGrid :: String -> Either String Grid
readNumberGrid s = (\cellSet -> readGrid cellSet s) =<< mkCellSet '.' ['1','2','3','4','5','6','7','8','9']

showGridWithPossibilities :: CellSet -> Grid -> String
showGridWithPossibilities (CellSet _ allValues) = (joinWith "\n") <<< map ((joinWith " ") <<< map showCell)
  where
    showCell (Fixed x)     = show x <> "          "
    showCell (Possible xs) =
      (\x -> x <> "]")
      <<< foldl (\acc x -> acc <> if x `elem` xs then show x else " ") "["
      $ allValues

----------------------------------------------
-- functions useful for solving and generation
----------------------------------------------

-- replace an element by its index [0,80] in a 9x9 grid
replace2D :: ∀ a. Int -> a -> Array (Array a) -> Array (Array a)
replace2D i v = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
    in replaceAt x (replaceAt y (const v))

-- retrieve an element by its index [0,80] in a 9x9 grid
index2D :: ∀ a. Array (Array a) -> Int -> Maybe a
index2D arr i = let (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9)) 
    in (\arr' -> index arr' x) =<< (index arr y)

replaceAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
replaceAt idx f xs = fromMaybe xs $ do
    x <- index xs idx
    -- intentional shaddowing to prevent accidental refrence to input
    xs <- deleteAt idx xs
    insertAt idx (f x) xs

----------------------------------------------
-- functions useful for testing and generation
----------------------------------------------

emptySudoku :: String
emptySudoku = fromCharArray $ replicate 81 '.'

diagonalOf :: String -> String
diagonalOf str = foldl onlyDiag "" $ toCharArray str `zip` (0..80) where

    onlyDiag :: String -> Tuple Char Int -> String
    onlyDiag s (Tuple c i) = if i `elem` idxs then s <> singleton c else s

    idxs :: Array Int
    idxs = map (\x -> 10 * x) (0..8)

------------------------------------------------
-- generally applicable functions and data types
------------------------------------------------

data Tuple3 a b c = Tuple3 a b c

-- returns only the unique elements from the set
unique :: ∀ a. Eq a => Ord a => Array a -> Array a
unique xs = Array.fromFoldable <<< Map.keys $ foldl (\m x -> Map.insert x unit m) Map.empty xs

isUnique :: ∀ a. Eq a => Ord a => Array a -> Boolean
isUnique l = isUnique' l Map.empty

isUnique' :: ∀ a. Eq a => Ord a => Array a -> Map a Unit -> Boolean
isUnique' arr seen = fromMaybe true $ f <$> uncons arr where
    f x = if isJust $ x.head `Map.lookup` seen then false else isUnique' x.tail (Map.insert x.head unit seen)

chunksOf :: ∀ a. Int -> Array a -> Array (Array a)
chunksOf n xs =
    if null (drop n xs)
    then pure xs
    else (take n xs) `cons` chunksOf n (drop n xs)

transpose :: ∀ a. Array (Array a) -> Array (Array a)
transpose l = case uncons l of
  Nothing -> []
  Just { head: l', tail: xss } -> case uncons l' of
    Nothing -> transpose xss
    Just { head: x, tail: xs } ->
      (x `cons` Array.mapMaybe Array.head xss)
        `cons`
      transpose (xs `cons` Array.mapMaybe Array.tail xss)

zip3 :: ∀ a b c d. (a -> b -> c -> d) -> Array a -> Array b -> Array c -> Array d
zip3 f as bs cs = case Tuple3 (uncons as) (uncons bs) (uncons cs) of
    Tuple3 (Just { head: a, tail: ast }) (Just { head: b, tail: bst }) (Just { head: c, tail: cst }) ->
        (f a b c) : (zip3 f ast bst cst)
    _ -> []

on :: ∀ a b c. (b -> b -> c) -> (a -> b) -> a -> a -> c
on g f = \x y -> g (f x) (f y)

toEither :: ∀ a b. Maybe b -> a -> Either a b
toEither Nothing  x = Left x
toEither (Just x) _ = Right x

-- fisher yates
randomArray :: ∀ a. Array a -> Effect (Array a)
randomArray input = do
    k <- randomInt 0 (length input - 1)
    fromMaybe (pure []) do -- indicies will always match
        v <- index input k
        arr <- deleteAt k input
        pure $ cons v <$> (randomArray arr)