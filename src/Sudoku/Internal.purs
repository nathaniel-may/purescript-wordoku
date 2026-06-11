module Sudoku.Internal where

import Prelude
import Data.Array (cons, deleteAt, drop, elem, index, insertAt, length, null, take, uncons, zip, (:), (..))
import Data.Array as Array
import Effect (Effect)
import Effect.Random (randomInt)
import Data.Either (Either(..))
import Data.Int (quot)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Foldable (foldl)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicate)

------------------------------------------------------------------
-- functions and data types for reading and showing sudoku puzzles
------------------------------------------------------------------

data Value = V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9

derive instance eqValue :: Eq Value
derive instance ordValue :: Ord Value

allValues :: Array Value
allValues = [ V1, V2, V3, V4, V5, V6, V7, V8, V9 ]

data Cell = Fixed Value | Possible (Array Value)

derive instance cellEq :: Eq Cell
instance cellShow :: Show Cell where
  show (Fixed _) = "."
  show (Possible _) = "."

type Row = Array Cell

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
  map _ NoSolution = NoSolution
  map f (Unique x) = Unique (f x)
  map f (NotUnique x y) = NotUnique (f x) (f y)

----------------------------------------------
-- functions useful for solving and generation
----------------------------------------------

-- retrieve an element by its index [0,80] in a 9x9 grid
index2D :: ∀ a. Array (Array a) -> Int -> Maybe a
index2D arr i =
  let
    (Tuple x y) = (Tuple (i `quot` 9) (i `mod` 9))
  in
    (\arr' -> index arr' x) =<< (index arr y)

replaceAt :: ∀ a. Int -> (a -> a) -> Array a -> Array a
replaceAt idx f xs = fromMaybe xs $ do
  x <- index xs idx
  xs' <- deleteAt idx xs
  insertAt idx (f x) xs'

----------------------------------------------
-- functions useful for testing and generation
----------------------------------------------

emptySudoku :: String
emptySudoku = fromCharArray $ replicate 81 '.'

diagonalString :: String -> String
diagonalString str = foldl onlyDiag "" $ toCharArray str `zip` (0 .. 80)
  where

  onlyDiag :: String -> Tuple Char Int -> String
  onlyDiag s (Tuple c i) = if i `elem` idxs then s <> singleton c else s

  idxs :: Array Int
  idxs = map (\x -> 10 * x) (0 .. 8)

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
isUnique' arr seen = fromMaybe true $ f <$> uncons arr
  where
  f x = if isJust $ x.head `Map.lookup` seen then false else isUnique' x.tail (Map.insert x.head unit seen)

chunksOf :: ∀ a. Int -> Array a -> Array (Array a)
chunksOf n xs =
  if null (drop n xs) then pure xs
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
toEither Nothing x = Left x
toEither (Just x) _ = Right x

-- fisher yates
randomArray :: ∀ a. Array a -> Effect (Array a)
randomArray input = do
  k <- randomInt 0 (length input - 1)
  fromMaybe (pure []) do -- indicies will always match
    v <- index input k
    arr <- deleteAt k input
    pure $ cons v <$> (randomArray arr)
