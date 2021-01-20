module Sudoku.Internal where

import Prelude
import Data.Array (cons, drop, null, take, uncons, (:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Foldable (foldl)


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