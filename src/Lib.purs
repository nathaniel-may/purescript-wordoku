module Lib where

import Prelude
import Data.Array (cons, drop, null, take, uncons, (:))
import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Foldable (foldl)


data Tuple3 a b c = Tuple3 a b c

-- returns only the unique elements from the set
unique :: ∀ a. Eq a => Ord a => Array a -> Array a
unique xs = Array.fromFoldable <<< Map.keys $ foldl (\m x -> Map.insert x unit m) Map.empty xs

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