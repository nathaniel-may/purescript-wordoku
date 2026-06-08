module Sudoku.Encoding where

import Prelude

import Data.Array (all, elemIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, splitAt)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Sudoku.Internal.Generator (Game(..))

buildKey :: Game -> String -> String
buildKey Sudoku _ = "123456789"
buildKey Colorku _ = "ROYLGBIPV"
buildKey Wordoku word = word

normalizeCell :: String -> Char -> Char
normalizeCell key cell = 
  case elemIndex cell (toCharArray key) of
    Just i -> fromMaybe '0' $ charAt i "123456789"
    Nothing -> '0'

normalize :: String -> String -> String
normalize key puzzle = fromCharArray $ map (normalizeCell key) (toCharArray puzzle)

denormalize :: String -> String -> String
denormalize key normalized = fromCharArray $ map denormalizeCell (toCharArray normalized)
  where
    denormalizeCell '0' = '.'
    denormalizeCell c = fromMaybe '.' do
      i <- elemIndex c (toCharArray "123456789")
      charAt i key

encodePuzzle :: String -> String -> String
encodePuzzle normalized key = normalized <> key

decodePuzzle :: String -> Maybe { puzzle :: String, key :: String }
decodePuzzle s
  | length s == 90 = 
      let { before: puzzle, after: key } = splitAt 81 s
      in if all (\c -> c >= '0' && c <= '9') (toCharArray puzzle)
         then Just { puzzle, key }
         else Nothing
  | otherwise = Nothing
