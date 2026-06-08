module Sudoku.Encoding where

import Prelude

import Data.Array (all, elem, elemIndex, nub)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length, splitAt)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Sudoku.Internal.Generator (Game(..))

puzzleLength :: Int
puzzleLength = 81

keyLength :: Int
keyLength = 9

totalLength :: Int
totalLength = 90

digits :: String
digits = "123456789"

buildKey :: Game -> String -> String
buildKey Sudoku _ = digits
buildKey Colorku _ = "ROYLGBIPV"
buildKey Wordoku word = word

normalizeCell :: String -> Char -> Char
normalizeCell key cell = 
  case elemIndex cell (toCharArray key) of
    Just i -> fromMaybe '0' $ charAt i digits
    Nothing -> '0'

normalize :: String -> String -> String
normalize key puzzle = fromCharArray $ map (normalizeCell key) (toCharArray puzzle)

denormalize :: String -> String -> String
denormalize key normalized = fromCharArray $ map denormalizeCell (toCharArray normalized)
  where
    denormalizeCell '0' = '.'
    denormalizeCell c = fromMaybe '.' do
      i <- elemIndex c (toCharArray digits)
      charAt i key

encodePuzzle :: String -> String -> String
encodePuzzle normalized key = normalized <> key

decodePuzzle :: String -> Maybe { puzzle :: String, key :: String }
decodePuzzle s
  | length s == totalLength = 
      let { before: puzzle, after: key } = splitAt puzzleLength s
      in if all (\c -> c >= '0' && c <= '9') (toCharArray puzzle)
            && length (fromCharArray $ nub $ toCharArray key) == keyLength
            && not ('.' `elem` toCharArray key)
         then Just { puzzle, key }
         else Nothing
  | otherwise = Nothing

