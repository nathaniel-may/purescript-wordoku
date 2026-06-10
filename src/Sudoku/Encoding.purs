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

data DecodedKey
  = SudokuKey
  | ColorkuKey
  | WordokuKey String

derive instance eqDecodedKey :: Eq DecodedKey

instance showDecodedKey :: Show DecodedKey where
  show SudokuKey = "SudokuKey"
  show ColorkuKey = "ColorkuKey"
  show (WordokuKey w) = "WordokuKey " <> w

keyToString :: DecodedKey -> String
keyToString SudokuKey = digits
keyToString ColorkuKey = "ROYLGBIPV"
keyToString (WordokuKey w) = w

parseKey :: Game -> String -> Maybe DecodedKey
parseKey Sudoku k | k == digits = Just SudokuKey
parseKey Colorku k | k == "ROYLGBIPV" = Just ColorkuKey
parseKey Wordoku k
  | length k == keyLength
      && length (fromCharArray $ nub $ toCharArray k) == keyLength
      && not ('.' `elem` toCharArray k)
      && all (\c -> c >= 'a' && c <= 'z') (toCharArray k) = Just (WordokuKey k)
parseKey _ _ = Nothing

normalizeCell :: DecodedKey -> Char -> Char
normalizeCell dk cell =
  let
    key = keyToString dk
  in
    case elemIndex cell (toCharArray key) of
      Just i -> fromMaybe '0' $ charAt i digits
      Nothing -> '0'

normalize :: DecodedKey -> String -> String
normalize dk puzzle = fromCharArray $ map (normalizeCell dk) (toCharArray puzzle)

denormalize :: DecodedKey -> String -> String
denormalize dk normalized = fromCharArray $ map denormalizeCell (toCharArray normalized)
  where
  key = keyToString dk
  denormalizeCell '0' = '.'
  denormalizeCell c = fromMaybe '.' do
    i <- elemIndex c (toCharArray digits)
    charAt i key

encodePuzzle :: String -> DecodedKey -> String
encodePuzzle normalized key = normalized <> keyToString key

decodePuzzle :: Game -> String -> Maybe { puzzle :: String, key :: DecodedKey }
decodePuzzle g s
  | length s == totalLength =
      let
        { before: puzzle, after: keyStr } = splitAt puzzleLength s
      in
        if all (\c -> c >= '0' && c <= '9') (toCharArray puzzle) then { puzzle, key: _ } <$> parseKey g keyStr
        else Nothing
  | otherwise = Nothing
