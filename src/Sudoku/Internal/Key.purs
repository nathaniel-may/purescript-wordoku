module Sudoku.Internal.Key
  ( Key
  , mkKey
  , toChar
  , fromChar
  , sudokuKey
  , colorkuKey
  ) where

import Prelude

import Data.Array (length, nub, zip)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Data.String.CodeUnits (toCharArray)
import Sudoku.Internal (Value, allValues)

newtype Key = Key { toChar :: Map Value Char, fromChar :: Map Char Value }

mkKey :: String -> Either String Key
mkKey s =
  let
    chars = toCharArray s
  in
    if length chars /= 9 then Left "key must be exactly 9 characters"
    else if length (nub chars) /= 9 then Left "key characters must be unique"
    else Right $ Key
      { toChar: Map.fromFoldable (allValues `zip` chars)
      , fromChar: Map.fromFoldable (chars `zip` allValues)
      }

toChar :: Key -> Value -> Char
toChar (Key k) v = case Map.lookup v k.toChar of
  Just c -> c
  Nothing -> unsafeCrashWith "Key.toChar: unreachable — allValues must cover all Value constructors"

fromChar :: Key -> Char -> Maybe Value
fromChar (Key k) c = Map.lookup c k.fromChar

sudokuKey :: Key
sudokuKey = case mkKey "123456789" of
  Right k -> k
  Left err -> unsafeCrashWith err

colorkuKey :: Key
colorkuKey = case mkKey "ROYLGBIPV" of
  Right k -> k
  Left err -> unsafeCrashWith err
