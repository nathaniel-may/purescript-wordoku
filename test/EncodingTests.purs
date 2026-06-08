module Test.EncodingTests where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Sudoku.Encoding (DecodedKey(..), decodePuzzle, denormalize, encodePuzzle, normalize, normalizeCell)
import Sudoku.Internal.Generator (Game(..))
import Test.QuickCheck (Result, (<?>))

encodingTests :: Array Result
encodingTests = 
    [ (normalizeCell ColorkuKey '.' == '0') <?> "normalizeCell '.' -> '0' failed"
    , (normalizeCell ColorkuKey 'R' == '1') <?> "normalizeCell 'R' -> '1' failed"
    , (normalizeCell ColorkuKey 'V' == '9') <?> "normalizeCell 'V' -> '9' failed"
    , (normalizeCell ColorkuKey 'X' == '0') <?> "normalizeCell unknown -> '0' failed"
    
    , let k = WordokuKey "countries"
          p = "c.o.u.n.t.r.i.e.s................................................................"
          norm = normalize k p
          denorm = denormalize k norm
      in (denorm == p) <?> "Normalize/Denormalize round-trip failed"

    , let k = SudokuKey
          p = "123456789123456789123456789123456789123456789123456789123456789123456789123456789"
          norm = normalize k p
          encoded = encodePuzzle norm k
          decoded = decodePuzzle Sudoku encoded
      in (decoded == Just { puzzle: norm, key: k }) <?> "Encode/Decode round-trip failed"

    , (decodePuzzle Sudoku "" == Nothing) <?> "decodePuzzle empty failed"
    , (decodePuzzle Sudoku "abc" == Nothing) <?> "decodePuzzle short failed"
    
    , let repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)
      in (decodePuzzle Sudoku (repeat 91 '0') == Nothing) <?> "decodePuzzle long failed"
      
    , let repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)
      in (decodePuzzle Sudoku (repeat 81 'a' <> repeat 9 'b') == Nothing) <?> "decodePuzzle non-digit failed"

    , let repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)
      in (decodePuzzle Wordoku (repeat 81 '0' <> "112345678") == Nothing) <?> "decodePuzzle duplicate key failed"

    , let repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)
      in (decodePuzzle Wordoku (repeat 81 '0' <> "123.45678") == Nothing) <?> "decodePuzzle dot in key failed"
    ]
