module Test.EncodingTests where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (singleton)
import Sudoku.Encoding (buildKey, decodePuzzle, denormalize, encodePuzzle, normalize, normalizeCell)
import Sudoku.Internal.Generator (Game(..))
import Test.QuickCheck (Result, (<?>))

encodingTests :: Array Result
encodingTests = 
    [ testNormalizeCell_Empty
    , testNormalizeCell_First
    , testNormalizeCell_Last
    , testNormalizeCell_Unknown
    , testNormalizeDenormalizeRoundTrip
    , testEncodeDecodeRoundTrip
    , testDecodePuzzle_Empty
    , testDecodePuzzle_Short
    , testDecodePuzzle_Long
    , testDecodePuzzle_NonDigit
    , testWordokuKeyInvariant
    ]

testNormalizeCell_Empty :: Result
testNormalizeCell_Empty = (normalizeCell "ROYLGBIPV" '.' == '0') <?> "normalizeCell '.' -> '0' failed"

testNormalizeCell_First :: Result
testNormalizeCell_First = (normalizeCell "ROYLGBIPV" 'R' == '1') <?> "normalizeCell 'R' -> '1' failed"

testNormalizeCell_Last :: Result
testNormalizeCell_Last = (normalizeCell "ROYLGBIPV" 'V' == '9') <?> "normalizeCell 'V' -> '9' failed"

testNormalizeCell_Unknown :: Result
testNormalizeCell_Unknown = (normalizeCell "ROYLGBIPV" 'X' == '0') <?> "normalizeCell unknown -> '0' failed"

testNormalizeDenormalizeRoundTrip :: Result
testNormalizeDenormalizeRoundTrip =
    let k = "countries"
        p = "c.o.u.n.t.r.i.e.s................................................................"
        norm = normalize k p
        denorm = denormalize k norm
    in (denorm == p) <?> "Normalize/Denormalize round-trip failed:\n" <> p <> "\n" <> denorm

testEncodeDecodeRoundTrip :: Result
testEncodeDecodeRoundTrip =
    let k = "123456789"
        p = "123456789123456789123456789123456789123456789123456789123456789123456789123456789"
        norm = normalize k p
        encoded = encodePuzzle norm k
        decoded = decodePuzzle encoded
    in (decoded == Just { puzzle: norm, key: k }) <?> "Encode/Decode round-trip failed"

testDecodePuzzle_Empty :: Result
testDecodePuzzle_Empty = (decodePuzzle "" == Nothing) <?> "decodePuzzle empty failed"

testDecodePuzzle_Short :: Result
testDecodePuzzle_Short = (decodePuzzle "abc" == Nothing) <?> "decodePuzzle short failed"

testDecodePuzzle_Long :: Result
testDecodePuzzle_Long = (decodePuzzle (repeat 91 '0') == Nothing) <?> "decodePuzzle long failed"
    where
        repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)

testDecodePuzzle_NonDigit :: Result
testDecodePuzzle_NonDigit = (decodePuzzle (repeat 81 'a' <> repeat 9 'b') == Nothing) <?> "decodePuzzle non-digit failed"
    where
        repeat n c = foldr (\_ s -> s <> singleton c) "" (1..n)

testWordokuKeyInvariant :: Result
testWordokuKeyInvariant =
    let k = buildKey Wordoku "countries"
    in (k == "countries") <?> "Wordoku key invariant failed"
