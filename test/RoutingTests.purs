module Test.RoutingTests where

import Prelude

import Data.Array ((..))
import Data.Foldable (foldr)
import Data.String.CodeUnits (singleton)
import Routing (Route(..), buildPath, parsePath)
import Sudoku.Encoding (DecodedKey(..), normalize)
import Sudoku.Internal.Generator (Difficulty(..), Game(..))
import Test.QuickCheck (Result, (<?>))

routingTests :: Array Result
routingTests =
  [ (parsePath "/" == Home) <?> "parsePath '/' failed"
  , (parsePath "/wordoku" == GameRoute Wordoku) <?> "parsePath '/wordoku' failed"
  , (parsePath "/wordoku/tricky" == DifficultyRoute Wordoku Tricky) <?> "parsePath '/wordoku/tricky' failed"
  , (parsePath "/Wordoku/Tricky" == DifficultyRoute Wordoku Tricky) <?> "parsePath '/Wordoku/Tricky' failed"
  , (parsePath "/unknown" == Home) <?> "parsePath '/unknown' failed"
  , (parsePath "/wordoku/unknown" == GameRoute Wordoku) <?> "parsePath '/wordoku/unknown' failed"
  , (parsePath "/wordoku/tricky/too-short" == DifficultyRoute Wordoku Tricky) <?> "parsePath '/wordoku/tricky/too-short' failed"

  , let
      p = repeat 81 '0'
      badKey = "ROYLGBIPV" -- Colorku key for a Sudoku game
      repeat n c = foldr (\_ s -> s <> singleton c) "" (1 .. n)
    in
      (parsePath ("/sudoku/tricky/" <> p <> badKey) == DifficultyRoute Sudoku Tricky) <?> "parsePath key/game mismatch failed"

  , let
      g = Wordoku
      d = Tricky
      k = WordokuKey "countries"
      p = "c.o.u.n.t.r.i.e.s................................................................"
      norm = normalize k p
      path = buildPath g d norm k
      expected = PuzzleRoute g d norm k
    in
      (parsePath path == expected) <?> "Routing round-trip failed"
  ]
