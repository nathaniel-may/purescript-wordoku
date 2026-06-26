module Test.RenderTests where

import Prelude

import Data.Array as Array
import Data.Newtype (unwrap)
import Data.String (Pattern(..), contains)
import Data.String.CodeUnits (fromCharArray)
import Data.Void (absurd)
import Halogen.HTML (HTML)
import Halogen.VDom.DOM.StringRenderer (render) as StringRenderer
import Main (tableFrom)
import Sudoku (Game(..))
import Test.QuickCheck (Result, (<?>))

-- The exact `class="..."` markers `tableFrom` emits on an extra-clue cell:
-- a small dot drawn over the circle for Colorku (its circle already fills
-- most of the cell, so the usual tint is barely visible), and a yellow
-- tint for Sudoku/Wordoku. The trailing quote disambiguates the two --
-- "ExtraClue" alone is a substring of "ExtraClueDot".
dotMarker :: String
dotMarker = "td ExtraClueDot\""

tintMarker :: String
tintMarker = "td ExtraClue\""

renderToString :: HTML Void Void -> String
renderToString = StringRenderer.render absurd <<< unwrap

-- An 81-char puzzle that's blank except for one revealed clue at index 0,
-- used to render a table with exactly one extra-clue cell.
fixture :: Char -> { original :: String, displayed :: String }
fixture clueChar =
  { original: fromCharArray (Array.replicate 81 '.')
  , displayed: fromCharArray (Array.cons clueChar (Array.replicate 80 '.'))
  }

renderTests :: Array Result
renderTests =
  [ render Colorku 'R' `hasMarkerAndNotOther` { has: dotMarker, lacks: tintMarker }
      <?> "tableFrom: Colorku's extra-clue cell gets the dot marker, not the tint marker"

  , render Sudoku '5' `hasMarkerAndNotOther` { has: tintMarker, lacks: dotMarker }
      <?> "tableFrom: Sudoku's extra-clue cell gets the tint marker, not the dot marker"

  , render Wordoku 'a' `hasMarkerAndNotOther` { has: tintMarker, lacks: dotMarker }
      <?> "tableFrom: Wordoku's extra-clue cell gets the tint marker, not the dot marker"
  ]
  where
  render :: Game -> Char -> String
  render game clueChar =
    let
      { original, displayed } = fixture clueChar
    in
      renderToString (tableFrom game original displayed)

  hasMarkerAndNotOther :: String -> { has :: String, lacks :: String } -> Boolean
  hasMarkerAndNotOther html { has, lacks } = contains (Pattern has) html && not (contains (Pattern lacks) html)
