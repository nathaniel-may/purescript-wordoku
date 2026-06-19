-- | Pure display/toggle-selection logic for the "Show Solution" feature.
-- | Deliberately Effect/Aff-free: every function here is a total, pure
-- | mapping over already-known values (a cached Grid, a Boolean flag), never
-- | invoking the solver. This is what makes "toggling is instant" provable
-- | by the types alone -- see Test 1 in the design doc.
module Sudoku.Display
  ( solutionButtonLabel
  , solutionButtonDisabled
  , displayedPuzzleString
  , applySolveResult
  , clueOrder
  , applyClues
  , addClueButtonDisabled
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Sudoku.Encoding (DecodedKey, keyToString)
import Sudoku.Internal.Grid (Grid, gridString)
import Sudoku.Internal.Key (mkKey)

-- | Label always reflects what clicking the button would switch *to* --
-- | not solve progress. There is no separate "Solving..." label; disabling
-- | (see solutionButtonDisabled) is the only signal solving isn't done yet.
solutionButtonLabel :: Boolean -> String
solutionButtonLabel true = "Clues"
solutionButtonLabel false = "Solution"

-- | Disabled exactly when there is no solution to show yet (covers both
-- | "still solving" and "solve failed permanently" -- solveError, a separate
-- | field, carries the distinction the rest of the app needs for those).
solutionButtonDisabled :: Maybe Grid -> Boolean
solutionButtonDisabled = isNothing

-- | Selects which puzzle string to render: the original puzzle (clues) or
-- | the solved grid rendered in the puzzle's own display alphabet.
-- | `key` is the puzzle's own DecodedKey; the cached `Grid` is always keyed
-- | by that same key (it's the alphabet every worker solve response is
-- | encoded/parsed in), so rendering it back out just needs `gridString`
-- | with that key directly -- no denormalize step is involved (denormalize
-- | is solely for the unrelated URL-encoding alphabet).
displayedPuzzleString :: DecodedKey -> Boolean -> String -> Maybe Grid -> String
displayedPuzzleString _ false puzzle _ = puzzle
displayedPuzzleString _ true puzzle Nothing = puzzle
displayedPuzzleString key true puzzle (Just grid) = case mkKey (keyToString key) of
  Left _ -> puzzle
  Right k -> gridString k grid

-- | Pure decision for the stale/duplicate solve dispatch guard (see design
-- | doc section 5). Applies an incoming solve result to `solution`/
-- | `solveError` only if `incomingId` matches the latest dispatched request
-- | id; otherwise the incoming result is discarded as stale and the current
-- | values are returned unchanged.
applySolveResult
  :: { latestRequestId :: Int
     , currentSolution :: Maybe Grid
     , currentSolveError :: Maybe String
     }
  -> Int
  -> Either String Grid
  -> { solution :: Maybe Grid, solveError :: Maybe String }
applySolveResult st incomingId result
  | incomingId == st.latestRequestId =
      case result of
        Right grid -> { solution: Just grid, solveError: Nothing }
        Left err -> { solution: Nothing, solveError: Just err }
  | otherwise =
      { solution: st.currentSolution, solveError: st.currentSolveError }

-- | Folds the puzzle string into a single `Int` seed using each `Char`'s
-- | code point. `Int` is 32-bit signed and wraps on overflow in PureScript;
-- | no special overflow handling is needed since only relative ordering of
-- | hashes matters within one puzzle.
hashSeed :: String -> Int
hashSeed s = foldl (\acc c -> acc * 31 + fromEnum c) 0 (toCharArray s)

-- | A deterministic integer mix function; exact constants aren't
-- | load-bearing beyond their relative magnitudes, they just need to be
-- | fixed and produce a reasonably-distributed ordering for typical 9x9
-- | puzzles. This is a Weyl/golden-ratio multiplicative hash: both
-- | `-1640531535` (Knuth's multiplicative-hash constant `2654435761`,
-- | represented as its 32-bit signed two's-complement equivalent so the
-- | literal stays in the JS-backend `Int` range; numerically identical mod
-- | 2^32, so it produces the exact same wraparound arithmetic) and
-- | `618033988` (the golden-ratio fraction of the modulus, `floor(0.618...
-- | * 1000000007)`) are on the same order of magnitude as the modulus
-- | `1000000007` itself. That's what makes this scramble rather than ramp:
-- | incrementing `idx` by 1 jumps the product by a large, non-trivial
-- | fraction of the modulus each time, so it wraps repeatedly across the
-- | small range `idx` takes (0-80 for a 9x9 puzzle). A small per-`idx` step
-- | relative to the modulus (as in earlier, buggy versions of this
-- | function) never wraps over that range and so stays monotonic in `idx`
-- | -- merely reversing direction depending on the sign of the seed term,
-- | which still reveals cells in positional order.
cellHash :: Int -> Int -> Int
cellHash seed idx =
  let
    h = (seed * (-1640531535) + idx * 618033988) `mod` 1000000007
  in
    if h < 0 then negate h else h

-- | A deterministic ordering over a puzzle's blank cell indices, used to
-- | decide which cell gets revealed first/second/etc when the user clicks
-- | "+1 Clue" repeatedly. Re-derives the `'.'` check directly from the
-- | input string's characters (the *denormalized display* string), so it
-- | has no dependency on `Sudoku.Encoding.denormalize`.
clueOrder :: String -> Array Int
clueOrder puzzle =
  let
    chars = toCharArray puzzle
    blanks = Array.filter (\i -> Array.index chars i == Just '.') (Array.range 0 (Array.length chars - 1))
    seed = hashSeed puzzle
  in
    Array.sortBy (comparing (cellHash seed)) blanks

-- | Reveals the first `n` blanks (per `clueOrder`) of `puzzle` using the
-- | corresponding characters from the solved `Grid`. `n` is clamped to
-- | `[0, blankCount]` -- this is the single clamp point used both by the UI
-- | (button disable) and the URL parser fallback path (out-of-range high
-- | values from `Routing.purs`). Mirrors `displayedPuzzleString`'s fallback
-- | pattern: returns `puzzle` unchanged if `key` doesn't construct.
applyClues :: DecodedKey -> Int -> String -> Grid -> String
applyClues key n puzzle grid = case mkKey (keyToString key) of
  Left _ -> puzzle
  Right k ->
    let
      order = clueOrder puzzle
      blankCount = Array.length order
      clamped = max 0 (min n blankCount)
      idxsToReveal = Array.take clamped order
      solvedStr = gridString k grid
      puzzleChars = toCharArray puzzle
      solvedChars = toCharArray solvedStr

      reveal :: Array Char -> Int -> Array Char
      reveal cs i = case Array.index solvedChars i of
        Just c -> fromMaybe cs (Array.updateAt i c cs)
        Nothing -> cs
    in
      fromCharArray (foldl reveal puzzleChars idxsToReveal)

-- | Disabled when there's no solution cached yet, or when every blank in
-- | the original puzzle string has already been revealed as an extra clue.
addClueButtonDisabled :: Maybe Grid -> Int -> String -> Boolean
addClueButtonDisabled Nothing _ _ = true
addClueButtonDisabled (Just _) clueCount puzzle =
  clueCount >= Array.length (Array.filter (_ == '.') (toCharArray puzzle))
