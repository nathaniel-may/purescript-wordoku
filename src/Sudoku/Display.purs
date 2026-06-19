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
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..), isNothing)
import Sudoku.Encoding (DecodedKey, keyToString)
import Sudoku.Internal.Grid (Grid, gridString)
import Sudoku.Internal.Key (mkKey)

-- | Label always reflects what clicking the button would switch *to* --
-- | not solve progress. There is no separate "Solving..." label; disabling
-- | (see solutionButtonDisabled) is the only signal solving isn't done yet.
solutionButtonLabel :: Boolean -> String
solutionButtonLabel true = "Clues"
solutionButtonLabel false = "Show Solution"

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
