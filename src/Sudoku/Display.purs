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
import Sudoku.Encoding (DecodedKey, denormalize)
import Sudoku.Internal.Grid (Grid, gridString)
import Sudoku.Internal.Key (sudokuKey)

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
-- | the solved grid denormalized into the puzzle's display alphabet.
-- | `key` is the puzzle's own DecodedKey (used only for the denormalize
-- | step); the Grid itself is always parsed in the fixed canonical digit
-- | alphabet (`sudokuKey`), regardless of `key`, because that's the
-- | alphabet every worker solve response is encoded in.
displayedPuzzleString :: DecodedKey -> Boolean -> String -> Maybe Grid -> String
displayedPuzzleString _ false puzzle _ = puzzle
displayedPuzzleString _ true puzzle Nothing = puzzle
displayedPuzzleString key true _ (Just grid) = denormalize key (gridString sudokuKey grid)

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
