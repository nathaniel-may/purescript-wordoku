module Test.StaleSolveTests where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Sudoku.Display (applySolveResult)
import Sudoku.Internal.Grid (Grid, readGrid)
import Sudoku.Internal.Key (sudokuKey)
import Test.QuickCheck (Result, (<?>))

-- Two distinct, validly-solved 81-character Sudoku grids, used only as
-- distinguishable `Grid` fixtures for the stale-dispatch guard tests below
-- (their values are otherwise irrelevant -- `readGrid` doesn't validate
-- Sudoku correctness, but both of these happen to be genuine solved grids:
-- `solutionB` is `solutionA` with every `1` and `2` swapped, which preserves
-- validity since relabeling a digit pair uniformly preserves the
-- one-of-each-digit property in every row/column/box).
solutionA :: Grid
solutionA = fixtureGrid "415873962836129475972564183184935726763482519529716834657348291348291657291657348"

solutionB :: Grid
solutionB = fixtureGrid "425873961836219475971564283284935716763481529519726834657348192348192657192657348"

fixtureGrid :: String -> Grid
fixtureGrid s = case readGrid sudokuKey s of
  Right g -> g
  Left err -> unsafeCrashWith ("StaleSolveTests fixture is broken: " <> err)

staleSolveTests :: Array Result
staleSolveTests =
  [ -- Out-of-order completion, two different puzzles: request 2's result
    -- (the latest dispatched) is applied.
    let
      afterB = applySolveResult
        { latestRequestId: 2, currentSolution: Nothing, currentSolveError: Nothing }
        2
        (Right solutionB)
    in
      (afterB.solution == Just solutionB && afterB.solveError == Nothing)
        <?> "applySolveResult: request 2's (latest) result should apply"

  , -- request 1's late-arriving result (now stale) is discarded.
    let
      afterB = applySolveResult
        { latestRequestId: 2, currentSolution: Nothing, currentSolveError: Nothing }
        2
        (Right solutionB)
      afterStaleA = applySolveResult
        { latestRequestId: 2, currentSolution: afterB.solution, currentSolveError: afterB.solveError }
        1
        (Right solutionA)
    in
      (afterStaleA == afterB)
        <?> "applySolveResult: stale request 1's result should be discarded, leaving state unchanged"

  , -- In-order completion (sanity/control case): request 1 resolves first,
    -- applies; request 2 resolves after, also applies (overwrites 1's result).
    let
      afterA = applySolveResult
        { latestRequestId: 1, currentSolution: Nothing, currentSolveError: Nothing }
        1
        (Right solutionA)
      afterB = applySolveResult
        { latestRequestId: 2, currentSolution: afterA.solution, currentSolveError: afterA.solveError }
        2
        (Right solutionB)
    in
      (afterA.solution == Just solutionA && afterB.solution == Just solutionB)
        <?> "applySolveResult: in-order completions should both apply, latest wins"

  , -- Failure result respects the same guard: a stale failure must not
    -- clobber a newer, successful solution.
    let
      afterB = applySolveResult
        { latestRequestId: 2, currentSolution: Nothing, currentSolveError: Nothing }
        2
        (Right solutionB)
      afterStaleFailure = applySolveResult
        { latestRequestId: 2, currentSolution: afterB.solution, currentSolveError: afterB.solveError }
        1
        (Left "stale failure should be discarded")
    in
      (afterStaleFailure == afterB)
        <?> "applySolveResult: a stale failure result should be discarded, not clobber a newer success"

  , -- Companion: when a failure DOES apply (matching request id), both
    -- output fields are set together correctly.
    let
      result = applySolveResult
        { latestRequestId: 1, currentSolution: Nothing, currentSolveError: Nothing }
        1
        (Left "boom")
    in
      (result.solution == Nothing && result.solveError == Just "boom")
        <?> "applySolveResult: a matching failure should set solution=Nothing, solveError=Just err"
  ]
