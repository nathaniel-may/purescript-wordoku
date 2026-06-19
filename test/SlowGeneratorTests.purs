module Test.SlowGeneratorTests (slowGeneratorTests) where

import Prelude

import Data.Array (filter, find, null, (..))
import Data.DateTime (diff)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either, isLeft)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Sudoku (Difficulty(..), Game(..), Variant(..))
import Test.Main (checkInvariants, hushLeft)
import Test.QuickCheck (Result, (<?>))

-- | Slow puzzle-generation properties (Challenge/Difficult difficulties),
-- | separated from Test.Main's fast suite. Each case logs a start message,
-- | and on completion logs elapsed wall-clock time, so a developer watching
-- | stdout sees continuous progress instead of long silence.
slowGeneratorTests :: Effect (Array Result)
slowGeneratorTests = do
  r1 <- timedLog "Standard/Challenge" standardChallenge
  r2 <- standardDifficultLoop
  r3 <- timedLog "UniqueDiagonal/Challenge x2" uniqueDiagonalChallengeLoop
  pure [ r1, r2, r3 ]

  where
  standardChallenge :: Effect Result
  standardChallenge = do
    results <- checkInvariants Standard Challenge Sudoku
    pure $ (not $ isLeft results) <?> ("Standard Challenge: " <> (fromMaybe "" $ hushLeft results))

  -- runs and times each of the 20 iterations individually, logging
  -- "Standard/Difficult #n/20" before each and elapsed time after
  standardDifficultLoop :: Effect Result
  standardDifficultLoop = do
    results <- traverse iter (1 .. 20)
    let mFirstFailure = find isLeft results
    pure $ (null $ filter isLeft results) <?> ("Standard Difficult: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))
    where
    iter :: Int -> Effect (Either String Unit)
    iter n = timedLog ("Standard/Difficult #" <> show n <> "/20") (checkInvariants Standard Difficult Sudoku)

  uniqueDiagonalChallengeLoop :: Effect Result
  uniqueDiagonalChallengeLoop = do
    results <- traverse (\_ -> checkInvariants UniqueDiagonal Challenge Sudoku) (1 .. 2)
    let mFirstFailure = find isLeft results
    pure $ (null $ filter isLeft results) <?> ("UniqueDiagonal Challenge: " <> (fromMaybe "" $ mFirstFailure >>= hushLeft))

  -- logs "<label> starting..." before, "<label> done in <ms>ms" after, runs the action, returns its result
  timedLog :: forall a. String -> Effect a -> Effect a
  timedLog label action = do
    log (label <> " starting...")
    start <- map toDateTime now
    result <- action
    end <- map toDateTime now
    log (label <> " done in " <> show (diff end start :: Milliseconds))
    pure result
