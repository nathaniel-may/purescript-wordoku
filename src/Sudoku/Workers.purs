module Sudoku.Workers (workerCount, raceGenerateSudoku) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff, effectCanceler, error)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array ((..))
import Data.Traversable (traverse)
import Effect.Ref as Ref
import Sudoku.Internal (Variant(..))
import Sudoku.Internal.Generator (Difficulty)

foreign import data Worker :: Type
foreign import workerCountImpl :: Effect Int
foreign import spawnWorkerImpl :: (String -> Effect Unit) -> (String -> Effect Unit) -> Effect Worker
foreign import postMessageImpl :: Worker -> String -> String -> Effect Unit
foreign import terminateWorkerImpl :: Worker -> Effect Unit

workerCount :: Effect Int
workerCount = workerCountImpl

variantToString :: Variant -> String
variantToString Standard = "Standard"
variantToString UniqueDiagonal = "UniqueDiagonal"

raceGenerateSudoku :: Int -> Variant -> Difficulty -> Aff String
raceGenerateSudoku n variant difficulty = makeAff \cb -> do
  resolvedRef <- Ref.new false
  errorCountRef <- Ref.new 0
  workersRef <- Ref.new []

  let cleanup = do
        workers <- Ref.read workersRef
        void $ traverse terminateWorkerImpl workers

      handleMessage msg = do
        resolved <- Ref.read resolvedRef
        unless resolved do
          Ref.write true resolvedRef
          cleanup
          cb (Right msg)

      handleError err = do
        log $ "Worker error: " <> err
        resolved <- Ref.read resolvedRef
        unless resolved do
          count <- Ref.modify (_ + 1) errorCountRef
          when (count == n) do
            Ref.write true resolvedRef
            cleanup
            cb (Left $ error "All workers failed to generate puzzle")

  workers <- traverse (\_ -> spawnWorkerImpl handleMessage handleError) (1..n)
  Ref.write workers workersRef

  void $ traverse (\w -> postMessageImpl w (variantToString variant) (show difficulty)) workers

  pure $ effectCanceler cleanup
