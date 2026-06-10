module Sudoku.Workers
  ( WorkerPool
  , WorkerState
  , PendingRequest
  , workerCount
  , initPool
  , raceGenerateSudoku
  , Worker
  ) where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler, error)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.Array (filter, length, (..), take, catMaybes)
import Data.Traversable (traverse, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect.Exception (Error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Sudoku.Internal (Variant(..))
import Sudoku.Internal.Generator (Difficulty)

foreign import data Worker :: Type

type PendingRequest =
  { cb :: Either Error String -> Effect Unit
  , outstanding :: Int
  }

type WorkerState =
  { worker :: Worker
  , id     :: Int
  , jobId  :: Ref (Maybe Int)
  }

type WorkerPool =
  { idleWorkers      :: Ref (Array WorkerState)
  , allWorkers       :: Ref (Array WorkerState)
  , requestId        :: Ref Int
  , workerId         :: Ref Int
  , pendingRequests  :: Ref (Map Int PendingRequest)
  , cap              :: Int
  }

foreign import workerCountImpl :: Effect Int
foreign import spawnWorkerImpl :: (String -> Effect Unit) -> Effect (Nullable Worker)
foreign import postMessageImpl :: Worker -> { id :: Int, variant :: String, difficulty :: String } -> Effect Unit
foreign import onMessageImpl :: Worker -> ({ id :: Int, result :: String, error :: Nullable String } -> Effect Unit) -> Effect Unit
foreign import onErrorImpl :: Worker -> (String -> Effect Unit) -> Effect Unit
foreign import terminateWorkerImpl :: Worker -> Effect Unit

workerCount :: Effect Int
workerCount = workerCountImpl

variantToString :: Variant -> String
variantToString Standard = "Standard"
variantToString UniqueDiagonal = "UniqueDiagonal"

initPool :: Effect WorkerPool
initPool = do
  cap <- workerCountImpl
  idleWorkers <- Ref.new []
  allWorkers <- Ref.new []
  requestId <- Ref.new 0
  workerId <- Ref.new 0
  pendingRequests <- Ref.new Map.empty
  let pool = { idleWorkers, allWorkers, requestId, workerId, pendingRequests, cap }
  -- Pre-warm all workers
  for_ (1..pool.cap) \_ -> do
    void $ spawnAndAdd pool
  pure pool

spawnAndAdd :: WorkerPool -> Effect (Maybe WorkerState)
spawnAndAdd pool = do
  let handleErr msg = log $ "Worker spawn error: " <> msg
  maybeWorker <- map toMaybe $ spawnWorkerImpl handleErr
  case maybeWorker of
    Just w -> do
      wid <- Ref.modify (_ + 1) pool.workerId
      jobId <- Ref.new Nothing
      let ws = { worker: w, id: wid, jobId }
      Ref.modify_ (_ <> [ws]) pool.allWorkers
      Ref.modify_ (_ <> [ws]) pool.idleWorkers
      onMessageImpl w (handleMessage pool ws)
      onErrorImpl w (handleWorkerHardwareError pool ws)
      pure (Just ws)
    Nothing -> pure Nothing

handleMessage :: WorkerPool -> WorkerState -> { id :: Int, result :: String, error :: Nullable String } -> Effect Unit
handleMessage pool ws msg = do
  -- Clear current job
  Ref.write Nothing ws.jobId
  -- Return to idle pool if still in allWorkers
  all <- Ref.read pool.allWorkers
  when (ws `elem'` all) do
    Ref.modify_ (_ <> [ws]) pool.idleWorkers

  -- Lookup and resolve
  pending <- Ref.read pool.pendingRequests
  case Map.lookup msg.id pending of
    Nothing -> pure unit -- Already resolved or discarded
    Just req -> do
      case toMaybe msg.error of
        Just err -> do
          log $ "Worker reported error for request " <> show msg.id <> ": " <> err
          handleFailure pool msg.id
        Nothing -> do
          -- Success! Delete from map so subsequent results are ignored
          Ref.modify_ (Map.delete msg.id) pool.pendingRequests
          req.cb (Right msg.result)

handleWorkerHardwareError :: WorkerPool -> WorkerState -> String -> Effect Unit
handleWorkerHardwareError pool ws err = do
  -- Terminate and remove from allWorkers/idleWorkers
  terminateWorkerImpl ws.worker
  Ref.modify_ (filter (\x -> x.id /= ws.id)) pool.allWorkers
  Ref.modify_ (filter (\x -> x.id /= ws.id)) pool.idleWorkers

  -- If it was on a job, decrement that job's outstanding count
  mjid <- Ref.read ws.jobId
  case mjid of
    Nothing -> pure unit
    Just rid -> do
      log $ "Worker hardware error during job " <> show rid <> ": " <> err
      handleFailure pool rid

handleFailure :: WorkerPool -> Int -> Effect Unit
handleFailure pool rid = do
  pending <- Ref.read pool.pendingRequests
  case Map.lookup rid pending of
    Nothing -> pure unit
    Just req -> do
      let newCount = req.outstanding - 1
      if newCount <= 0 then do
        Ref.modify_ (Map.delete rid) pool.pendingRequests
        req.cb (Left $ error "All workers failed for this request")
      else do
        Ref.modify_ (Map.insert rid (req { outstanding = newCount })) pool.pendingRequests

-- | Dispatches a generation request to `n` workers in parallel, returning the first successful result.
-- | NOTE: Losing workers are NOT cancelled. They continue computing until they finish or fail,
-- | at which point they return to the idle pool. This temporarily increases CPU usage on multi-core
-- | systems but avoids the overhead of terminating and respawning workers for rapid subsequent requests.
raceGenerateSudoku :: WorkerPool -> Int -> Variant -> Difficulty -> Aff String
raceGenerateSudoku pool n variant difficulty = makeAff \cb -> do
  rid <- Ref.modify (_ + 1) pool.requestId

  -- Acquire workers
  available <- Ref.read pool.idleWorkers
  let toTake = min n (length available)
      dispatching = take toTake available

  Ref.modify_ (filter (\ws -> not (ws `elem'` dispatching))) pool.idleWorkers

  -- If we need more, try to spawn up to cap
  allCount <- length <$> Ref.read pool.allWorkers
  let needed = n - (length dispatching)
      canSpawn = min needed (pool.cap - allCount)

  newlySpawned <- if canSpawn > 0
    then catMaybes <$> traverse (\_ -> spawnAndAdd pool) (1..canSpawn)
    else pure []

  -- Mark newly spawned as not idle (they are added to idle in spawnAndAdd, so remove them)
  Ref.modify_ (filter (\ws -> not (ws `elem'` newlySpawned))) pool.idleWorkers

  let totalDispatching = dispatching <> newlySpawned

  if length totalDispatching == 0 then
    cb (Left $ error "No workers available")
  else do
    Ref.modify_ (Map.insert rid { cb, outstanding: length totalDispatching }) pool.pendingRequests
    for_ totalDispatching \ws -> do
      Ref.write (Just rid) ws.jobId
      postMessageImpl ws.worker { id: rid, variant: variantToString variant, difficulty: show difficulty }

  pure nonCanceler

-- Helpers
elem' :: WorkerState -> Array WorkerState -> Boolean
elem' x xs = length (filter (\y -> y.id == x.id) xs) > 0
