export const workerCountImpl = () => {
  if (typeof navigator !== 'undefined' && navigator.hardwareConcurrency) {
    // We cap at 4 to avoid excessive memory/CPU contention on mobile.
    return Math.min(4, navigator.hardwareConcurrency);
  }
  return 2;
};

export const spawnWorkerImpl = onMsg => onErr => () => {
  // Path assumes compiled location: output/Sudoku.Workers/foreign.js
  const worker = new Worker(
    new URL('../../worker.js', import.meta.url), 
    { type: 'module' }
  );

  worker.onmessage = (e) => onMsg(e.data)();
  worker.onerror = (e) => onErr(e.message)();

  return worker;
};

export const postMessageImpl = worker => variant => difficulty => () => {
  worker.postMessage({ variant, difficulty });
};

export const terminateWorkerImpl = worker => () => {
  worker.terminate();
};
