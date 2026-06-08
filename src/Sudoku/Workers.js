export const workerCountImpl = () => {
  if (typeof navigator !== 'undefined' && navigator.hardwareConcurrency) {
    // We cap at 4 to avoid excessive memory/CPU contention on mobile.
    return Math.min(4, navigator.hardwareConcurrency);
  }
  return 2;
};

export const spawnWorkerImpl = onMsg => onErr => () => {
  try {
    const worker = new Worker(
      new URL('../../src/worker.js', import.meta.url), 
      { type: 'module' }
    );

    worker.onmessage = (e) => onMsg(e.data)();
    worker.onerror = (e) => {
      const msg = e.message || "Worker error (possibly failed to load script or module not supported)";
      onErr(msg)();
    };

    return worker;
  } catch (err) {
    onErr("Failed to spawn worker: " + (err.message || err))();
    // Return a dummy object to satisfy the Type, though things will likely fail
    return { terminate: () => {}, postMessage: () => {} };
  }
};

export const postMessageImpl = worker => variant => difficulty => () => {
  worker.postMessage({ variant, difficulty });
};

export const terminateWorkerImpl = worker => () => {
  worker.terminate();
};
