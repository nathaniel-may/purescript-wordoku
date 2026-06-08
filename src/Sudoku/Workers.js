const workerUrl = new URL('../../src/worker.js', import.meta.url);

export const workerCountImpl = () => {
  if (typeof navigator !== 'undefined' && navigator.hardwareConcurrency) {
    return Math.min(4, navigator.hardwareConcurrency);
  }
  return 2;
};

export const spawnWorkerImpl = onErr => () => {
  try {
    return new Worker(workerUrl, { type: 'module' });
  } catch (err) {
    onErr("Failed to spawn worker: " + (err.message || err))();
    return null;
  }
};

export const postMessageImpl = worker => msg => () => worker.postMessage(msg);

export const onMessageImpl = worker => handler => () => { 
  worker.onmessage = e => handler(e.data)(); 
};

export const onErrorImpl = worker => handler => () => { 
  worker.onerror = e => handler(e.message || "Worker error")(); 
};

export const terminateWorkerImpl = worker => () => worker.terminate();
