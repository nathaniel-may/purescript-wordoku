const workerUrl = new URL('../../src/worker.js', import.meta.url);

const isMobile = () => {
  if (typeof navigator === 'undefined') return false;
  return /Mobi|Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
};

export const workerCountImpl = () => {
  if (typeof navigator !== 'undefined' && navigator.hardwareConcurrency) {
    const cap = isMobile() ? 2 : 4;
    return Math.min(cap, navigator.hardwareConcurrency);
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
  worker.onerror = e => {
    // Web Workers often provide very little info for module load errors
    const msg = e.message || "Worker error (likely module load failure or resource limit)";
    handler(msg)(); 
  };
};

export const terminateWorkerImpl = worker => () => worker.terminate();
