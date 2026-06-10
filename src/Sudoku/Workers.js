const workerUrl = new URL("../../src/worker.js", import.meta.url);

const isMobile = () => {
  if (typeof navigator === "undefined") return false;
  return /Mobi|Android|iPhone|iPad|iPod/i.test(navigator.userAgent);
};

// worker pool is required to avoid browser detecting and killing rapid worker creations
export const workerCountImpl = () => {
  if (navigator.hardwareConcurrency) {
    if (!isMobile()) {
      return navigator.hardwareConcurrency;
    }
    // Avoiding OS throttling by only using half of all available cores on mobile
    return Math.floor(navigator.hardwareConcurrency / 2);
  }
  // mobile-friendly default of 4 workers
  return 4;
};

export const spawnWorkerImpl = (onErr) => () => {
  try {
    return new Worker(workerUrl, { type: "module" });
  } catch (err) {
    onErr("Failed to spawn worker: " + (err.message || err))();
    return null;
  }
};

export const postMessageImpl = (worker) => (msg) => () =>
  worker.postMessage(msg);

export const onMessageImpl = (worker) => (handler) => () => {
  worker.onmessage = (e) => handler(e.data)();
};

export const onErrorImpl = (worker) => (handler) => () => {
  worker.onerror = (e) => {
    // Web Workers often provide very little info for module load errors
    const msg =
      e.message ||
      "Worker error (likely module load failure or resource limit)";
    handler(msg)();
  };
};

export const terminateWorkerImpl = (worker) => () => worker.terminate();
