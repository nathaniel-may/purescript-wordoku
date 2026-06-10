import { generateStringly } from "../output/Sudoku.WorkerAPI/index.js";

self.onmessage = (e) => {
  const { id, variant, difficulty } = e.data;
  try {
    const { result, error } = generateStringly(variant)(difficulty)();
    if (error) {
      console.error("Sudoku Worker: Explicit error from API", error);
      self.postMessage({ id, error });
    } else {
      self.postMessage({ id, result });
    }
  } catch (err) {
    console.error("Sudoku Worker: Unexpected exception during generation", err);
    self.postMessage({ id, error: err.message || "Internal worker error" });
  }
};
