import {
  generateStringly,
  solveStringly,
} from "../output/Sudoku.WorkerAPI/index.js";

self.onmessage = (e) => {
  const { id } = e.data;
  try {
    if ("puzzle" in e.data) {
      const { key, puzzle } = e.data;
      const { result, error } = solveStringly(key)(puzzle)();
      if (error) {
        console.error("Sudoku Worker: Explicit error from API", error);
        self.postMessage({ id, error });
      } else {
        self.postMessage({ id, result });
      }
    } else {
      const { variant, difficulty } = e.data;
      const { result, error } = generateStringly(variant)(difficulty)();
      if (error) {
        console.error("Sudoku Worker: Explicit error from API", error);
        self.postMessage({ id, error });
      } else {
        self.postMessage({ id, result });
      }
    }
  } catch (err) {
    console.error("Sudoku Worker: Unexpected exception", err);
    self.postMessage({ id, error: err.message || "Internal worker error" });
  }
};
