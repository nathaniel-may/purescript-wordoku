import { generateStringly } from "../output/Sudoku.WorkerAPI/index.js";

console.log("Sudoku Worker: Module loaded");

self.onmessage = (e) => {
  const { variant, difficulty } = e.data;
  console.log("Sudoku Worker: Received request", { variant, difficulty });
  try {
    const result = generateStringly(variant)(difficulty)();
    self.postMessage(result);
  } catch (err) {
    console.error("Sudoku Worker: Error during generation", err);
    throw err;
  }
};
