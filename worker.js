import { generateStringly } from "./output/Sudoku.WorkerAPI/index.js";

self.onmessage = (e) => {
  const { variant, difficulty } = e.data;
  // CONTRACT: Strings must match Sudoku.Workers.variantToString 
  // and the Show Difficulty instance.
  const result = generateStringly(variant)(difficulty)();
  self.postMessage(result);
};
