import { Bench } from 'tinybench';
import * as Generator from '../output/Sudoku.Internal.Generator/index.js';
import * as Internal from '../output/Sudoku.Internal/index.js';

const difficulties = [
    { name: 'Beginner',  difficulty: Generator.Beginner.value  },
    { name: 'Casual',    difficulty: Generator.Casual.value    },
    { name: 'Tricky',    difficulty: Generator.Tricky.value    },
    { name: 'Difficult', difficulty: Generator.Difficult.value },
    { name: 'Challenge', difficulty: Generator.Challenge.value },
];

const variant = Internal.UniqueDiagonal.value;

const WARMUP_ITERS = 3;
const BENCH_ITERS  = 30;

function p95(samples) {
    const sorted = [...samples].sort((a, b) => a - b);
    const idx = Math.ceil(sorted.length * 0.95) - 1;
    return sorted[idx];
}

function fmt(ms) {
    return ms.toFixed(0).padStart(7) + 'ms';
}

console.log(`Warming up (${WARMUP_ITERS} iterations per difficulty)...`);
for (const { difficulty } of difficulties) {
    for (let i = 0; i < WARMUP_ITERS; i++) {
        Generator.generateSudoku(variant)(difficulty)();
    }
}

console.log(`\nBenchmarking (${BENCH_ITERS} iterations per difficulty)...\n`);

const results = [];
for (const { name, difficulty } of difficulties) {
    const samples = [];
    for (let i = 0; i < BENCH_ITERS; i++) {
        const t0 = performance.now();
        Generator.generateSudoku(variant)(difficulty)();
        samples.push(performance.now() - t0);
    }
    results.push({ name, samples });
}

// Print summary table
const header = ['Difficulty', 'min', 'median', 'mean', 'p95', 'max'];
const rows = results.map(({ name, samples }) => {
    const sorted = [...samples].sort((a, b) => a - b);
    const min    = sorted[0];
    const median = sorted[Math.floor(sorted.length / 2)];
    const mean   = samples.reduce((a, b) => a + b, 0) / samples.length;
    const max    = sorted[sorted.length - 1];
    return [name, fmt(min), fmt(median), fmt(mean), fmt(p95(samples)), fmt(max)];
});

const colWidths = header.map((h, i) =>
    Math.max(h.length, ...rows.map(r => r[i].length))
);

const line = colWidths.map(w => '-'.repeat(w)).join('-+-');
const fmt_row = row => row.map((cell, i) => cell.padStart(colWidths[i])).join(' | ');

console.log(fmt_row(header));
console.log(line);
for (const row of rows) console.log(fmt_row(row));
