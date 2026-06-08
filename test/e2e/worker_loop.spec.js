const puppeteer = require('puppeteer');
const path = require('path');
const { spawn } = require('child_process');

async function runTest() {
  console.log("Starting E2E test...");
  const browser = await puppeteer.launch({
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  const page = await browser.newPage();
  
  // Emulate Pixel 5
  await page.setUserAgent('Mozilla/5.0 (Linux; Android 11; Pixel 5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.91 Mobile Safari/537.36');
  await page.setViewport({ width: 393, height: 851, isMobile: true, hasTouch: true });

  // Throttle CPU
  const client = await page.target().createCDPSession();
  await client.send('Emulation.setCPUThrottlingRate', { rate: 6 });

  let workerSpawns = 0;
  let errors = [];

  await page.evaluateOnNewDocument(() => {
    const OriginalWorker = window.Worker;
    window.Worker = function(url, options) {
      window.workerSpawns = (window.workerSpawns || 0) + 1;
      console.log(`Worker spawned: ${url} (Total: ${window.workerSpawns})`);
      return new OriginalWorker(url, options);
    };
    
    window.addEventListener('error', e => {
        window.testErrors = window.testErrors || [];
        window.testErrors.push(e.message);
    });
  });

  page.on('console', msg => {
    if (msg.type() === 'error') {
        errors.push(msg.text());
    }
    console.log('PAGE LOG:', msg.text());
  });

  // Navigate to the app
  await page.goto('http://localhost:1234', { waitUntil: 'networkidle2' });

  console.log("App loaded. Starting 50 generations...");

  const ITERATIONS = 50;
  for (let i = 0; i < ITERATIONS; i++) {
    process.stdout.write(`Iteration ${i + 1}/${ITERATIONS}...\r`);
    
    // Wait for Generate button to be enabled
    await page.waitForSelector('#Generate:not([disabled])');
    await page.click('#Generate');
    
    // Wait for "Working..." to disappear (button becomes enabled again)
    await page.waitForSelector('#Generate:not([disabled])', { timeout: 30000 });
    
    // Check for errors in the page
    const pageErrors = await page.evaluate(() => window.testErrors || []);
    if (pageErrors.length > 0) {
        console.error(`\nErrors detected on iteration ${i + 1}:`, pageErrors);
        errors.push(...pageErrors);
        break;
    }
  }
  console.log("\nFinished iterations.");

  workerSpawns = await page.evaluate(() => window.workerSpawns || 0);
  console.log(`Total worker spawns: ${workerSpawns}`);

  await browser.close();

  if (errors.length > 0) {
    console.error("Test failed with errors.");
    process.exit(1);
  }

  // In the current implementation, we expect workerSpawns to be roughly ITERATIONS * k (where k is workers per request)
  // After the fix, it should be <= cap (e.g., 4).
  if (workerSpawns > 10) {
      console.log("Confirmed high worker churn (Phase 1).");
  } else {
      console.log("Low worker churn detected.");
  }
}

runTest().catch(err => {
  console.error(err);
  process.exit(1);
});
