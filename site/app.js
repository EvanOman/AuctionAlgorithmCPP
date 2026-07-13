"use strict";

/* ============================================================
   The Auction Algorithm — demo site logic
   Loads the real C++ solver (compiled to WASM), drives the
   live courier/job animation, the playground, and static
   benchmark data on the algorithm tab.
   ============================================================ */

/* ---------------------------------------------------------------
   Tab / segmented-control wiring (generic)
   --------------------------------------------------------------- */

function wireTabGroup(buttonIds, panelIds) {
  buttonIds.forEach((btnId, i) => {
    const btn = document.getElementById(btnId);
    btn.addEventListener("click", () => {
      buttonIds.forEach((id, j) => {
        document.getElementById(id).setAttribute("aria-selected", String(i === j));
        const panel = document.getElementById(panelIds[j]);
        if (panel) panel.hidden = i !== j;
      });
    });
  });
}

wireTabGroup(
  ["tab-btn-demo", "tab-btn-playground", "tab-btn-algorithm"],
  ["tab-demo", "tab-playground", "tab-algorithm"]
);

wireTabGroup(["mode-btn-editor", "mode-btn-upload"], ["mode-editor", "mode-upload"]);

/* ---------------------------------------------------------------
   WASM module loading
   --------------------------------------------------------------- */

const wasmStatusEl = document.getElementById("wasmStatus");
const wasmStatusText = document.getElementById("wasmStatusText");

let Module = null;
let solveTextRaw = null;

function solveProblemText(text, maxEvents = 0) {
  const ptr = solveTextRaw(text, maxEvents);
  const json = Module.UTF8ToString(ptr);
  Module._auction_free_json(ptr);
  return JSON.parse(json);
}

createAuctionModule()
  .then((M) => {
    Module = M;
    solveTextRaw = M.cwrap("auction_solve_problem_text_json", "number", ["string", "number"]);
    wasmStatusEl.classList.add("ready");
    wasmStatusText.textContent = "auction.wasm loaded — solving runs entirely in your browser";
    initDemo();
    initPlayground();
  })
  .catch((err) => {
    wasmStatusEl.classList.add("error");
    wasmStatusText.textContent = "failed to load auction.wasm: " + err;
    console.error(err);
  });

/* ---------------------------------------------------------------
   Shared problem-text helpers
   --------------------------------------------------------------- */

function buildProblemText(costs, n, objective) {
  const lines = [`objective ${objective}`, `n ${n}`];
  for (let i = 0; i < n; i++) {
    const row = [];
    for (let j = 0; j < n; j++) row.push(costs[i * n + j]);
    lines.push(row.join(" "));
  }
  return lines.join("\n") + "\n";
}

// Best-effort client-side parse, used only to drive display (cost columns,
// the cost-matrix render). The authoritative parser — the one whose error
// messages are shown to the user — is the C++ parser running in wasm.
function parseProblemTextForDisplay(text) {
  const lines = text.split(/\r?\n/);
  let n = null;
  let objective = "min";
  const rows = [];
  for (const raw of lines) {
    const line = raw.replace(/#.*/, "").trim();
    if (!line) continue;
    const parts = line.split(/\s+/);
    if (parts[0] === "objective") {
      objective = parts[1];
      continue;
    }
    if (parts[0] === "n") {
      n = parseInt(parts[1], 10);
      continue;
    }
    rows.push(parts.map(Number));
  }
  if (!n || rows.length !== n) return null;
  const costs = [];
  for (const r of rows) {
    if (r.length !== n || r.some(Number.isNaN)) return null;
    costs.push(...r);
  }
  return { n, objective, costs };
}

/* ============================================================
   TAB 1 — Live demo
   ============================================================ */

function initDemo() {
  const canvas = document.getElementById("demoCanvas");
  const ctx = canvas.getContext("2d");
  const LOGICAL_W = 900;
  const LOGICAL_H = 675;
  const PAD = 56;

  function setupCanvasDPR() {
    const dpr = window.devicePixelRatio || 1;
    canvas.width = LOGICAL_W * dpr;
    canvas.height = LOGICAL_H * dpr;
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
  }
  setupCanvasDPR();

  const nSlider = document.getElementById("nSlider");
  const nValue = document.getElementById("nValue");
  const speedSlider = document.getElementById("speedSlider");
  const speedValue = document.getElementById("speedValue");
  const rerollBtn = document.getElementById("rerollBtn");
  const playPauseBtn = document.getElementById("playPauseBtn");
  const skipBtn = document.getElementById("skipBtn");

  const $ = (sel) => document.querySelector(sel);
  const statPhaseVal = $("#statPhase .value");
  const statRoundVal = $("#statRound .value");
  const statEpsilonVal = $("#statEpsilon .value");
  const statEventVal = $("#statEvent .value");
  const statTotalVal = $("#statTotal");
  const statPhasesVal = $("#statPhases");
  const statRoundsVal = $("#statRounds");
  const statEventsVal = $("#statEvents");
  const auctionBar = document.getElementById("auctionBar");
  const auctionNum = document.getElementById("auctionNum");
  const greedyBar = document.getElementById("greedyBar");
  const greedyNum = document.getElementById("greedyNum");
  const compareVerdict = document.getElementById("compareVerdict");

  const FLASH_DURATION = 550;

  const demo = {
    n: 12,
    courierPts: [],
    jobPts: [],
    costs: null,
    result: null,
    events: [],
    eventIndex: 0,
    phase: null,
    currentAssignment: new Map(), // person -> object
    lastPrice: [],
    flashes: [], // {type:'award'|'displace', person, object, t}
    playing: false,
    done: false,
    greedyTotal: 0,
    maxCost: 1,
  };

  function rand(lo, hi) {
    return lo + Math.random() * (hi - lo);
  }

  function dist(a, b) {
    return Math.hypot(a.x - b.x, a.y - b.y);
  }

  function generateInstance(n) {
    demo.n = n;
    demo.courierPts = Array.from({ length: n }, () => ({ x: rand(0.07, 0.93), y: rand(0.09, 0.91) }));
    demo.jobPts = Array.from({ length: n }, () => ({ x: rand(0.07, 0.93), y: rand(0.09, 0.91) }));
    const costs = new Array(n * n);
    let maxCost = 1;
    for (let i = 0; i < n; i++) {
      for (let j = 0; j < n; j++) {
        const c = Math.round(dist(demo.courierPts[i], demo.jobPts[j]) * 100);
        costs[i * n + j] = c;
        if (c > maxCost) maxCost = c;
      }
    }
    demo.costs = costs;
    demo.maxCost = maxCost;
  }

  function computeGreedy() {
    const n = demo.n;
    const costs = demo.costs;
    const usedJobs = new Array(n).fill(false);
    let total = 0;
    for (let i = 0; i < n; i++) {
      let bestJ = -1;
      let bestC = Infinity;
      for (let j = 0; j < n; j++) {
        if (usedJobs[j]) continue;
        const c = costs[i * n + j];
        if (c < bestC) {
          bestC = c;
          bestJ = j;
        }
      }
      usedJobs[bestJ] = true;
      total += bestC;
    }
    demo.greedyTotal = total;
  }

  function toPixel(pt) {
    return {
      x: PAD + pt.x * (LOGICAL_W - 2 * PAD),
      y: PAD + pt.y * (LOGICAL_H - 2 * PAD),
    };
  }

  function resetAnimationState() {
    demo.eventIndex = 0;
    demo.phase = null;
    demo.currentAssignment = new Map();
    demo.lastPrice = new Array(demo.n).fill(0);
    demo.flashes = [];
    demo.playing = false;
    demo.done = false;
    playPauseBtn.innerHTML = "&#9654; Play";
    statPhaseVal.textContent = "—";
    statRoundVal.textContent = "—";
    statEpsilonVal.textContent = "—";
    statEventVal.textContent = `0 / ${demo.events.length}`;
    statTotalVal.textContent = "—";
    statPhasesVal.textContent = "—";
    statRoundsVal.textContent = "—";
    statEventsVal.textContent = "—";
    auctionBar.style.width = "0%";
    greedyBar.style.width = "0%";
    auctionNum.textContent = "—";
    greedyNum.textContent = "—";
    compareVerdict.textContent = "Watching the auction run…";
  }

  function runSolve() {
    const text = buildProblemText(demo.costs, demo.n, "min");
    const result = solveProblemText(text, 0);
    if (result.error) {
      // Shouldn't happen for a generated instance; surface it defensively.
      console.error("demo solve error:", result.error);
      demo.result = null;
      demo.events = [];
      return;
    }
    demo.result = result;
    demo.events = result.events || [];
  }

  function eventsPerSecond() {
    return 2 * Number(speedSlider.value);
  }

  function processNextEvent() {
    const ev = demo.events[demo.eventIndex++];
    if (demo.phase !== ev.phase) {
      demo.currentAssignment.clear();
      demo.phase = ev.phase;
    }
    if (ev.displaced !== -1 && ev.displaced !== undefined) {
      demo.currentAssignment.delete(ev.displaced);
      demo.flashes.push({ type: "displace", person: ev.displaced, object: ev.object, t: performance.now() });
    }
    demo.currentAssignment.set(ev.winner, ev.object);
    demo.lastPrice[ev.object] = ev.price;
    demo.flashes.push({ type: "award", person: ev.winner, object: ev.object, t: performance.now() });

    statPhaseVal.textContent = ev.phase;
    statRoundVal.textContent = ev.round;
    statEpsilonVal.textContent = ev.epsilon.toFixed(4);
    statEventVal.textContent = `${demo.eventIndex} / ${demo.events.length}`;
  }

  function finishAnimation() {
    demo.playing = false;
    demo.done = true;
    playPauseBtn.innerHTML = "&#8635; Replay";

    if (demo.result) {
      statTotalVal.textContent = demo.result.total_cost;
      statPhasesVal.textContent = demo.result.phases;
      statRoundsVal.textContent = demo.result.rounds;
      statEventsVal.textContent = demo.events.length;

      const auctionTotal = demo.result.total_cost;
      const greedyTotal = demo.greedyTotal;
      const maxVal = Math.max(auctionTotal, greedyTotal, 1);
      auctionBar.style.width = `${(auctionTotal / maxVal) * 100}%`;
      greedyBar.style.width = `${(greedyTotal / maxVal) * 100}%`;
      auctionNum.textContent = auctionTotal;
      greedyNum.textContent = greedyTotal;

      if (greedyTotal <= 0) {
        compareVerdict.textContent = "Both baselines total zero on this instance.";
      } else if (auctionTotal >= greedyTotal) {
        compareVerdict.textContent = "Auction matches the greedy baseline on this instance.";
      } else {
        const pct = ((greedyTotal - auctionTotal) / greedyTotal) * 100;
        compareVerdict.innerHTML = `Auction total is <strong>${pct.toFixed(1)}% better</strong> than greedy nearest-courier.`;
      }
    }
  }

  function togglePlayPause() {
    if (demo.done) {
      resetAnimationState();
      demo.playing = true;
      playPauseBtn.innerHTML = "&#10074;&#10074; Pause";
      return;
    }
    demo.playing = !demo.playing;
    playPauseBtn.innerHTML = demo.playing ? "&#10074;&#10074; Pause" : "&#9654; Play";
  }

  function skipToEnd() {
    while (demo.eventIndex < demo.events.length) processNextEvent();
    finishAnimation();
  }

  function rerollInstance() {
    generateInstance(Number(nSlider.value));
    computeGreedy();
    runSolve();
    resetAnimationState();
    demo.playing = true;
    playPauseBtn.innerHTML = "&#10074;&#10074; Pause";
  }

  /* -------- drawing -------- */

  function drawGrid() {
    ctx.strokeStyle = "rgba(255,255,255,0.035)";
    ctx.lineWidth = 1;
    const step = 45;
    for (let x = 0; x <= LOGICAL_W; x += step) {
      ctx.beginPath();
      ctx.moveTo(x + 0.5, 0);
      ctx.lineTo(x + 0.5, LOGICAL_H);
      ctx.stroke();
    }
    for (let y = 0; y <= LOGICAL_H; y += step) {
      ctx.beginPath();
      ctx.moveTo(0, y + 0.5);
      ctx.lineTo(LOGICAL_W, y + 0.5);
      ctx.stroke();
    }
  }

  function drawCanvas() {
    ctx.clearRect(0, 0, LOGICAL_W, LOGICAL_H);
    ctx.fillStyle = "#08090b";
    ctx.fillRect(0, 0, LOGICAL_W, LOGICAL_H);
    drawGrid();

    const n = demo.n;
    if (!n || !demo.jobPts.length) return;

    // price rings behind job markers
    for (let j = 0; j < n; j++) {
      const p = toPixel(demo.jobPts[j]);
      const price = demo.lastPrice[j] || 0;
      const radius = Math.min(30, 6 + price * 0.28);
      ctx.beginPath();
      ctx.arc(p.x, p.y, radius, 0, Math.PI * 2);
      ctx.strokeStyle = "rgba(255,176,0,0.45)";
      ctx.lineWidth = 1.2;
      ctx.stroke();
    }

    // edges
    if (demo.done && demo.result) {
      ctx.lineWidth = 2.6;
      ctx.strokeStyle = "#ffb000";
      ctx.shadowColor = "rgba(255,176,0,0.55)";
      ctx.shadowBlur = 6;
      for (let i = 0; i < n; i++) {
        const j = demo.result.assignment[i];
        const a = toPixel(demo.courierPts[i]);
        const b = toPixel(demo.jobPts[j]);
        ctx.beginPath();
        ctx.moveTo(a.x, a.y);
        ctx.lineTo(b.x, b.y);
        ctx.stroke();
      }
      ctx.shadowBlur = 0;
    } else {
      ctx.lineWidth = 1.6;
      ctx.strokeStyle = "rgba(255,176,0,0.75)";
      demo.currentAssignment.forEach((j, i) => {
        const a = toPixel(demo.courierPts[i]);
        const b = toPixel(demo.jobPts[j]);
        ctx.beginPath();
        ctx.moveTo(a.x, a.y);
        ctx.lineTo(b.x, b.y);
        ctx.stroke();
      });
    }

    // flashes (fade in/out over FLASH_DURATION)
    const now = performance.now();
    demo.flashes = demo.flashes.filter((f) => now - f.t < FLASH_DURATION);
    for (const f of demo.flashes) {
      const age = now - f.t;
      const alpha = 1 - age / FLASH_DURATION;
      const objPos = toPixel(demo.jobPts[f.object]);
      if (f.type === "award") {
        const r = 14 + (age / FLASH_DURATION) * 22;
        ctx.beginPath();
        ctx.arc(objPos.x, objPos.y, r, 0, Math.PI * 2);
        ctx.strokeStyle = `rgba(255,176,0,${alpha})`;
        ctx.lineWidth = 2;
        ctx.stroke();
      } else if (f.type === "displace") {
        const personPos = toPixel(demo.courierPts[f.person]);
        ctx.beginPath();
        ctx.moveTo(personPos.x, personPos.y);
        ctx.lineTo(objPos.x, objPos.y);
        ctx.strokeStyle = `rgba(255,92,92,${alpha})`;
        ctx.lineWidth = 2.4;
        ctx.stroke();
      }
    }

    // job markers (squares)
    const showLabels = n <= 16;
    ctx.font = "10px " + getComputedStyle(document.body).fontFamily;
    for (let j = 0; j < n; j++) {
      const p = toPixel(demo.jobPts[j]);
      ctx.fillStyle = "#0d1117";
      ctx.strokeStyle = "#46e0c4";
      ctx.lineWidth = 1.6;
      const s = 7;
      ctx.beginPath();
      ctx.rect(p.x - s, p.y - s, s * 2, s * 2);
      ctx.fill();
      ctx.stroke();
      if (showLabels) {
        ctx.fillStyle = "#46e0c4";
        ctx.fillText("J" + j, p.x + 10, p.y - 8);
      }
    }

    // courier markers (circles)
    for (let i = 0; i < n; i++) {
      const p = toPixel(demo.courierPts[i]);
      ctx.beginPath();
      ctx.arc(p.x, p.y, 7, 0, Math.PI * 2);
      ctx.fillStyle = "#08090b";
      ctx.fill();
      ctx.lineWidth = 1.8;
      ctx.strokeStyle = "#ffb000";
      ctx.stroke();
      if (showLabels) {
        ctx.fillStyle = "#ffb000";
        ctx.fillText("C" + i, p.x + 10, p.y + 14);
      }
    }
  }

  /* -------- animation loop -------- */

  let lastTs = 0;
  let acc = 0;
  function loop(ts) {
    if (!lastTs) lastTs = ts;
    const dt = ts - lastTs;
    lastTs = ts;
    if (demo.playing) {
      acc += dt;
      const interval = 1000 / eventsPerSecond();
      while (acc >= interval && demo.eventIndex < demo.events.length) {
        processNextEvent();
        acc -= interval;
      }
      if (demo.eventIndex >= demo.events.length) {
        finishAnimation();
        acc = 0;
      }
    }
    drawCanvas();
    requestAnimationFrame(loop);
  }
  requestAnimationFrame(loop);

  /* -------- controls -------- */

  nSlider.addEventListener("input", () => {
    nValue.textContent = nSlider.value;
  });
  nSlider.addEventListener("change", () => {
    rerollInstance();
  });
  speedSlider.addEventListener("input", () => {
    speedValue.textContent = speedSlider.value + "×";
  });
  rerollBtn.addEventListener("click", rerollInstance);
  playPauseBtn.addEventListener("click", togglePlayPause);
  skipBtn.addEventListener("click", skipToEnd);

  // initial instance
  generateInstance(Number(nSlider.value));
  computeGreedy();
  runSolve();
  resetAnimationState();
  demo.playing = true;
  playPauseBtn.innerHTML = "&#10074;&#10074; Pause";
  drawCanvas();
}

/* ============================================================
   TAB 2 — Playground
   ============================================================ */

function initPlayground() {
  const problemInput = document.getElementById("problemInput");
  const fileInput = document.getElementById("fileInput");
  const genN = document.getElementById("genN");
  const genLo = document.getElementById("genLo");
  const genHi = document.getElementById("genHi");
  const genObjective = document.getElementById("genObjective");
  const generateBtn = document.getElementById("generateBtn");
  const solveBtn = document.getElementById("solveBtn");
  const downloadBtn = document.getElementById("downloadBtn");
  const errorBox = document.getElementById("errorBox");
  const resultEmpty = document.getElementById("resultEmpty");
  const resultContent = document.getElementById("resultContent");
  const resObjective = document.getElementById("resObjective");
  const resN = document.getElementById("resN");
  const resTotal = document.getElementById("resTotal");
  const resPhases = document.getElementById("resPhases");
  const resRounds = document.getElementById("resRounds");
  const resTruncated = document.getElementById("resTruncated");
  const assignTableBody = document.getElementById("assignTableBody");
  const matrixWrap = document.getElementById("matrixWrap");
  const modeBtnEditor = document.getElementById("mode-btn-editor");
  const modeBtnUpload = document.getElementById("mode-btn-upload");

  function showError(message) {
    errorBox.textContent = message;
    errorBox.classList.add("show");
    resultContent.hidden = true;
    resultEmpty.hidden = false;
    resultEmpty.textContent = "Fix the error above and solve again.";
  }

  function clearError() {
    errorBox.classList.remove("show");
    errorBox.textContent = "";
  }

  function renderMatrix(info, assignment) {
    matrixWrap.innerHTML = "";
    if (!info || info.n > 12) return;
    const { n, costs } = info;
    const table = document.createElement("table");
    table.className = "cost-matrix";
    const thead = document.createElement("tr");
    thead.appendChild(document.createElement("th"));
    for (let j = 0; j < n; j++) {
      const th = document.createElement("th");
      th.textContent = "J" + j;
      thead.appendChild(th);
    }
    table.appendChild(thead);
    for (let i = 0; i < n; i++) {
      const tr = document.createElement("tr");
      const rowHead = document.createElement("th");
      rowHead.textContent = "P" + i;
      tr.appendChild(rowHead);
      for (let j = 0; j < n; j++) {
        const td = document.createElement("td");
        td.textContent = costs[i * n + j];
        if (assignment[i] === j) td.classList.add("chosen");
        tr.appendChild(td);
      }
      table.appendChild(tr);
    }
    matrixWrap.appendChild(table);
  }

  function renderResult(result, text) {
    resultEmpty.hidden = true;
    resultContent.hidden = false;

    resObjective.textContent = result.objective;
    resN.textContent = result.n;
    resTotal.textContent = result.total_cost;
    resPhases.textContent = result.phases;
    resRounds.textContent = result.rounds;
    resTruncated.textContent = result.truncated ? "yes" : "no";

    const info = parseProblemTextForDisplay(text);

    assignTableBody.innerHTML = "";
    for (let i = 0; i < result.assignment.length; i++) {
      const j = result.assignment[i];
      const tr = document.createElement("tr");
      const cost = info && info.n === result.n ? info.costs[i * info.n + j] : "—";
      tr.innerHTML = `<td>${i}</td><td>${j}</td><td>${cost}</td>`;
      assignTableBody.appendChild(tr);
    }

    if (info && info.n === result.n) {
      renderMatrix(info, result.assignment);
    } else {
      matrixWrap.innerHTML = "";
    }
  }

  function doSolve() {
    clearError();
    const text = problemInput.value;
    const result = solveProblemText(text, 0);
    if (result.error) {
      showError(result.error);
      return;
    }
    renderResult(result, text);
  }

  function doGenerate() {
    const n = Math.max(1, Math.min(500, parseInt(genN.value, 10) || 1));
    const lo = parseInt(genLo.value, 10) || 0;
    const hi = parseInt(genHi.value, 10) || 0;
    const objective = genObjective.value;
    const lines = [`objective ${objective}`, `n ${n}`];
    const [a, b] = lo <= hi ? [lo, hi] : [hi, lo];
    for (let i = 0; i < n; i++) {
      const row = [];
      for (let j = 0; j < n; j++) row.push(a + Math.floor(Math.random() * (b - a + 1)));
      lines.push(row.join(" "));
    }
    problemInput.value = lines.join("\n") + "\n";
    clearError();
    resultContent.hidden = true;
    resultEmpty.hidden = false;
    resultEmpty.textContent = "No solve run yet.";
    modeBtnEditor.click();
  }

  function doDownload() {
    const blob = new Blob([problemInput.value], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = "problem.apf";
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  }

  fileInput.addEventListener("change", () => {
    const file = fileInput.files[0];
    if (!file) return;
    const reader = new FileReader();
    reader.onload = () => {
      problemInput.value = String(reader.result);
      clearError();
      modeBtnEditor.click();
    };
    reader.readAsText(file);
  });

  solveBtn.addEventListener("click", doSolve);
  generateBtn.addEventListener("click", doGenerate);
  downloadBtn.addEventListener("click", doDownload);

  // Solve the prefilled example immediately so the panel isn't empty.
  doSolve();
}

/* ============================================================
   TAB 3 — The algorithm: benchmark table
   ============================================================ */

(function renderBenchTable() {
  const rows = [
    ["uniform_narrow (costs 1..n)", 100, 0.54, 0.275],
    ["uniform_narrow (costs 1..n)", 250, 3.641, 1.686],
    ["uniform_narrow (costs 1..n)", 500, 15.246, 7.239],
    ["uniform_narrow (costs 1..n)", 1000, 67.526, 40.117],
    ["uniform_narrow (costs 1..n)", 2000, 302.777, 197.384],
    ["uniform_wide (costs 1..1000n)", 100, 0.561, 0.266],
    ["uniform_wide (costs 1..1000n)", 250, 3.556, 1.998],
    ["uniform_wide (costs 1..1000n)", 500, 14.205, 10.493],
    ["uniform_wide (costs 1..1000n)", 1000, 64.512, 55.9],
    ["uniform_wide (costs 1..1000n)", 2000, 335.217, 318.43],
  ];
  const tbody = document.getElementById("benchTableBody");
  tbody.innerHTML = rows
    .map(([family, n, auctionMs, scipyMs]) => {
      const ratio = auctionMs / scipyMs;
      return `<tr>
        <td class="family">${family}</td>
        <td>${n}</td>
        <td>${auctionMs.toFixed(3)}</td>
        <td>${scipyMs.toFixed(3)}</td>
        <td class="ratio">${ratio.toFixed(2)}×</td>
      </tr>`;
    })
    .join("");
})();
