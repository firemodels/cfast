"use strict";
async function addManualsNavigation() {
  const catalog = new URL("../../../manuals.json", location.href);
  try {
    const response = await fetch(catalog, {cache: "no-store"});
    if (!response.ok || (await response.json()).kind !== "cfast-manual-catalog") return;
    const header = document.querySelector(".header-links");
    if (!header || header.querySelector(".all-manuals")) return;
    const link = document.createElement("a");
    link.className = "all-manuals";
    const home = new URL("index.html", catalog);
    link.href = home.href;
    link.textContent = "All manuals";
    header.prepend(link);
    document.body.dataset.manualsCatalog = "available";
  } catch { /* Standalone manual servers do not have a shared catalog. */ }
}
addManualsNavigation();
const dialog = document.querySelector("#search-dialog");
const input = document.querySelector("#search-input");
const results = document.querySelector("#search-results");
const status = document.querySelector("#search-status");
let searchIndex;
let searchRequest;
async function loadIndex() {
  if (!searchRequest) searchRequest = fetch("search-index.json").then(response => {
    if (!response.ok) throw new Error("Search index unavailable");
    return response.json();
  });
  searchIndex = await searchRequest;
}
async function openSearch() {
  dialog.showModal();
  input.focus();
  try { await loadIndex(); renderSearch(); }
  catch { status.textContent = "Search could not load. Please reload the page."; }
}
document.querySelector(".search-trigger").addEventListener("click", openSearch);
document.addEventListener("keydown", event => {
  if ((event.metaKey || event.ctrlKey) && event.key.toLowerCase() === "k") {
    event.preventDefault();
    if (dialog.open) dialog.close(); else openSearch();
  }
});
dialog.addEventListener("click", event => { if (event.target === dialog) {
  const box = dialog.getBoundingClientRect();
  if (event.clientX < box.left || event.clientX > box.right || event.clientY < box.top || event.clientY > box.bottom) dialog.close();
}});
function renderSearch() {
  results.replaceChildren();
  const query = input.value.trim().toLowerCase();
  if (!query) { status.textContent = "Search this manual."; return; }
  if (!searchIndex) return;
  const terms = query.split(/\s+/);
  const matches = searchIndex.map(item => {
    const text = (item.title + " " + item.text).toLowerCase();
    return {item, score: terms.every(term => text.includes(term)) ?
      1 + (item.title.toLowerCase().includes(query) ? 10 : 0) + (text.includes(query) ? 3 : 0) : 0};
  }).filter(result => result.score).sort((a, b) => b.score - a.score);
  status.textContent = `${matches.length} matching section${matches.length === 1 ? "" : "s"}`;
  for (const {item} of matches.slice(0, 25)) {
    const link = document.createElement("a");
    link.className = "search-result";
    link.href = item.url;
    const chapter = document.createElement("small"); chapter.textContent = item.chapter;
    const title = document.createElement("strong"); title.textContent = item.title;
    const snippet = document.createElement("p");
    const at = Math.max(0, item.text.toLowerCase().indexOf(terms[0]) - 70);
    snippet.textContent = (at ? "…" : "") + item.text.slice(at, at + 240) + "…";
    link.append(chapter, title, snippet);
    link.addEventListener("click", () => dialog.close());
    results.append(link);
  }
}
input.addEventListener("input", renderSearch);
const menu = document.querySelector(".menu-toggle");
const sidebar = document.querySelector(".sidebar");
const mobile = window.matchMedia("(max-width: 720px)");
function setMenu(open, restoreFocus = false) {
  sidebar.classList.toggle("open", open);
  menu.setAttribute("aria-expanded", String(open));
  sidebar.inert = mobile.matches && !open;
  if (restoreFocus) menu.focus();
}
menu.addEventListener("click", () => setMenu(!sidebar.classList.contains("open")));
mobile.addEventListener("change", () => {
  const wasInside = sidebar.contains(document.activeElement);
  setMenu(false, mobile.matches && wasInside);
});
setMenu(false);
document.addEventListener("keydown", event => {
  // Escape first belongs to the modal search dialog.
  if (event.key === "Escape" && !dialog.open && mobile.matches && sidebar.classList.contains("open")) {
    setMenu(false, true);
  }
});
function updateScrollRegions() {
  for (const region of document.querySelectorAll(".table-scroll")) {
    const overflowing = region.scrollWidth > region.clientWidth + 1;
    if (overflowing) {
      region.setAttribute("tabindex", "0");
      region.setAttribute("role", "region");
      region.setAttribute("aria-label", "Scrollable equation or table");
    } else {
      region.removeAttribute("tabindex");
      region.removeAttribute("role");
      region.removeAttribute("aria-label");
    }
  }
}
const resizeObserver = new ResizeObserver(updateScrollRegions);
for (const region of document.querySelectorAll(".table-scroll")) resizeObserver.observe(region);
window.addEventListener("load", updateScrollRegions);
document.fonts.ready.then(updateScrollRegions);
updateScrollRegions();
const sections = [...document.querySelectorAll("article h2[id], article h3[id]")];
const tocLinks = [...document.querySelectorAll(".page-toc nav a")];
let queued = false;
function highlightSection() {
  const current = [...sections].reverse().find(element => element.getBoundingClientRect().top <= 170) || sections[0];
  for (const link of tocLinks) {
    const active = current && link.hash === "#" + current.id;
    link.classList.toggle("current", !!active);
    if (active) link.setAttribute("aria-current", "location"); else link.removeAttribute("aria-current");
  }
  queued = false;
}
document.addEventListener("scroll", () => { if (!queued) { queued = true; requestAnimationFrame(highlightSection); } }, {passive:true});
highlightSection();

function equationNumberPlacement(region, math, label) {
  const outer = region.getBoundingClientRect();
  const expression = math.getBoundingClientRect();
  const number = label.getBoundingClientRect();
  return {
    width: Math.max(region.clientWidth, expression.right - outer.left + region.scrollLeft),
    top: number.top + number.height / 2 - outer.top + region.scrollTop
  };
}
function positionEquationNumbers() {
  const groups = [];
  for (const math of document.querySelectorAll('math[display="block"]')) {
    const region = math.closest('.table-scroll');
    if (!region) continue;
    // Recognize native arrays as well as newly marked builds, so shared reader
    // updates also fix previously generated manual pages.
    const cells = [];
    for (const table of math.querySelectorAll('mtable')) {
      const numbers = new Set();
      for (const marker of table.querySelectorAll('mstyle.maketag, mtext.eqnarray')) {
        if (marker.closest('mtable') !== table || !marker.textContent.trim()) continue;
        const cell = marker.closest('mtd');
        if (cell) numbers.add(cell);
      }
      if (!numbers.size) continue;
      table.classList.add('numbered-equations');
      for (const cell of numbers) { cell.classList.add('equation-label'); cells.push(cell); }
    }
    if (!cells.length) continue;
    math.classList.add('numbered-math');
    const layer = document.createElement('span');
    layer.className = 'equation-number-layer';
    layer.setAttribute('aria-hidden', 'true');
    const labels = [];
    for (const cell of cells) {
      const text = cell.textContent.trim();
      if (!text) continue;
      const original = cell.querySelector('mtext') || cell;
      const visual = document.createElement('span');
      visual.className = 'equation-number-visual';
      visual.textContent = text;
      layer.append(visual);
      labels.push({original, visual});
    }
    if (!labels.length) continue;
    region.classList.add('numbered-equation-region');
    region.append(layer);
    groups.push({region, math, layer, labels});
  }
  if (!groups.length) return;
  function update() {
    for (const {region, math, layer, labels} of groups) {
      if (!region.clientWidth || !math.getBoundingClientRect().width) continue;
      for (const {original, visual} of labels) {
        const placement = equationNumberPlacement(region, math, original);
        layer.style.width = `${placement.width}px`;
        visual.style.top = `${placement.top}px`;
      }
      math.classList.add('equation-numbers-positioned');
    }
  }
  let scheduled = false;
  function schedule() {
    if (scheduled) return;
    scheduled = true;
    requestAnimationFrame(() => { scheduled = false; update(); });
  }
  const observer = new ResizeObserver(schedule);
  for (const {region, math} of groups) { observer.observe(region); observer.observe(math); }
  window.addEventListener('resize', schedule);
  window.addEventListener('load', schedule);
  document.fonts.ready.then(schedule);
  update();
}
function normalizeMathPresentation(root = document) {
  for (const accent of root.querySelectorAll('math mover')) {
    const mark = accent.lastElementChild;
    if (accent.children.length === 2 && mark.localName === 'mo' &&
        ['˙', '\u0307'].includes(mark.textContent.trim())) {
      accent.classList.add('dot-accent');
    }
  }
  for (const row of root.querySelectorAll('math mtable > mtr')) {
    const cells = [...row.children];
    const labels = cells.filter(cell => cell.matches('.equation-label') ||
      [...cell.querySelectorAll('mtext.eqnarray, mstyle.maketag')].some(marker => marker.closest('mtr') === row));
    if (!labels.length) continue;
    const content = cells.filter(cell => !labels.includes(cell));
    if (content.length && content.every(cell => !cell.textContent.trim() &&
        !cell.querySelector('mi, mn, mo, mtext, mglyph, mspace[width], [id]')) &&
        !row.id && !row.querySelector('[id]')) row.remove();
  }
}
normalizeMathPresentation();
positionEquationNumbers();
