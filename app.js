// Core math utilities for average deposit calculations
// Definitions:
// - avgToDate: average deposit until today (for elapsedDays days)
// - elapsedDays: number of days counted so far
// - currentBalance: current end-of-day point-in-time deposit that will be held for the remaining period
// - targetDays (T): target total counted days in cycle
// - threshold: required average to meet (e.g., 10000 for 达标户, 500000 for 中型)

/**
 * Compute whether average reaches threshold at T if currentBalance held for remaining days.
 * avg_T = (avgToDate * elapsedDays + currentBalance * (T - elapsedDays)) / T
 */
function isMeetingAtT(avgToDate, elapsedDays, currentBalance, targetDays, threshold) {
  if (!isFiniteValues(avgToDate, elapsedDays, currentBalance, targetDays) || targetDays <= 0) return false;
  const T = targetDays;
  const D = elapsedDays;
  const remain = Math.max(0, T - D);
  const sum = avgToDate * D + currentBalance * remain;
  const avgT = sum / T;
  return avgT >= threshold - 1e-9; // numeric tolerance
}

/**
 * Compute minimal currentBalance needed (held constant for remaining days) so that avg at T hits threshold.
 * Solve for M in: (avgToDate*D + M*(T-D)) / T >= threshold -> M >= (threshold*T - avgToDate*D) / (T-D)
 * Edge cases:
 * - If T == D: no remaining days; requirement reduces to avgToDate >= threshold
 */
function requiredBalanceForT(avgToDate, elapsedDays, targetDays, threshold) {
  if (!isFiniteValues(avgToDate, elapsedDays, targetDays) || targetDays <= 0) return NaN;
  const T = targetDays;
  const D = elapsedDays;
  if (T === D) {
    return avgToDate >= threshold ? 0 : Infinity;
  }
  const numerator = threshold * T - avgToDate * D;
  const denom = T - D;
  const M = numerator / denom;
  return M;
}

/**
 * Given currentBalance M held for future days, compute minimal extra days N needed to reach threshold average.
 * Let total days = D + N; Condition: (avgToDate*D + M*N) / (D + N) >= threshold
 * Solve: avgToDate*D + M*N >= threshold*D + threshold*N -> N*(M - threshold) >= D*(threshold - avgToDate)
 * -> If M == threshold: require RHS <= 0 to meet; otherwise impossible unless already met.
 * -> If M > threshold: N >= D*(threshold - avgToDate) / (M - threshold)
 * -> If M < threshold: impossible to improve average to threshold with constant M; return Infinity
 */
function daysNeededWithM(avgToDate, elapsedDays, currentBalance, threshold) {
  if (!isFiniteValues(avgToDate, elapsedDays, currentBalance)) return NaN;
  const D = Math.max(0, elapsedDays);
  const M = currentBalance;
  const rhs = D * (threshold - avgToDate);
  if (Math.abs(M - threshold) < 1e-12) {
    if (rhs <= 0) return 0; // already at or above threshold
    return Infinity; // cannot move upwards if M equals threshold exactly
  }
  if (M > threshold) {
    const N = rhs / (M - threshold);
    return Math.max(0, N);
  }
  if (M === Infinity) return 0;
  return rhs <= 0 ? 0 : Infinity;
}

function isFiniteValues(...vals) { return vals.every(v => typeof v === 'number' && isFinite(v)); }

function fmtMoney(n) {
  if (n === Infinity) return '∞';
  if (Number.isNaN(n)) return '—';
  return new Intl.NumberFormat('zh-CN', { style: 'currency', currency: 'CNY', maximumFractionDigits: 2 }).format(n);
}

function fmtNumber(n, digits = 2) {
  if (n === Infinity) return '∞';
  if (Number.isNaN(n)) return '—';
  return new Intl.NumberFormat('zh-CN', { maximumFractionDigits: digits }).format(n);
}

function readNumber(id) {
  const el = document.getElementById(id);
  const v = parseFloat(el.value);
  return isFinite(v) ? v : NaN;
}

function writeText(id, text) { const el = document.getElementById(id); if (el) el.textContent = text; }
function writeHtml(id, html) { const el = document.getElementById(id); if (el) el.innerHTML = html; }

function parseDateValue(id) {
  const el = document.getElementById(id);
  if (!el || !el.value) return null;
  const d = new Date(el.value + 'T00:00:00');
  return isNaN(d.getTime()) ? null : d;
}

function dayOfYear(d) {
  const start = new Date(d.getFullYear(), 0, 1);
  const diff = d - start;
  const oneDay = 24 * 60 * 60 * 1000;
  return Math.floor(diff / oneDay) + 1;
}

function daysSinceYearStartExclusive(d) {
  return Math.max(0, dayOfYear(d) - 1);
}

function formatDateLocal(d) {
  const y = d.getFullYear();
  const m = String(d.getMonth() + 1).padStart(2, '0');
  const day = String(d.getDate()).padStart(2, '0');
  return `${y}-${m}-${day}`;
}

function getThresholdByLevel(level) { return level === 'mid' ? 500000 : 10000; }

function isHitByMode(avgToDate, elapsedDays, threshold, mode) {
  const D = Math.max(0, elapsedDays);
  if (!isFiniteValues(avgToDate, D, threshold)) return false;
  if (mode === 'year') {
    return avgToDate * D >= threshold * 365;
  }
  // point mode: reduces to avgToDate >= threshold (when D>0), treat D=0 as not hit
  if (D === 0) return false;
  return avgToDate >= threshold - 1e-9;
}

function computeSingle() {
  const avgToDate = readNumber('avg-to-date');
  const currentBalance = readNumber('current-balance');
  const statsDate = parseDateValue('stats-date');
  const targetDate = parseDateValue('target-date');
  const levelEl = document.getElementById('level-select');
  const level = levelEl && levelEl.value ? levelEl.value : 'basic';
  const modeEl = document.getElementById('mode-select');
  const mode = modeEl && modeEl.value ? modeEl.value : 'point';

  if (!isFinite(avgToDate) || !isFinite(currentBalance) || !statsDate || !targetDate || targetDate.getTime() < statsDate.getTime()) {
    writeHtml('kpi-status', '—');
    writeText('kpi-avg', '—');
    writeText('kpi-needed', '—');
    writeText('kpi-days', '—');
    return;
  }

  const elapsedDays = daysSinceYearStartExclusive(statsDate);
  const targetDays = daysSinceYearStartExclusive(targetDate);
  const threshold = getThresholdByLevel(level);

  const hit = isHitByMode(avgToDate, elapsedDays, threshold, mode);
  // Year-end required balance (for the KPI "年末达标需时点存款")
  const yearEnd = new Date(statsDate.getFullYear(), 11, 31);
  const yearEndDays = daysSinceYearStartExclusive(yearEnd);
  const needM = requiredBalanceForT(avgToDate, elapsedDays, yearEndDays, threshold);
  // Year-end requirement: minimal holding days N with currentBalance to reach threshold by 12/31
  const remainingDays = Math.max(0, daysSinceYearStartExclusive(yearEnd) - elapsedDays);
  const needDaysYear = daysNeededWithM(avgToDate, elapsedDays, currentBalance, threshold);
  const avgT = targetDays > 0 ? (avgToDate * elapsedDays + currentBalance * Math.max(0, targetDays - elapsedDays)) / targetDays : NaN;

  writeHtml('kpi-status', hit ? `<span class="ok">已达标</span>` : `<span class="not-ok">未达标</span>`);
  writeText('kpi-avg', `${formatDateLocal(targetDate)} 日均：${fmtNumber(avgT)}`);
  writeText('kpi-needed', needM <= 0 ? '0（无需增加）' : (needM === Infinity ? '不可达' : fmtMoney(needM)));
  if (needDaysYear === Infinity) {
    writeText('kpi-days', '不可达（需提高M）');
  } else if (needDaysYear > remainingDays) {
    writeText('kpi-days', `不可达（至少需要${fmtNumber(Math.ceil(needDaysYear), 0)}天）`);
  } else {
    const attain = new Date(statsDate);
    attain.setDate(attain.getDate() + Math.ceil(needDaysYear));
    writeText('kpi-days', `${formatDateLocal(attain)}`);
  }
}

function setupEvents() {
  const inputs = ['avg-to-date', 'current-balance', 'stats-date', 'target-date', 'level-select', 'mode-select'];
  inputs.forEach(id => {
    const el = document.getElementById(id);
    if (el) el.addEventListener('input', computeSingle);
    if (el) el.addEventListener('change', computeSingle);
  });

  const btnBatch = document.getElementById('btn-run-batch');
  if (btnBatch) btnBatch.addEventListener('click', handleBatch);

  const btnQ3 = document.getElementById('btn-q3-end');
  if (btnQ3) btnQ3.addEventListener('click', () => { setQuickTarget('q3'); computeSingle(); });
  const btnYear = document.getElementById('btn-year-end');
  if (btnYear) btnYear.addEventListener('click', () => { setQuickTarget('year'); computeSingle(); });
}

function setQuickTarget(type) {
  const statsDate = parseDateValue('stats-date');
  const today = new Date();
  const baseYear = statsDate ? statsDate.getFullYear() : today.getFullYear();
  let target;
  if (type === 'q3') {
    target = new Date(baseYear, 8, 30); // Sep 30
  } else {
    target = new Date(baseYear, 11, 31); // Dec 31
  }
  const input = document.getElementById('target-date');
  if (input) input.value = formatDateLocal(target);
}

function handleBatch() {
  const fileInput = document.getElementById('csv-file');
  const file = fileInput.files && fileInput.files[0];
  if (!file) { alert('请先选择 CSV 文件'); return; }

  Papa.parse(file, {
    header: true,
    skipEmptyLines: true,
    complete: (res) => {
      const rows = (res.data || []).map(r => ({
        name: r.name || '',
        id: r.id || '',
        avgToDate: parseFloat(r.avg_to_date),
        currentBalance: parseFloat(r.current_balance),
        statsDate: r.stats_date ? new Date(r.stats_date + 'T00:00:00') : null,
        targetDate: r.target_date ? new Date(r.target_date + 'T00:00:00') : null,
        level: (r.level || 'basic').trim(),
      }));
      const tbody = document.querySelector('#batch-table tbody');
      tbody.innerHTML = '';
      rows.forEach(r => {
        let cells = {};
        if (!r.statsDate || !r.targetDate || !isFinite(r.avgToDate) || !isFinite(r.currentBalance)) {
          cells = { hitText: '数据不完整', needMText: '—', needDaysText: '—' };
        } else if (r.targetDate.getTime() < r.statsDate.getTime()) {
          cells = { hitText: '目标早于统计日', needMText: '—', needDaysText: '—' };
        } else {
          const elapsedDays = daysSinceYearStartExclusive(r.statsDate);
          const targetDays = daysSinceYearStartExclusive(r.targetDate);
          const threshold = getThresholdByLevel(r.level === 'mid' ? 'mid' : 'basic');
          const modeEl = document.getElementById('mode-select');
          const mode = modeEl && modeEl.value ? modeEl.value : 'point';
          const hit = isHitByMode(r.avgToDate, elapsedDays, threshold, mode);
          const yearEnd = new Date(r.statsDate.getFullYear(), 11, 31);
          const yearEndDays = daysSinceYearStartExclusive(yearEnd);
          const needM = requiredBalanceForT(r.avgToDate, elapsedDays, yearEndDays, threshold);
          const remainingDays = Math.max(0, daysSinceYearStartExclusive(yearEnd) - elapsedDays);
          const needDaysYear = daysNeededWithM(r.avgToDate, elapsedDays, r.currentBalance, threshold);
          cells.hitText = hit ? '达标' : '未达标';
          cells.hitClass = hit ? 'ok' : 'not-ok';
          cells.needMText = needM <= 0 ? '0' : (needM === Infinity ? '不可达' : fmtMoney(needM));
          if (needDaysYear === Infinity) {
            cells.needDaysText = '不可达';
          } else if (needDaysYear > remainingDays) {
            cells.needDaysText = `不可达(需≥${fmtNumber(Math.ceil(needDaysYear), 0)}天)`;
          } else {
            const attain = new Date(r.statsDate);
            attain.setDate(attain.getDate() + Math.ceil(needDaysYear));
            cells.needDaysText = formatDateLocal(attain);
          }
        }

        const tr = document.createElement('tr');
        tr.innerHTML = `
          <td>${escapeHtml(r.name)}</td>
          <td>${escapeHtml(r.id)}</td>
          <td>${fmtNumber(r.avgToDate)}</td>
          <td>${fmtMoney(r.currentBalance)}</td>
          <td>${r.statsDate ? formatDateLocal(r.statsDate) : '—'}</td>
          <td>${r.targetDate ? formatDateLocal(r.targetDate) : '—'}</td>
          <td>${r.level === 'mid' ? '中型' : '达标'}</td>
          <td class="${cells.hitClass || ''}">${cells.hitText}</td>
          <td>${cells.needMText}</td>
          <td>${cells.needDaysText}</td>
        `;
        tbody.appendChild(tr);
      });
    }
  });
}

function escapeHtml(s) { return String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c])); }

document.addEventListener('DOMContentLoaded', setupEvents);


