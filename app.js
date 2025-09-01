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

function computeSingle() {
  const basic = readNumber('threshold-basic') || 10000;
  const mid = readNumber('threshold-mid') || 500000;
  const totalDays = readNumber('total-days');
  const avgToDate = readNumber('avg-to-date');
  const elapsedDays = readNumber('elapsed-days');
  const currentBalance = readNumber('current-balance');
  let targetDays = readNumber('target-days');
  if (!isFinite(targetDays) || targetDays <= 0) targetDays = totalDays;

  const dataSets = [
    { name: 'basic', threshold: basic },
    { name: 'mid', threshold: mid },
  ];

  dataSets.forEach(ds => {
    const hit = isMeetingAtT(avgToDate, elapsedDays, currentBalance, targetDays, ds.threshold);
    const needM = requiredBalanceForT(avgToDate, elapsedDays, targetDays, ds.threshold);
    const needDays = daysNeededWithM(avgToDate, elapsedDays, currentBalance, ds.threshold);

    writeHtml(`kpi-${ds.name}-status`, hit ? `<span class="ok">已达标</span>` : `<span class="not-ok">未达标</span>`);
    const avgT = (avgToDate * elapsedDays + currentBalance * Math.max(0, targetDays - elapsedDays)) / (targetDays || 1);
    writeText(`kpi-${ds.name}-avg`, `T时点日均：${fmtNumber(avgT)}`);
    writeText(`kpi-${ds.name}-needed`, needM <= 0 ? '0（无需增加）' : fmtMoney(needM));
    writeText(`kpi-${ds.name}-days`, needDays === Infinity ? '不可达（需提高M）' : `${fmtNumber(Math.ceil(needDays), 0)} 天`);
  });
}

function setupEvents() {
  document.getElementById('btn-fill-target').addEventListener('click', () => {
    const t = readNumber('total-days');
    if (isFinite(t)) document.getElementById('target-days').value = String(Math.max(0, Math.floor(t)));
  });
  document.getElementById('btn-run-single').addEventListener('click', computeSingle);

  document.getElementById('btn-run-batch').addEventListener('click', handleBatch);
}

function handleBatch() {
  const fileInput = document.getElementById('csv-file');
  const file = fileInput.files && fileInput.files[0];
  if (!file) { alert('请先选择 CSV 文件'); return; }
  const basic = readNumber('threshold-basic') || 10000;
  const mid = readNumber('threshold-mid') || 500000;

  Papa.parse(file, {
    header: true,
    skipEmptyLines: true,
    complete: (res) => {
      const rows = (res.data || []).map(r => ({
        name: r.name || '',
        id: r.id || '',
        avgToDate: parseFloat(r.avg_to_date),
        elapsedDays: parseFloat(r.elapsed_days),
        currentBalance: parseFloat(r.current_balance),
        totalDays: parseFloat(r.total_days),
      }));
      const tbody = document.querySelector('#batch-table tbody');
      tbody.innerHTML = '';
      rows.forEach(r => {
        const T = isFinite(r.totalDays) ? r.totalDays : (readNumber('total-days') || readNumber('target-days'));
        const results = [
          { name: 'basic', threshold: basic },
          { name: 'mid', threshold: mid },
        ].map(ds => ({
          hit: isMeetingAtT(r.avgToDate, r.elapsedDays, r.currentBalance, T, ds.threshold),
          needM: requiredBalanceForT(r.avgToDate, r.elapsedDays, T, ds.threshold),
          needDays: daysNeededWithM(r.avgToDate, r.elapsedDays, r.currentBalance, ds.threshold),
        }));

        const tr = document.createElement('tr');
        tr.innerHTML = `
          <td>${escapeHtml(r.name)}</td>
          <td>${escapeHtml(r.id)}</td>
          <td>${fmtNumber(r.avgToDate)}</td>
          <td>${fmtNumber(r.elapsedDays, 0)}</td>
          <td>${fmtMoney(r.currentBalance)}</td>
          <td>${fmtNumber(T, 0)}</td>
          <td class="${results[0].hit ? 'ok' : 'not-ok'}">${results[0].hit ? '达标' : '未达标'}</td>
          <td class="${results[1].hit ? 'ok' : 'not-ok'}">${results[1].hit ? '达标' : '未达标'}</td>
          <td>${results[0].needM <= 0 ? '0' : fmtMoney(results[0].needM)}</td>
          <td>${results[1].needM <= 0 ? '0' : fmtMoney(results[1].needM)}</td>
          <td>${results[0].needDays === Infinity ? '不可达' : fmtNumber(Math.ceil(results[0].needDays), 0) + ' 天'}</td>
          <td>${results[1].needDays === Infinity ? '不可达' : fmtNumber(Math.ceil(results[1].needDays), 0) + ' 天'}</td>
        `;
        tbody.appendChild(tr);
      });
    }
  });
}

function escapeHtml(s) { return String(s).replace(/[&<>"']/g, c => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c])); }

document.addEventListener('DOMContentLoaded', setupEvents);


