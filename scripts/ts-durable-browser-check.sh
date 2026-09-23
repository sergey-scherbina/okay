#!/bin/sh
# typescript-types T10, the LIVE check of durable flows in a REAL browser:
# a page runs a two-step checkout (`durable`, journalled in IndexedDB)
# in headless Chrome. The first load dies at the second step, as a
# reload would interrupt it. The second load, same profile directory,
# (driven through the DevTools protocol, scripts/chrome-read.mjs), must finish the flow with each callback having run EXACTLY ONCE
# across both loads: the first step answered from IndexedDB, not called
# again. Prints `ts-durable-browser: GREEN` or `: RED` with the reason.
set -eu
here=$(cd "$(dirname "$0")/.." && pwd)
chrome="${CHROME:-/Applications/Google Chrome.app/Contents/MacOS/Google Chrome}"
work=$(mktemp -d "${TMPDIR:-/tmp}/okay-ts-durable.XXXXXX")
red() { echo "ts-durable-browser: RED — $1"; exit 1; }
[ -x "$chrome" ] || red "no Chrome at $chrome (set CHROME)"
command -v node >/dev/null 2>&1 || red "node drives Chrome, and it is not installed"

sh "$here/scripts/gate.sh" "okayTsNpmJS/npmPackage" > "$work/gate.log" 2>&1 || true
grep -q '^gate: GREEN' "$work/gate.log" || { grep '^gate:' "$work/gate.log"; red "the package did not build (log: $work/gate.log)"; }

site="$work/site"
mkdir "$site"
# ONE FILE, opened as file://: the page's module script imports the
# package from a data: URL. No server — a loopback listener is not
# reachable from every sandbox this runs in — and no file fetch, which
# Chrome refuses a module from a file:// page. (Inlining main.js does not
# work: Scala.js exports under internal names, `export { $e_x as x }`,
# so the exported names are not bindings of the script that holds them.)
# no margin: the docs quote these lines
cat > "$site/page.js" <<'JS'
const crash = new URLSearchParams(location.search).has("crash");
const calls = JSON.parse(localStorage.getItem("calls") ?? "{}");
const count = (n) => { calls[n] = (calls[n] ?? 0) + 1; localStorage.setItem("calls", JSON.stringify(calls)); };
const out = document.getElementById("out");
const checkout = then(performing("reserve", "tea"), (r) =>
  then(performing("charge", r), (c) => done({ reserved: r, charged: c })));
try {
  const receipt = await durable("checkout", checkout, {
    reserve: async (sku) => { count("reserve"); return "R-" + sku; },
    charge: async (r) => { if (crash) throw new Error("the page was reloaded"); count("charge"); return "C-" + r; },
  }, indexedDbJournal("okay-check"));
  out.textContent = "done " + JSON.stringify({ receipt, calls });
} catch (e) {
  out.textContent = "died " + JSON.stringify({ why: e.message, calls });
}
JS
{
  printf '<!doctype html>\n<html><body><pre id="out">waiting</pre>\n<script type="module">\n'
  printf 'import { durable, indexedDbJournal, then, performing, done } from "data:text/javascript;base64,'
  base64 < "$here/okay-ts-npm/.js/target/npm/main.js" | tr -d '\n'
  printf '";\n'
  cat "$site/page.js"
  printf '</script></body></html>\n'
} > "$site/index.html"

profile="$work/profile"
load() {
  node "$here/scripts/chrome-read.mjs" "$chrome" "$profile" "$1" "#out" "waiting" 60000 2>>"$work/chrome.err"
}

first=$(load "file://$site/index.html?crash=1")
echo "first load:  $first"
case "$first" in
  died*'"reserve":1'*) ;;
  *) red "the first load should die at the charge, having reserved once; it said: $first" ;;
esac

second=$(load "file://$site/index.html")
echo "second load: $second"
want='done {"receipt":{"reserved":"R-tea","charged":"C-R-tea"},"calls":{"reserve":1,"charge":1}}'
[ "$second" = "$want" ] || red "the second load should finish with each callback run once; expected $want"

echo "ts-durable-browser: GREEN — Chrome $("$chrome" --version | awk '{print $3}'), IndexedDB, resumed after a reload with each step run once"
