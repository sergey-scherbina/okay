#!/bin/sh
# typescript-types T9, the LIVE check of okay's npm package, the way a
# TypeScript project meets it: build the package through the gate, `npm
# pack` it, install the tarball OFFLINE into a fresh project, compile a
# consumer with `tsc --strict` (and a wrong one, which must be refused),
# and run the consumer with Node. Prints `ts-npm: GREEN` or `ts-npm: RED`
# with the reason; exits 0 only on GREEN.
set -eu
here=$(cd "$(dirname "$0")/.." && pwd)
work=$(mktemp -d "${TMPDIR:-/tmp}/okay-ts-npm.XXXXXX")
red() { echo "ts-npm: RED — $1"; exit 1; }

for tool in node npm tsc; do
  command -v "$tool" >/dev/null 2>&1 || red "$tool is not installed"
done

sh "$here/scripts/gate.sh" "okayTsNpmJS/npmPackage" > "$work/gate.log" 2>&1 || true
grep -q '^gate: GREEN' "$work/gate.log" || { grep '^gate:' "$work/gate.log"; red "the package did not build (log: $work/gate.log)"; }

pkg="$here/okay-ts-npm/.js/target/npm"
(cd "$pkg" && npm pack --silent --pack-destination "$work" > "$work/pack.txt") || red "npm pack failed"
tarball="$work/$(tail -1 "$work/pack.txt")"

app="$work/app"
mkdir "$app"
printf '{ "name": "consumer", "private": true, "type": "module" }\n' > "$app/package.json"
(cd "$app" && npm install --offline --no-audit --no-fund --silent "$tarball") || red "npm install of the tarball failed"

# no margin: the docs quote these lines
cat > "$app/consumer.ts" <<'TS'
import { gcounter, orset, run, then, performing, done, channel, type GCounter } from "@okay/ts";

const a: GCounter = gcounter.inc(gcounter.empty(), "phone", 2);
const b: GCounter = gcounter.inc(gcounter.empty(), "laptop");
const total: number = gcounter.value(gcounter.merge(a, b));

const cart = orset.merge(orset.add(orset.empty(), "tea"), orset.add(orset.empty(), "milk"));

const quote = await run(
  then(performing<number>("price", "tea"), (price) =>
    then(performing<number>("stock", "tea"), (stock) => done({ price, stock }))),
  { price: (sku: string) => (sku === "tea" ? 4.5 : 0), stock: async () => 12 },
);

const updates = channel<string>();
updates.offer("packed");
updates.offer("shipped");
updates.close();
const seen: string[] = [];
for await (const u of updates) seen.push(u);

console.log(JSON.stringify({ total, cart: orset.values(cart), quote, seen }));
TS

cat > "$app/wrong.ts" <<'TS'
import { gcounter } from "@okay/ts";
const n: string = gcounter.value(gcounter.empty());
console.log(n);
TS

(cd "$app" && tsc --noEmit --strict --target es2022 --module nodenext consumer.ts > "$work/tsc.txt" 2>&1) \
  || { cat "$work/tsc.txt"; red "tsc --strict refused the consumer"; }
if (cd "$app" && tsc --noEmit --strict --target es2022 --module nodenext wrong.ts > "$work/wrong.txt" 2>&1); then
  red "tsc accepted a number where the package declares it is not a string"
fi
grep -q "not assignable to type 'string'" "$work/wrong.txt" || { cat "$work/wrong.txt"; red "tsc refused wrong.ts for another reason"; }

out=$(cd "$app" && node consumer.ts 2>&1) || { echo "$out"; red "node failed to run the consumer"; }
want='{"total":3,"cart":["milk","tea"],"quote":{"price":4.5,"stock":12},"seen":["packed","shipped"]}'
[ "$out" = "$want" ] || red "the consumer printed $out, expected $want"

echo "ts-npm: GREEN — packed, installed offline, typed by tsc --strict, run by Node ($tarball)"
