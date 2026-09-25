## stack-safety-guard - the stack-recursion inventory only shrinks, and the gate says so

Stack-safety stage 9 (specs/stack-safety.md). `scripts/recscan-check.sh`
is the `docs/snippet-debt.txt` discipline for "NO UNBOUNDED STACK
RECURSION", and `gate.sh` runs it after every GREEN as `--since master`.
It refuses three things, each keyed by file and def:

- NEW: a recursion recscan finds that no inventory row names;
- BARE: a row the diff added with no reason in its sixth column;
- PAID: a row whose recursion or file is gone. `--write` deletes exactly
  those and never adds a line.

It scans only the modules whose main sources the diff touched, about 3 s
each, and skips, by name, a module whose classes are older than its
sources. It runs on warm gates too, because deleting a source compiles
nothing. `scripts/recscan.py` gained `RECSCAN_ONLY` and now reads only the
newest `scala-*` classes of a target: the old 3.7.4 classes beside 3.9.0
reported rows for code that is gone.

Tested by mutants through the real gate. A recursive def in the core was
RED (NEW), its reasonless row was RED (BARE), a reasoned row held, and
deleting the def made the row RED (PAID, file gone) until `--write`
removed it. A first cut of `--all` read an uncompiled okay2 checkout as
"56 rows paid", and `--write` would have emptied the inventory. The
freshness check covers `--all` now.

The first run named 30 recursions that had landed after the inventory:
16 in okay (okay-arrow, okay-py `ArrowFrames.cells`, `ContMacro.rewrite`)
and 14 in okay2 (okay2-sql `Query`/`Typed.fits`, -jdbc, -spark, the
fs2/zio interop `again`s). They are UNAUDITED rows with backlog items
(okay-core/stack-safety-catch-up, okay2
modules/stack-safety-catch-up-okay2). The suspect worth taking first is
`OkayArrow.parseField`, which recurses per level of a schema read from a
file. Six rows that recscan now sees as deferred (a LazyList tail, a
`new Free.Bind`) are deleted. `gate-selftest.sh` sets `GATE_RECSCAN=0`,
because its fake builds have no classes.
