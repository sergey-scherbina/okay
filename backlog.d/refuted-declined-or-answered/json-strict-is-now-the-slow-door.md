- json-strict-is-now-the-slow-door — CLOSED WONTFIX 2026-09-19: the
  entry's own DISQUALIFYING condition is already met in
  docs/benchmarks.md §10 — the strict walk's cost is the field map,
  the erased parts and `make`, ~3.3x the bare parse, none of it a
  fixable inefficiency. `Staged.strict[A]` is the fast door already;
  `readStrict` stays for its REFUSAL, not its speed.
