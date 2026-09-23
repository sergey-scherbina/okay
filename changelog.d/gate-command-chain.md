## gate-command-chain - `gate.sh "a; b"` runs b too

`scripts/gate.sh "a; b"` handed sbt the whole string as ONE argument,
and sbt ran `a` and dropped `b` without a word — met twice on
2026-09-23 (three suites in one call, only the first ran; "0 test
results" in the verdict line was the only tell).

- `scripts/gate.sh`: a `;`-chain is split into one sbt argument per
  command — sbt's own spelling for a sequence — in order, trimmed,
  empty parts dropped; sbt still stops at the first that fails. The
  JVM-first split stays reserved for a single `affected <ref>`.
- A chain naming NO command (`" ; ; "`) is refused with exit 2.
  Found on this lane, the hard way: real sbt with zero arguments
  opens its INTERACTIVE shell, and the gate would wait on it forever.
- `scripts/gate-selftest.sh` cases 6 and 7, through a new
  `scripts/fake-sbt-args.sh` that echoes what it was handed: three
  commands arrive separate, ordered and trimmed; a plain command is
  untouched; an empty chain never starts sbt. Watched FAIL first
  (sbt handed `<a; ; b ;c>`), both under bash and `/bin/sh`.
- AGENTS.md's single-path paragraph names the chained form.
