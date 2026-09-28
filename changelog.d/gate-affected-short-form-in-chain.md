## gate-affected-short-form-in-chain - `affected <ref> [staged]` is expanded inside a `;` chain too

- `scripts/gate.sh "affected master staged; okayDeploy/testOnly X"` handed
  sbt `affected master staged` raw, and sbt refused it ("Not a valid key:
  staged"): the JVM-first expansion matched the WHOLE argument, and a
  chain is never the whole argument (foreign-one-r, 2026-09-26). Loud,
  so it cost a run, not a verdict.
- The chain is split FIRST, then each part is expanded on its own — the
  two staged phases in place, the plain command after them, sbt still
  stopping at the first that fails. A single command is a chain of one,
  so the three code paths the script had (plain, two-phase, chain) are
  one. `scripts/gate-selftest.sh` case 6b pins the shape with the fake
  sbt; cases 6, 7 and 8 (the chain, the empty chain, the four-argument
  pass-through) are unchanged and still pass.
