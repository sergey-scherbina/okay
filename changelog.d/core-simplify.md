## core-simplify — the core review's behavior-free items (2026-10-02)

Commits: 731729ea8.

- Free.scala says what is true: `resume` is THE rotation and
  `resumeRun` is the same rotation stopping at a machine run, kept
  apart so `resume` carries no `Pending` test under the inlining
  threshold (it had been called "the only one on this side"); the
  reflection-without-remorse note is written once, in `resume`'s doc,
  not also in the header; `Free.defer` goes through `Freer.defer`
  instead of repeating its body.
- Delimited.scala: a capture that found its delimiter is written once,
  as the inline `found` (the generative-prompt claim, `k` and the
  stack below re-typed at the shift's indexes, the body run), shared by
  the general `cut` and `nearest`, which had each written it out.
  Not bytecode-identical (`capture` 159 → 157 instructions, `cut`
  158 → 157: `k` is built before the claim now), so measured:
  HandlerBenchmark.contAnswer A/B, arms alternated, 35.25 against
  35.18 us/op, 1.002 — parity (src/jmh/history.d,
  core-simplify-capture).
- Filed, not done: `cont-list-combinators-one-walk` (Cont's four list
  lowerings as one deferred walk — after cont-stack-layer1-c, which
  adds more beside them), `handling-ever-per-machine` (the
  process-wide `Cont0.Handling.ever` flag as the machine's own state),
  `handle-overloads-chain` (the 2- and 3-handler `handle` overloads).

Gate: `affected master staged`, GREEN, 9 675 tests, no warnings.
