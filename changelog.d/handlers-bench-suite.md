## handlers-bench-suite - `CountdownBenchmark`: the first new lane of Kammar-Lindley-Oury's suite

The effect-handlers-bench family (Kammar, Lindley & Oury, "Handlers
in action", ICFP 2013; the descendant suite Koka/Effekt/OCaml 5's own
papers report on) had no lane in `compare/` — two of its shapes were
already covered under other names (`ChoiceBenchmark` ≈ nqueens/
triples' search family, `GeneratorBenchmark` ≈ fibonacci_recursive's
generator family); this lands the suite's own opening lane, countdown.

- `compare/src/jmh/scala/okay/CountdownBenchmark.scala`: N=100000
  handled state operations, tail-resumptive — `stdLoop` (hand
  ceiling), `okayCountdown` (`State.handle`), `kyoCountdown`
  (`kyo.Var`), `catsState` (`cats.data.State`), `zioRef` (ZIO has no
  first-class State effect, so `Ref` — the same idiom
  `IdiomaticApiBenchmark` already uses for it elsewhere).
- Smoke-tested (not the quiet-box gated protocol): all four effect
  runtimes within one order of magnitude, matching the literature's
  description of countdown as every runtime's cheapest lane; the
  hand loop reads at the JIT's noise floor — the correct answer for
  a pure counting loop with no per-iteration effect, not a defect.
- specs/handlers-bench-suite.md names the five remaining shapes
  (handler_sieve, resume_nontail, tree_explore, triples,
  parsing_dollars) and why each did not land this pass, with a
  trigger for each; and why a table against the papers' OWN
  published numbers would violate this repo's own cross-machine lane
  rule if a competitor tried to quote it that way. docs/benchmarks.md
  §2b.
