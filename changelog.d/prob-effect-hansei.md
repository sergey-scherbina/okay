## prob-effect-hansei - probabilistic programming as an effect, the Hansei design

Kiselyov & Shan, "Embedded probabilistic programming" (DSL 2009): a
probabilistic model is an ordinary program with one extra operation
(`Dist`, a weighted choice), and inference is a HANDLER over it — the
same "one program, several readings" this library already gives
`Choose`. The showcase reason it lands here: exact inference explores
every branch by MULTI-SHOT capture, which no one-shot effect runtime
(kyo, cats-effect, ZIO — measured elsewhere in docs/benchmarks.md) can
host at all.

- `Dist[+A]` (src/main/scala/Prob.scala), its own signature, the same
  shape `Choose` uses. `Prob.dist`/`Prob.uniform`/`Prob.observe`,
  `Prob.runExact` (multi-shot, over `Effects.handle`), `.posterior`
  (normalize), `Prob.sampleOnce`/`Prob.runRejection` (single-shot, a
  bespoke resume-loop like `State.handle`/`Once.run` — a program that
  may fail has no `Return` case to answer).
- Everything but `Dist` lives under `object Prob`, not package level:
  `Logic.observe(n: Int)` already names an unrelated operation, and a
  bare `observe(cond: Boolean)` would be silently SHADOWED by
  `import Logic.*` (an explicit import outranks a same-package
  member) — namespacing sidesteps the collision outright.
- TestProb (10 tests): the textbook wet-grass Bayes net's exact
  rational posterior (15/29, 14/29) and a small two-state HMM checked
  against an independent, effect-free hand enumerator, bit for bit;
  rejection sampling agreeing with both within statistical tolerance;
  pruning, forwarding, and THE NUMBER — exact inference's per-branch
  price against the hand enumerator (~5x on a 4-branch HMM, session
  measurement, not a JMH lane).
- docs/guide.md: the Hansei paragraph with the wet-grass example,
  pinned verbatim. specs/prob-effect-hansei.md carries the rest,
  including two things explicitly left out (a JMH lane; importance
  sampling, which coincides with rejection while `observe` is
  boolean-only) and why.
