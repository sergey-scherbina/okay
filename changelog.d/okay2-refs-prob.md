## okay2-refs-prob - cells made at run time, and probabilistic programming

Two of okay's main effects in okay2 (specs/okay2.md stage 12), the
operator having lifted the on-demand rule for the main effects:

- `Refs`: `ref`/`read`/`write` over a heap threaded through the
  handler; `Ref[S]` a value class made only by `ref`. No cast in the
  handler, where the Scala 3 core has one: okay2's handlers run at the
  answer type `Any`, so a slot's value goes straight into its
  continuation.
- `Dist`/`Prob`: `dist`/`uniform`/`observe`, `runExact` (multi-shot
  exact inference through `!.handle[Dist, R]`), `.posterior`,
  `sampleOnce`, `runRejection` — Hansei (Kiselyov & Shan, DSL 2009).

14 tests (the Scala 3 suites, plus 100 000 cells and 4096 branches).
Docs: docs/okay2.md section 15.
