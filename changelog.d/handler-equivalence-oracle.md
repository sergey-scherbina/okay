## handler-equivalence-oracle - `Bisim.check`: are these two programs the same program?

`okay.Bisim` (core) walks two programs in lockstep, demands the same
operation at every step, and feeds both continuations each answer from
a finite sample (`Answers[F]`, combined over a row with `+`). This is
normal-form bisimilarity (Biernacki, Lenglet & Polesiuk, FSCD 2020)
made executable. `Differ` is a proof, with its path. `Same(paths,
cut)` is evidence and reports its counts. There is no cast: each
program's operation is answered from its own `Answers` call, and the
two lists are zipped.

- TestBisim (20): the oracle's controls (self-equality counts, a
  difference on one answer with its path, the sample deciding,
  return vs perform, depth and budget cuts, `get; get` vs `get`
  differing freely and agreeing after `State.handle`), then Gen stage
  LAWS and MODELS on the materialised programs.
- Three mutants of Gen's own stage code, each watched: `dropping` off
  by one is caught by a law, `taking` counting down by two passes the
  law and is caught by the model, and a `taking` mutant that turned out
  equivalent for n >= 1 is caught only by `take(0)`. Details are in
  specs/handler-equivalence-oracle.md, Results.
- docs/equivalence.md: the user page, with its examples pinned in
  TestBisim and the literature.
