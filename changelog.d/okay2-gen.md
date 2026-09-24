## okay2-gen - generators for the Scala 2 core

`Gen[W]` for okay2 (specs/okay2.md stage 9): a generator is a
`Writer[W]` program with `Stop` beside it; element-wise combinators as
members so a for-comprehension is a generator; stopping readers
through `FoldUntil`; `iterator` with Python's `next()` semantics;
`toLazyList`. The Scala 3 core's chain fusion is ported whole — `Chain`
(`Plain`/`Staged`/`Cat`) and the `Xf` stages, each read fused (a
`FoldUntil` transformer) or materialised (the walk) — and `zip` pulls
through a fused `flatMap` (strymonas's hard case). 22 tests; 325
in the okay2 gate.

- The walks match `Writer.said` instead of splitting with a closure,
  so the reader is a real `@tailrec` loop with no `@unchecked` pattern.
- No widening: `Writer[W]` and `Stop` programs are `Row[W]` programs by
  contravariance.

Docs: docs/okay2.md section 14; specs/okay2.md stage 9.
