## mark-glyph-only - `.?` is the one postfix mark; `.!?` is retired

The direct block's mark had three spellings since unwrap-glyph:
`.reflect`, `.!?` and `.?`. `.!?` stayed only because it was already
written in many places. The operator asked for one glyph (2026-09-25),
so it is gone:

- `Direct`'s `!?` on `F[A]` and on `Gen[W]` is removed. `Gen[W]` has its
  own `?`. `Cont.Monadic`'s symbolic reflect is `.?[B]`.
- The macros' mark sets and the "use the explicit marks" messages name
  `.reflect / .? / !prog`.
- TestUnwrapMark pins that `m.!?` no longer compiles (seen red first).
- Every call site is rewritten: tests, benchmarks, scaladoc, and
  docs/direct-style.md, tutorial.md, typepedia.md, capabilities.md and
  the theory chapters. The eight rewritten debt lines are pinned
  (TestDocExamplesTutorialMarks, TestCtxReaderElim), and six more
  tutorial lines left docs/snippet-debt.txt.
- Pinning found a real defect in a doc example: the tutorial's staged
  `State.modify[Int](_ + i).?` statement discards an `Int` (E176 under
  `-Wall`). It is `val _ = …` now.

Spec: specs/unwrap-glyph.md stage 5.
