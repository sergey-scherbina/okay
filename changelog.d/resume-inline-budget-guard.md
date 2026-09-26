## resume-inline-budget-guard - a test that fails when a hot interpreter method crosses FreqInlineSize

- `TestInlineBudget` (src/test/scala) reads the bytecode length of
  `Free.resume` (323), `Effects.relay`'s loop (266) and
  `Effects[Free].handle`'s loop (318) from the compiled classes. It fails
  when one exceeds HotSpot's `FreqInlineSize` of 325, and the failure
  names the lanes to re-measure. Before this, a crossing was noticed only
  if someone happened to run a benchmark. Three measured crossings moved
  numbers by 6-15%.
- The class-file reader is our own (JVMS §4), because dotty 3.9 cannot
  load `java.lang.classfile`'s sealed model types. Watched red on a
  mutated `resume` (465 bytes), then reverted. Spec specs/core-gaps.md
  stage 4; off the sprint.
