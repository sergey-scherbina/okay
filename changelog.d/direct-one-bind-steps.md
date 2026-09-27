## direct-one-bind-steps - answered: the direct macro already writes one bind a step

- Measured, not read. TestDirectShape runs `direct` blocks through a
  counting interpreter. Straight-line code, a branch and dropped values
  have 0 left-nested heads in 12 steps. The emitter already writes one
  flatMap a step, and loop bodies already fuse their tail.
- The one exception is a loop followed by the rest of its block. The
  whole loop is bound before the rest, so each iteration's head is
  rotated. It is pinned by the test and filed as `direct-loop-then-rest`,
  LOW, because the shape is worth ≤1.1x (left-nested-build-cost).
- No macro change. The sprint item is closed.
