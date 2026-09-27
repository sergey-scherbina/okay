# Left-nested build cost — measured, and mostly not a cost

## Overview

handler-single-pass's re-measure (2026-09-27, specs/handler-fusion.md)
read `-prof stack` as "60-70% of a foldLeft-built program's time is
`Free.resume`'s rotation", and filed this lane on that reading. The
reading was WRONG. The sampled frame `Free.resume$$anonfun$1` is the
rotation's lambda `f(_).flatMap(g)`, and INSIDE it runs `f`, which is the
user's continuation together with the handler work it reaches. The frame
measures everything a step does, not the rotation alone.

This lane built the tool the reading asked for, then measured the
question directly.

## Interface

```scala
object Effects:   // `!`
  def foldM[X, B, F[+_]](xs: Iterable[X])(z: B)(f: (B, X) => B ! F): B ! F
  def each[X, F[+_]](xs: Iterable[X])(f: X => Unit ! F): Unit ! F
```

Both build RIGHT-nested: each step's continuation builds the next, the
head is always one operation, and nothing is rotated. The input is
indexed, so the program is a value that runs again, and it is stack-safe
(1 000 000 elements). A `!.traverse` was dropped. Under `import !.*` it
shadowed the top-level Applicative `traverse` and broke a test's
compile, and it was only `foldM` collecting into a Vector.

- [x] effects in order; the program runs twice with the same answer
- [x] the accumulator threads left to right; the empty input answers `z`
- [x] the built program's head is `Bind(Inject(_), _)` (right-nested)
- [x] stack-safe over 1 000 000 elements

## Results (BuildShapeBenchmark, 2026-09-27; history.d `*-build-shape.tsv`)

The same N = 1000 operations, BUILT AND RUN inside the measured method
(every real caller builds per call), per-lane minima, each lane its own
`jmh-lane.sh` run:

| program | foldLeft | foldM / each | time | B/op |
|---|---:|---:|---:|---:|
| Writer telling 1000 values | 17.1 µs | 16.9 | 1.01x | 206 KB → 150 KB (−27%) |
| State counter with an accumulator | 27.1 | 26.6 | 1.02x | 336 → 280 (−17%) |
| State + Writer row, two handlers | 31.1 | 28.8 | 1.08x (±2.5 µs noise) | 370 → 306 (−17%) |

**The shape is worth memory, not time.** The rotation costs 56 B per
element, a closure and a Bind, and HotSpot pays for them almost for free.
The 22 `foldLeft(pure)(flatMap)` builders in main code (surveyed
2026-09-27) are therefore NOT converted by this lane: at most ~1.1x, and
only in multi-handler rows. Converting them for the memory remains
possible and is filed with the numbers.

**What the 32-vs-13 µs gap of FusionBenchmark's nestedSW/nestedSWr is
then.** The shape is not it: `rowFoldM` above is right-nested and still
reads 28.8 µs. The difference between `rowFoldM` and `rightSW` is that
every step here is `op.map(acc + _)` followed by a flatMap, TWO binds
nested left per operation and rotated on every step, where `rightSW`
does one `flatMap` per operation. That is a HYPOTHESIS, and backlog
`map-flatmap-pair-cost` is the lane that measures it.

## Decisions

- 2026-09-27: `foldM`/`each` land as API. Their right-nested build is the
  honest default, it saves 17-27% of the allocation, and they read better
  than a `foldLeft` over `flatMap`.
- 2026-09-27: the 22 builders are not converted for speed. The
  measurement says ≤1.1x.
