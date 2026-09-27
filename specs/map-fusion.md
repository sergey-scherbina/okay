# Map fusion — `p.map(f).flatMap(g)` is one Bind

## Overview

left-nested-build-cost showed that a program's global shape is worth
≤1.1x, yet two programs of the same 1000 operations under the same two
handlers differed 2.27x. BuildShapeBenchmark measured the difference
directly (2026-09-27): each step written `op.map(acc + _)` followed by a
flatMap took 28.6 µs / 306 KB, and each step written as ONE flatMap took
12.6 µs / 138 KB. `map` was `flatMap(a => Return(f(a)))`, so every such
step was `Bind(Bind(op, mapK), g)`: two binds nested left, which
`resume` rotated on every step, plus a `Return` and a `Bind(Return, g)`
to resolve.

## Interface (Free.scala)

```scala
object Free:
  final class Mapped[F[+_], X, A](val f: X => A, val depth: Int) extends (X => Free[F, A])
  inline val MaxFusedMaps = 32
  def bind[F[+_], A, B](m: Free[F, A], g: A => Free[F, B]): Free[F, B]   // Free#flatMap
  def mapped[F[+_], A, B](m: Free[F, A], f: A => B): Free[F, B]         // Free#map
```

`map` leaves a `Mapped` as the continuation. A `flatMap` on top of it
builds `Bind(op, y => g(f(y)))`, and a `map` on top composes the
functions. Either way there is ONE Bind over the operation. Composition
stops at 32 maps: the composed function is a chain of `andThen` applies
on the JVM stack, so it is bounded, and past the bound a new Bind is
nested and rotated as before.

- [x] map + flatMap and map + map build one `Bind(Inject, _)` (watched RED)
- [x] the mapped function runs at run time, and again on every run
- [x] 1 000 000 maps in a row stay stack-safe (the bound)
- [x] a map over a raise still stops; over Choose it runs per branch
- [x] `TestInlineBudget`: `resume` untouched, still under 325 bytes

## Results (history.d `*-map-fusion.tsv`, A/B against master)

| lane | master | fused | time | B/op |
|---|---:|---:|---:|---:|
| FusionBenchmark.nestedSW | 32.1 µs | 26.2 | 1.23x | −17% |
| BuildShapeBenchmark.rowFoldM | 28.7 | 24.6 | 1.17x | −18% |
| BuildShapeBenchmark.stateFoldM | 26.6 | 22.0 | 1.21x | −29% |
| relayPrebuilt / handlePrebuilt / fusedSWr (no maps) | — | — | 1.00 / 1.00 / 1.01 | identical |

The type test every `flatMap` now makes costs nothing measurable on the
map-free lanes.

**What is left.** `rowFoldM` at 24.6 µs is still 2x `rowOneBind`'s
12.6. `map` still ALLOCATES the Bind and the `Mapped` that the next
`flatMap` discards. That is filed as backlog `map-fusion-residual`.

## Decisions

- 2026-09-27: fuse at CONSTRUCTION, not in `resume`. `resume` is 323 of
  325 bytes (`TestInlineBudget`), and a case added there re-decides the
  inlining of every interpreter loop.
- 2026-09-27: the two `@unchecked` type tests live in `bind`/`mapped`
  only. A `Mapped` found as the continuation of a `Bind[F, x, A]` IS a
  `Mapped[F, x, A]` by where it sits (the no-cast rule's one isolated
  claim).
