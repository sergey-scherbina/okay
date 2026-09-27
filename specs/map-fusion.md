# Map fusion — measured, and REFUTED in its safe form

## Overview

left-nested-build-cost showed that a program's global shape is worth
≤1.1x, yet two programs of the same 1000 operations under the same two
handlers differed 2.27x. BuildShapeBenchmark measured the difference
directly (2026-09-27), with two rounds agreeing within 1%. With each step
written `op.map(acc + _)` then a flatMap (`rowFoldM`), the program took
28.6 µs / 306 KB. With each step ONE flatMap (`rowOneBind`, the new
lane), it took 12.6 µs / 138 KB. `map` is `flatMap(a => Return(f(a)))`,
so such a step is `Bind(Bind(op, mapK), g)`: two binds nested left,
rotated on every step, plus a `Return` and a `Bind(Return, g)` to
resolve. **The gap is real, and it is the step's two binds.**

## What was tried (Free.scala, not landed)

`map` left a recognisable `Mapped(f)` continuation, and a `flatMap` on
top of it built one Bind over the operation. Two forms of that Bind's
continuation were tried:

1. **Direct: `y => g(f(y))`.** A/B against master: nestedSW 1.23x,
   rowFoldM 1.17x, stateFoldM 1.21x, bytes −17-29%, and the map-free
   lanes (relay/handle/fusedSWr) unchanged. **Not stack-safe.** The
   next continuation is CALLED rather than returned to the interpreter,
   and continuations composed by continuations (Delim's
   `k1(x).flatMap(k2)`, n segments) chain n direct calls.
   TestStackSafetyCore's Delim test overflowed at n = 20 000. The
   operator's rule (no unbounded stack recursion) refuses it, and no
   bound is available: the chain runs through user and library lambdas
   the builder cannot see.
2. **Trampolined: `y => Bind(Return(f(y)), g)`**, the stack-safe form.
   A/B: stateFoldM 1.15x, rowFoldM 1.02x, **nestedSW 1.21x SLOWER**
   (31.7 → 38.3 µs). Refuted.

Both runs are in history.d (`*-map-fusion.tsv`). Free.scala is unchanged
on master.

## What stays

- `BuildShapeBenchmark.rowOneBind`, the control that shows the gap.
- The finding, for whoever writes a hot step. Write it as one `flatMap`
  (`op.flatMap(x => next(acc + x))`) rather than `op.map(f).flatMap(g)`
  when the loop is hot. That is a 2x on the step, and it needs no
  library change.

## Decisions

- 2026-09-27: not landed. The fast form is stack-unsafe with composed
  continuations, and the safe form regresses the left-nested row by
  21%.

## The continuation queue, prototyped and dropped (bind-continuation-queue, 2026-09-27)

ORDER 3 of the map-cost plan, built as the item asked: `resume`'s
rotation builds a queue node `ThenK(l, r)` (the type-aligned sequence of
van der Ploeg and Kiselyov, "Reflection without remorse", Haskell 2014,
as a binary tree) instead of the closure `l(_).flatMap(r)`, and applying
it runs a `Mapped` head IN PLACE — the map's value goes straight on,
no `Return`, no `Bind(Return, r)` — while a left-nested node is
re-associated by the same loop's tail call. Handlers unchanged; `resume`
still inside `FreqInlineSize`; the core suite green. THE FIRST CUT
OVERFLOWED on `TestStackSafetyCore`'s Delim shift under 20 000 pending
`.map`s: applying a `Mapped` then CALLED the next node, itself a queue
node with a `Mapped` head, n deep — the very chain that refuted map
fusion above. Continuing that case by the loop (`run(t.l, t.r, f(x))`)
fixed it.

Measured against master, 3 forks x 2 rounds, `-prof gc`
(`src/jmh/history.d/…-bind-continuation-queue.tsv`): the map-heavy
lanes did NOT move — `rowFoldM` 23.4 vs 23.4-23.8 µs, `stateFoldM` 19.3-19.5
vs 19.5, bytes identical — because one-bind-hot-steps and
op-map-constructors had already taken the shape out of the library's own
builders that morning; only `nestedSW`, where USER code nests map and
bind, moved (0.83-0.85x, -13% bytes); and the map-free lanes paid for the
queue node's class tests (`handlePrebuilt` 1.01-1.03x, `relayPrebuilt`
1.02-1.03x). The item's bar — map-heavy lanes toward `rowOneBind`,
map-free unchanged — failed on both counts: DROPPED. What remains open
is only user code's own `op.map(f).flatMap(k)`, which `direct` already
writes as one bind (direct-one-bind-steps) and a hand-written chain can
write as one `flatMap`.


## Where the other four fifths are (map-cost-residual, 2026-09-27)

The Overview's "the gap is real, and it is the step's two binds" was
only a fifth right. With the map fused in the DIRECT form — `foldM`'s
own step, one-bind-hot-steps — `rowFoldM` still read 23.4 against
`rowOneBind`'s 11.3 (history `…-bind-continuation-queue.tsv`, the master
arm), and `nestedSW` under the refuted direct fusion read 26.2 against
`nestedSWr`'s 13.7. Both refuted roads above attacked the fifth. This
lane named the rest with a LADDER of control lanes in
BuildShapeBenchmark, each rung adding one thing `foldM` does that the
hand-written `oneBind` loop does not, every lane its own `jmh-lane.sh`
run on a quiet box (`-f 2 -wi 3 -w 1 -i 5 -r 1 -prof gc`; history.d
`…-map-cost-residual.tsv`):

| rung | lane | µs | B/op | adds |
|---|---|---:|---:|---|
| 0 | rowOneBind | 11.48 | 127 416 | — (one flatMap a step, `acc: Int`) |
| 1 | rowUnwrap | 17.76 | 175 096 | the step is `op.map(acc + _)`, unwrapped as `step` does: **+6.3 µs, +48 B a step** — the `Bind` + `Mapped` (+ the lambda) that `.map` builds and the builder throws away |
| 2 | rowFoldM | 23.67 | 231 144 | the same through `!.foldM`: +5.9 µs, +56 B — `go`'s second closure, the boxed accumulator, ~16 B unnamed |
| — | rowOneBindAcc | 13.44 | 151 424 | `case class Acc(n: Int)` accumulator: ONE small object a step is **+2.0 µs** here, the ladder's calibration |
| — | rowFoldMAcc | 23.05 | 247 488 | the pair's ratio 1.72x against 2.06x: boxing of the erased `B` is ~2 µs of the residual |

**The residual 12.2 µs over the one-flatMap loop is allocation, about
2 µs an object: 6.3 in the map node the builder discards, ~2 in the
boxed accumulator, ~4 in the builder's second closure and generic
call.** None of it is a rotation and none of it is `Bind(Return, g)`:
`rowUnwrap` has neither and carries half the gap on its own. The map
node is the price of the syntax — `op.map(f)` must build something for
`foldM` to read the function out of — and the only cheaper node is a
`Map(a, f)` case of `Free` itself (one object instead of `Bind` +
`Mapped`, −16 B of the 48), which is a core enum change against
`resume`'s 325-byte budget (`TestInlineBudget`) and is NOT taken here.
The boxing is Scala 3's (no `@specialized`). The second closure was the
builder's own and is removed below.

Also read, not measured: form 2 above ("nestedSW 1.21x SLOWER") did
strictly less work than master and saved only 16 B a step where the
rotation it skipped weighs ~48. Its diff (`8f40e0bdf`) made
`inline def flatMap` a call to `Free.bind` with two type tests, on
EVERY flatMap in the library — the rotation's own `f(_).flatMap(g)`,
every handler's forwarding bind — while the fusion fired only inside the
rotation closure. The 21% priced those type tests, not the form. A
clean retest puts the same continuation in `resume`'s rotation case,
where the `Bind(Bind(a, f), g)` match already runs, and leaves
`flatMap` a constructor.
