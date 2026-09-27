## cont-stack-fastpath - statePara's extra 21 KB named (one re-boxed Long per get) and removed

After cont-stack stage C, statePara allocated +20 928 B/op over the
pre-cont-stack base with no stack switch possible. An exact count by class
(a heap histogram `-all` before and after 500 warm operations, with the GC
count checked unmoved) named the gap: every node type matched the base
object for object. `Reentry`'s 2000 objects weighed exactly what the base's
two lambdas did. `java.lang.Long` read 2617 per op against 1745, and
872 × 24 B is the whole gap (872 is the values above `Long.valueOf`'s cache).
JFR `ObjectAllocationOutsideTLAB` under `-XX:-UseTLAB` placed the extra box
in `PState.get`'s `s => k(s)(s)`. Inlined at the call site, that lambda is
specialised to `Long`, so the state is unboxed and boxed again for each of
its two uses. C2 had folded both re-boxes at the base; after cont-stack it
folded only one.

- `PState.get`/`set` now call erased generic bodies (`getAt`, `setAt`), so
  the state passes through as the object it already is.
- statePara, no switch: **28.20 vs master 29.16 µs (0.967x), 300 960 vs
  342 816 B/op**. That is 21 KB below the base's 321 888, because `set`'s
  two boxes are now one.
- statePara, default room: 31.23 vs 32.43 µs (0.963x).
- contAnswer and fib100 do not reach the change and read the same bytes.
- Each figure is the minimum of 2 alternating rounds via `jmh-lane.sh`
  (`-f2 -wi3 -i5 -prof gc`), from history.d `cont-stack-fastpath-r5-erased`.

REFUTED first: making `Reentry`'s rare road a static method, on the guess
that `this` escaped through it. It measured byte-identical on all four lanes
in both rounds and was reverted (history.d `cont-stack-fastpath-r4-static`).

What is left is filed as `cont-stack-statepara-time-residual`: the base's
no-switch time is to be re-read in the same series before a ~1 µs gap is
believed. The reader's knobs moved to `cont-stack-read-bounds-once`, where
they are now actually filed. The spec entry is specs/cont-stack.md stage C,
round 4; the user doc is docs/cont-stack.md, "What it costs when it does not
switch".
