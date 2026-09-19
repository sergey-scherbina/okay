## producer-writer-carrier-foldwriter-eager - the foldWriter fix, actually landed

The previous lane (producer-writer-carrier-foldwriter-fix) root-caused
`Chunks.foldWriter`'s 6.8x slowdown against `Chunks.fold`: `Fold.OfLong.
addLong` boxes its element argument by design, and the JIT's escape
analysis eliminates that box inside `Chunks.foldLeft`'s small compiled
loop but not inside `Writer.foldWith`'s bigger resume/split/Bind
trampoline. It named the fix — decouple the tree walk from the
per-chunk consumption — but called it blocked by an API-contract
question and stopped there.

The question turned out to be answerable: `foldWriter` had ZERO
production call sites (`Bulk.scala`, `Pipeline.scala`, and
`Acceptance.scala` all still call `Chunks.fold` on `Producer` directly),
so narrowing its signature broke no caller. Rebuilt it to walk via
`writerStreamIn`'s existing default `.iterator` (no override needed —
small, per-step `uncons` calls) instead of the fused trampoline, and
wrapped the resulting eager walk in `async { ... }` (an existing
primitive) so the returned value stays a suspended, composable program.
The signature narrowed from an arbitrary `G[+_]` to `Async`+`CanBlock`
— the same capability every other blocking door in this library
already requires.

**Measured, 3 rounds, JDK 21.0.12 pinned, N=10000/64:** 18.14 -> ~6.29
us/op median (2.9x faster), 249,584 -> 32,952 B/op (7.6x less garbage).
`-prof jfr`: `java.lang.Long` allocation samples down from 867 to 16 —
the element-boxing problem is gone.

**Not full parity with `Chunks.fold`'s 2.55us, and the gap is
understood, not hand-waved:** the remaining allocation is
`Iterator.unfold`'s generic `Option`+`Either`+`Free`-node wrapping, once
per CHUNK (157 times, not 10000). This is not writer-specific —
`Producer`'s own G-effectful `Stream` instance has no specialized
`iterator` override either (only its PURE instance does, in
Generate.scala), so any G-effectful stream in this library pays this
tax today. Closing it needs a hand-specialized, mutable-state iterator
override mirroring the pure one, benefiting both carriers — a genuinely
separate, future lane.

**Consequence:** `Bulk.scala`, `Pipeline.scala`, and `Acceptance.scala`
can migrate onto `foldWriter` now (their `G` is already `Async`-shaped).
Not done here — each sits inside its own `Chunks[A]`-typed surrounding
context, the same caution that made `okay-cluster`'s `Shape[A]` a
bigger job than a fold swap.

Gate: `okayStreamJVM/test` 346/346 green, 0 warnings; JS/Native compile
clean.
