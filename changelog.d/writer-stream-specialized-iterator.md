## writer-stream-specialized-iterator - closing the last of foldWriter's dispatch tax

The previous lane (producer-writer-carrier-foldwriter-eager) fixed
`Chunks.foldWriter`'s worst problem — a boxed `Long` per element — by
routing it through `writerStreamIn`'s `.iterator` instead of
`Writer.foldWith`'s fused trampoline. It left a smaller, honestly
documented residual: `.iterator`'s DEFAULT implementation
(`Iterator.unfold(s)(uncons(_).runWith)`) still allocates an
`Option`+`Either` wrapper and a `Free` node once per CHUNK (157 times,
not 10000), because every step builds and runs a tiny program instead
of just answering a value. It noted this wasn't writer-specific —
`Producer`'s own G-effectful `Stream` instance has no specialized
iterator either — and called closing it "its own lane."

That lane turned out to be small. `writerStreamIn` now has a
hand-specialized, mutable-state `iterator` override, mirroring
`Stream[Producer, Pure]`'s own override in Generate.scala byte for
byte in shape (`var cur`/`ready`/`ended`/`elem`, `@tailrec advance()`).
The one real difference: a forwarded `G`-operation is answered by
`Handler[G].handle(g)` directly — the comonadic, single-operation
interpretation every `Handler` already provides — instead of building
`Inject(g)` and running it as a program.

**Correctness:** a new test builds a writer program with REAL
interleaved `async` calls between tells (the benchmark's own data
never exercises that branch) and checks the specialized iterator
against `Writer.run` — built on the unrelated `foldWith` trampoline —
as an independent oracle.

**Measured, 3 rounds, JDK 21.0.12, N=10000/64:** 6.29 -> ~5.43 us/op,
32,952 -> 12,688 B/op — now matching `foldLeftWriter`'s own direct-call
baseline (12,656 B/op) almost exactly. `-prof jfr`: `Right`/`Some`
samples gone entirely; what remains is tree-construction cost shared
equally with the direct-call baseline.

**Total, from the original dispatched `foldWriter`:** 18.14 -> 5.43
us/op (3.3x faster), 249,584 -> 12,688 B/op (19.7x less garbage). The
remaining ~2x against `Chunks.fold`'s 2.55us is the same gap
`foldLeftWriter`'s own direct call already has and was accepted as "at
parity" for — the cost of walking a `Free`-tree program at all, not
this combinator's dispatch tax, which is now closed.

Gate: `okayJVM/test` 746/746 and `okayStreamJVM/test` 347/347 green, 0
warnings; JS/Native compile clean.
