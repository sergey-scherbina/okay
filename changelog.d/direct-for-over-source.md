## direct-for-over-source - `for x <- src do body` over a source whose step is a program

The last open item of the zipper/iteratee arc (operator: "сделай",
2026-09-23; specs/direct-loops.md v3). v1/v2 loops read things with an
iterator; a `Stream` carrier, a writer program under another effect
and the `Take` side of a `Stage` have none — their next element is a
PROGRAM, and the consumer loop was `!.loop` over `uncons`/`await` by
hand, as theory ch. 7 said until tonight.

- core `Pull[A, G]`: a source as a value — `step: Option[(A, Pull)] !
  G`, `withFilter` (what a guard desugars to), `loop(f): Unit ! G`
  (the loop as a program, by name). `Pull.of(stream)` from any
  `Stream[S, G]`, `Pull.told`/`toldIn` from writer programs
  (first-order, as Stream.scala's own overloads). okay-stream:
  `Take.each[I]: Pull[I, Take % I]`.
- okay-direct: `Direct.foreach` extension on `Pull`, present only
  where a block's ambient `DirectCtx` is, typed Unit; the macro
  (`DirectLoops.pullLoop`) emits `loop(p) = bind(p.step) {
  Some((h, tl)) => body; loop(tl) | None => pure(()) }` through the
  row lift a mark takes, the body compiled against the recursive call
  as its tail, guards honoured — and the road fires on the RECEIVER'S
  TYPE, marks in the body or not.
- Found on the way, recorded in the spec's Results: a program-typed
  `foreach` member is refused by the discarded-program lint at typer,
  before the macro — which is why the `for` over a source exists only
  inside blocks; and a pure source's `G` is `Nothing`, which no
  higher-kinded type pattern matches.
- `TestDirectSource` (7): the program form, the block form with and
  without marks, a guard, producer/body interleaving through a State
  cell, `told`, and the two refusals (a source outside the row, `yield`
  over a source, `for` outside a block). `TestTakeEach` (2).
- docs: direct-style.md "A loop over a source", theory ch. 7's closing
  paragraph (no longer "spelled `!.loop` today"), guide §5.

Gate `affected master` green, no warnings.
