## fold-until - a fold that can stop: `FoldUntil` and one consumer per carrier

`Fold[A, S]` walks to the end by construction, and its own `exists`
said so while citing a `Chunks.exists` that did not exist. The shape
the operator brought — a state that decides when the iteration ends,
`S => Either[S, R]`, `tailRecM` at `Id` — is now the consuming side's
stopping fold (specs/fold-until.md, stage 1): `FoldUntil[A, S, R]` in
core Fold.scala, `init`/`add`/`done`/`end`, a halting Moore machine
with the operator's `Either` step as the `FoldUntil.until` adapter
over it rather than the primitive, so no consumer pays a `Left` per
element. Instances `find`, `headOption`, `exists`, `forall`, `take`;
consumers `Stream.foldUntil` (any Stream, by its iterator),
`Chunks.foldUntil` (`done` per element and BEFORE the next pull),
`Writer.foldUntil` (the `loopWith` walk with an early `Pure`, the
producer never resumed past the satisfying tell, answering `R` alone)
and `Source.runFoldUntil`. `done(init)` is honoured, so `take(0)`
pulls nothing anywhere.

Laws count rather than read: `take(3)` over an on-demand LazyList
evaluates 3, over an infinite chunked generator pulls 1 chunk, over a
source with an Async op after every tell performs 2; 100 000 tells
with the stop never firing fold on the default stack. `TestFoldUntil`
(core, 5) and `TestFoldUntilStreams` (okay-stream, 4). Stages 2–4 —
`loop` on `!`, a stopping `transduce`, the `Foldable` side — stay in
the spec behind their triggers. Landed as f4c7856f.
