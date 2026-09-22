## fold-until-stages-3-4 - `Stage.transduceUntil`, `Take.foldUntil`, and `foldUntil` on `Foldable`

The last two stages of specs/fold-until.md, built the day the operator
removed their triggers. `Stage.transduceUntil(z)(step: (S, I) =>
Stage[I, O, Either[S, R]], end: S => R): Stage[I, O, R]` is
`transduce` whose step may END the stage: on `Right` the stage answers
and `through` pulls nothing more from upstream, on the input's end
`end` sees the last `Left` state — a header parser reads `k: v` lines,
stops at the blank one, and the body after it is never pulled (3 of 6
lines, counted; the control that never stops counts 6). `transduce` is
the same with a step that never answers `Right`, asserted on a shared
step. `Take.foldUntil(using fo): R ! Take % W` is a `FoldUntil` as a
consumer PROGRAM — the iteratee theory ch. 7 says it is, written over
`!.loop` — and `pipe(producer)(Take.foldUntil)` equals
`Writer.foldUntil(producer)` on every instance, pulling the same
number of elements. `Foldable.foldUntil` is on the trait with the
`IterableOnce` (an `Iterator` is left after the satisfying element)
and `Producer` instances, and `xs.foldUntilTo(using fo)` beside
`foldTo` — in an extension block of its own, for the reason
producer-fold-until found.

Tutorial §2, guide §3/§5, typepedia and theory ch. 7 name them; the
guide's header parser and the tutorial's two lines are in
`TestDocExamplesFoldUntil`. `TestFoldUntilStreams` +5, `TestFoldUntil`
+2. Landed as 02644e9e.
