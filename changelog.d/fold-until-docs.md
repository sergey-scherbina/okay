## fold-until-docs - the stopping fold and `!.loop` in the tutorial, the guide, the typepedia and the theory

fold-until and loop-on-bang landed with a spec and a one-row table
edit, and the operator asked for the documentation users read:
examples, explanations and the literature. Now written: the tutorial
(§2, beside `countdown` — a `find` that stops two steps into a
million, `!.loop` telling the digits of a number), the guide (§2
`!.loop` with the Collatz count carried in the state, §3 the stopping
fold with the two laws every consumer keeps and why `done` is a
branch rather than an `Either` per element), the typepedia (both
entries), theory chapter 4 (`!.loop` as Freeman's `tailRecM`, paid by
the tree every program already is) and chapter 7 (a consumer that
can say done: Kiselyov's iteratees, the Moore-machine presentation,
the `foldl` triple, Gibbons and Jones' unfold as the dual) — four
references added, all resolving. The chapter-7 text landed INSIDE the
"Iteratees" section iteratees-docs (f2d543fc) wrote the same hour:
Kiselyov and the iteratee-as-data reading were already there, so what
this lane adds is the Moore-machine presentation, the `foldl` triple
and the unfold dual, and the guide's §3 pointer goes to that section.

Every snippet is `TestDocExamplesFoldUntil` (okay-stream, 4), run by
the gate, with the answers the pages print. Writing the tutorial's
found a gap: `Stream.foldUntil(writerProgram)` does not infer through
the type-lambda instance and `Writer.foldUntil` asks for evidence a
reader should not see, so a pure writer program gained
`.foldUntil(using fo)` beside `uncons`/`toLazyList` (Stream.scala).
Landed as 2b2146e3.
