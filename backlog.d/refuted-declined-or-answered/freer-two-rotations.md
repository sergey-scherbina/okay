- freer-two-rotations — answered 2026-10-04: both stay, they are two
  contracts. `resume` forces everything, a nested run included, and
  always answers a head form. 76 files rely on that in their
  `(x.resume: @unchecked) match`: the interop walkers (cats, kyo, fs2),
  Stm, okay-sql's Tx, Bayes, Llm, Async's loop and the test interpreters.
  None of them can hand itself to a machine. `resumeRun` STOPS at a
  `Delay(Suspended)` and returns it unforced, which only a loop that has
  a machine face can use (the HandleFrames engines, `relay`, `translate`).
  Given to the interpreters, that node would fall through their
  `@unchecked` match. Folding them into one method puts the `Suspended`
  test into `resume`, which Free.scala's comment measured: 345 bytes,
  over FreqInlineSize, 1.32x on the loops.
