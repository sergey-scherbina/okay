## runfree-inlined-rotation - refuted (with a side finding): a private copy in runFree is not faster

The untried half of a lane refuted on 2026-09-15 (409c06e2 shrank
`Free.resume` itself, made every lane worse, and left "a third copy
in `runFree` while `resume` stays as it is" unmeasured): built as
`runFreeInlined`, a private tailrec loop that folds `resume`'s
rotation directly into `runFree`'s own body so `runFree` never calls
`.resume` — `resume` itself and its other callers (`fold`, relay's
loop, `State.run`, `Writer.run`) untouched. Three quiet rounds,
per-arm minima: `relayPrebuilt` 1.022x, `handlePrebuilt` 1.030x,
`handleCapture` 1.054x — WORSE on exactly the lanes the entry meant
to help. The first refutation's mechanism (a loop inlined into a
loop-shaped caller nests loops and loses more than the call it saves)
was never really about the JIT threshold that attempt tuned; it
applies to ANY way of merging the two loops, including by hand.
Reverted, nothing shipped.
Kept open as its own question: `effCont24` (a 24-step program, not a
10 000-op row) read 0.904x on the SAME code, bytes identical — filed
as `runfree-inlined-small-step` (backlog okay-core) rather than acted
on, since one benchmark shape does not answer whether small step
counts are the real variable. Rows `rfir-*`.
