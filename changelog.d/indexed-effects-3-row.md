## indexed-effects-3-row — the row of indexed signatures: `+~`, a match-typed unary member, splitI, the Indexed doors, State.handleIndexed

Stage 3 of specs/indexed-effects.md, in the core (Indexed.scala):
`F +~ G` is `+` at three parameters; `Unary[F]` puts an ordinary
effect in an indexed row as the match type `[S, R, X] =>> S match {
case R => F[X] }`, which reduces to `F[X]` on the diagonal and stays
stuck off it, so `Indexed.unary` takes a `State` operation and
`Indexed.effect` at a moving index refuses it by the compiler (the row
probe's throw is now a `compileErrors` pin); `TypeableI`/`splitI` are
`TypeableK`/`split` by class at three parameters; `Indexed.pure/
effect/unary` the doors; `Indexed.offDiagonal` the one exclusion arm a
handler still writes (a stuck match type is not `Nothing` at an
existential middle index). `State.handleIndexed` is the reference
handler — its own operations from the threaded state at fixed indexes
(one `@tailrec` loop), an `F` operation forwarded with the index it
came with, the `Inject` arm recursing through `flatMap`. TestFreerPara's
row probe runs on it. Not here: an inductive membership witness over
`+~` (row-membership-crash's shape; nothing needs one) and `direct`
over an indexed program (the macro's symbol table untouched). Gate:
TestFreerPara + TestState, `affected master Test/compile`.
