## fuse-plan-typed-term - Fuse's plan is typed on the term, ten casts gone

`Fuse.scala`'s `Plan.L(get: Any, put: Any)` held what the macro
understood as `Any`, and the four emitters read the halves back
through ten `asInstanceOf[Term]` — an `Any` where a type parameter
would do, the shape the operator's no-cast rule names outright. The
reason was real but not binding: `Plan` is declared outside any
`Quotes` and cannot name the path-dependent `q.reflect.Term`. It takes
the term type as a parameter instead — `enum Plan[+T]`, `plan`
answering `Plan[q.reflect.Term]`, `Some_` a `Plan[Nothing]` — and
every emitter reads a `Term`. One `import q.reflect.*` that only the
casts had used went with them.

No behaviour change: okay-optics' suite, whose fusion parity table
reaches the hand-written update byte for byte, is the check. The one
cast left in the file (`fuseTwice`'s `.asInstanceOf[A]` inside a
quote) is emitted code under an `=:=` the same function checks, a
different animal.

Found in the 2026-09-20 review; the last of its three code items
(with writer-collect-loops-unify and stream-fold-via-iterator).

Files: okay-optics/src/main/scala/Fuse.scala, specs/optics.md
(optics-fuse section).
