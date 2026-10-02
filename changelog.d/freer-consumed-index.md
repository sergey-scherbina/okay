## freer-consumed-index — S and R invariant on Freer: the tree serves both readings of its indexes, and a handler that consumes its index types

The operator's decision after the McBride probe ("Делаем S и R
инвариантными."). `enum Freer[G[_, _, +_], S, R, +A]`: `+R` and the
signature's covariant second parameter are gone, `+A` stays. `+R`'s
readers were two: `Cont.tailShift`/`tailPure`'s `liftCo`, now one cast
in `Cont.tailAt` with the `S <:< R` the macro summons as its parameter,
and `Cont.noProgram`, the CPS walk's never-matched placeholder, now a
throwing `Delay` at the walk's own index. TestFreerPara's reading-2 pin
flipped: `runSt`, `State.handle`'s loop with the type moving, `@tailrec`,
no continuation object, runs on the library's base and still refuses a
`Put` from the wrong state. Found on the way: a signature whose VALUE
parameter is invariant (`St[S, R, X]` against `G[_, _, +_]`) switches
the GADT off silently, because bound conformance is checked after
typing — `+X` it is; doors on an invariant signature spell their type
arguments. Records: specs/freer-base.md ("The indexes INVARIANT" and
the Variance decision, superseded again), docs/theory/03 and 04,
backlog `cont-variance` narrowed to `+A`. Gate: TestFreerPara, TestCont,
TestContMacro, TestContStack, TestState, TestProg 65/65, then the full
`affected master staged` (a signature change).
