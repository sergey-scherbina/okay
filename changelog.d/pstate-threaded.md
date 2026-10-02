## pstate-threaded — PState as data through the threading loop: the typed protocol at 1.07x the untyped State, against 1.79x through the answer type

Item 2 of the from-scratch list, the payoff of freer-consumed-index.
`PState.Op[S, R, +X]` (`Get`/`Put`, the old state answered as `set`
does), `PState.Threaded[A, S, R]`, doors `Threaded.get`/`put` and
`Threaded.run`, `State.handle`'s `@tailrec` loop with the type moving.
TestState pins the protocol and its refusal; HandlerBenchmark gains
`stateThreaded`, the same M-step workload as `statePara`. Measured,
three lanes on one tree, MIN of 3 rotated rounds, all quiet:
stateEffect 16.96 µs / 244 904 B, stateThreaded 18.07 / 276 904,
statePara 30.39 / 301 407 — the data road 1.07x the untyped State and
0.59x the shift road; the +32 B a step is one unshared `Inject(Get())`
per read, the next rung. State.scala's header, docs/typepedia.md and
docs/theory/03-parameterised.md carry the numbers (the shift road
reads 1.79x today, 1.29x on 2026-09-17: the JIT's inlining, not a
constant). Rows in src/jmh/history.d; specs/freer-base.md "PState as
data". Gate: TestState + TestFreerPara, `affected master
Test/compile`, `okayJVM/Jmh/compile`.
