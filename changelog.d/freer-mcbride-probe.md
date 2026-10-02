## freer-mcbride-probe — the consumed-state index is refused by the variance alone: on the invariant base the McBride loop types, @tailrec, with no continuation object

The operator's question after freer-paramonad ("в чем проблема с
индексом по МакБрайду?"). `src/test/scala/ProbeMcBride.scala` writes
the same `St` signature and the same threading loop TestFreerPara pins
as refused on `Freer[G, S, +R, +A]`, against ProbeFreerStep's INVARIANT
enum: it types — `Return` gives `S = R`, `Get` gives `X = T = R`, the
loop takes the state and answers the pair with no `k`, no `Reentry`, no
room — and Scala 3 accepts `@tailrec` with the type arguments changing
per call (Scala 2 refused it). A wrong `Put` and a run from the wrong
state are refused by the type. So the obstacle is `+R` and `G[_, +_,
+_]`, whose only reader is `Cont.tailShift`/`tailPure`'s `liftCo`.
specs/freer-base.md "McBride's reading is refused by the variance
ALONE" prices the trade (invariant indexes, one justified cast in
tailShift), names what is lost without it (a typed protocol at
`State.handle`'s cost instead of PState's 1.29x CPS) and what this tree
cannot express even then (McBride's value-dependent post-state, which
needs an index-polymorphic `Bind` continuation; Atkey's sum-typed state
is the encoding). backlog `freer-consumed-index` is the lane, in tension
with `cont-variance`. Gate: additive — TestFreerPara 8/8 + `affected
master Test/compile`.
