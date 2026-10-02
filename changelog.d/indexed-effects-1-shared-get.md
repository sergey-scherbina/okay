## indexed-effects-1-shared-get — PState.Threaded.get is one shared node, State.get's cast

Stage 1 of specs/indexed-effects.md. `SharedOps.getT` is the one
`Inject(PState.Op.Get())` for every state type, and
`PState.Threaded.get[S]` is that node under `State.get`'s cast and for
its reason: `Get` has no fields, so after erasure every `Get[S]` is one
object, and a program node is immutable. TestState pins `get[Int] eq
get[String]`. The byte count after it (expected the untyped State's
244 904 per 1000 steps, from 276 904) is a deferred measurement, by
the arc's rule. Gate: TestState + TestFreerPara, `okayJVM/Jmh/compile`,
`affected master Test/compile`.
