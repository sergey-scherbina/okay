## freer-base-step-extractor - one indexed base under Free and Cont: the answer types on the nodes, the runner typed by the GADT, one cast where there were two

The operator's question ("what if Cont[A, S, R] = Free[…]?") answered
in the code. `src/main/scala/Free.scala` is `enum Freer[G[_, +_, +_],
S, +R, +A]` — `Return | Inject | Bind | Delay`, one `resume`,
index-polymorphic, cast-free. `Free[F, A]` is that enum at `Lift[F]`
with both indexes `Unit`; `Cont`'s `Rep[A, S, R]` is it at `Shift = [S,
R, X] =>> (X => S) => R`, the shift body stored at its own type.

- `object Free` keeps `Return`, `Inject`, `Bind`, `Delay` at their old
  arities as constructors and patterns, so none of the 112 match sites
  across the family moved and the `direct` macro's symbol lookups
  (`okay.Free.Inject.apply` and friends) still resolve. `Free.Bind`'s
  `unapply` is the one cast: its pattern-bound type variables sit in
  its parameter type, so the type test binds them, and it answers the
  constant claim that a `Lift` tree is built with every index `Unit`.
  Stage 1 of specs/freer-base.md put the variable in the result, which
  dotty infers as `Nothing`; that was the refutation.
- Cont.scala loses `Shift.of`, `Shift.at`, `pinned` and `Cps.walkWith`:
  `Return` gives `S <: R`, a `Bind(Inject(s), f)` gives the leaf as
  `(X => T) => R` and `f` as its `X => Cont[B, S, T]`. Two class tests
  (`Leaf`, `Cps`) are `@unchecked` at the leaf's own arguments; the
  Layer 1 B walk keeps its one cast, `walked`, which is the pending
  stack's and never was the tree's.
- What the compiler added to the probe: `A` last in `Freer[G, S, R,
  A]` (a unary constructor is inferred over the last parameter, and
  `Monad[Free[F, *]]` and every `Stream` instance need `[A] =>>
  Free[F, A]`); `Lift[F] = Lifted[F]#L`, a class projection, because a
  bare lambda beta-reduces a row to a union and `!.tracing(p)([X] =>
  (e: Users[X]) => …)` inferred the whole row for `F`; `+R` on the
  base, with `ContMacro` summoning `S <:< R` at the call site for
  `tailShift`/`tailPure`. Seven `split(e) { case Say(w) => … }` sites
  in Writer and Chronicle are `(w0: @unchecked) match` now, as their
  `Bind` twins were, and so are four in okay-stream (Interop's `drive`,
  three in Pipe) and the same site in compare's
  `WriterFoldUntilBoxBenchmark` (`compare/Jmh/compile` is its gate).
  Five imports the old `Free`'s companion had counted as used — `+` in
  okay-lex, okay-ui's Form, scala2-http and jdbc's TestPool, `!.*` in
  TestDocExamplesAsync — read unused now and are gone.
- `TestInlineBudget` reads `Freer.resume`; `ProbeRowInference`'s
  shape 3 flipped from pinning a refusal to pinning an acceptance (an
  argument typed as the expanded union satisfies `R ! Delim + F`
  without type arguments now); `TestCont`/`TestFree` probe tree shapes
  with `Freer.Bind`, the enum's own pattern; three `Free[?, ?]` type
  tests (Eager, okay-spring) read `Freer[?, ?, ?, ?]`.
- `Prog` is untouched, and the item's step (4) is closed as not this
  lane: a leaf carrying a transition function is a different design.
- Spec: "The dual placement, LANDED"; the Variance and Names decisions
  say what superseded them; theory ch. 11's answer-types section and
  ch. 2, typepedia, benchmarks, README and two specs read the new
  shape. backlog: `cont-variance` rewritten for the facade's half.

Gate: the core on JVM, Scala.js and Scala Native (612 / 10 / 14
green), then `affected origin/master staged` over the family. Not
measured: `scripts/jmh-lane.sh` needs the Mac. The lanes to re-read
are the item's (statePara, relayForward, the Fib lanes, HandlerBenchmark
stepOneByOne); every node is the same object, so any movement is a
defect.
