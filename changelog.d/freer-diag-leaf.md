## freer-diag-leaf — Freer.Diag, the diagonal leaf as a case of the node: a unary effect enters an indexed row bare, no wrapper, no cast

The operator's "Да" to the first from-scratch item. `Freer.Diag[G, R,
A](a: G[R, R, A]) extends Freer[G, R, R, A]` and the door `Freer.diag`:
the node says the operation moves no index, so a handler matching
`Bind(Diag(e), k)` continues at `R` (GADT `T = R` on the invariant
base). TestFreerPara's row is now `[S, R, X] =>> PSt[S, R, X] |
State[Int, X]` with State bare, `counted` answers `State` under `Diag`
and forwards `PSt` under `Inject`; the `At` wrapper is gone. `!.peek`
gained a `Freer.Diag` arm (the one exhaustive match over the erased
tree); Cont's `step` is `@unchecked` (a Cont never holds a `Diag`, and
a dead arm is bytes in the loop the Fib lanes price); `Free`'s doors at
`Unit` keep building `Inject`, so no match site moved. Not measured
(the runner gated beside): the JIT's view of a five-case hierarchy is
the one thing that could move, named in specs/freer-base.md "The
diagonal leaf". Gate: the six core suites 65/65, then the full
`affected master staged`.
