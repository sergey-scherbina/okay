# 4. Free and freer

## Programs as data

A *free* monad turns "a program using operations from `F`" into a
data structure: leaves are values, nodes are operations, and `flatMap`
just grows the tree. The interpreter is then an ordinary function over
that tree, and *changing the interpretation* — run it, test it, print
it, optimize it — needs no change to the program. Wouter Swierstra's
"Data types à la carte" [Swierstra 2008] made this the standard recipe
for extensible interpreters: effects are functors, programs are free
monads over their coproduct, handlers are folds.

The classical construction requires `F` to be a **functor** — its
`Bind` stores `F[Free[F, A]]`, so sequencing must `map` into the
operation. Oleg Kiselyov and Hiromi Ishii observed that this
requirement is both a tax and a distortion [Kiselyov & Ishii 2015]: the
tax is a `Functor` instance and a `map` per layer; the distortion is
that operations must be *shaped* to carry their continuation. Their
**freer** monad stores the continuation *beside* the operation instead:

```scala
// Free.scala:29
enum Free[F[+_], A] {
  case Pure(a: A)
  // an operation, bare
  case Inject(a: F[A])
  case Bind[F[+_], A, B](a: Free[F, A], f: A => Free[F, B]) ...
}
// Free.scala:22 — the point, in one comment:
// "Free[F, *] is a Monad for every signature F, with NO constraint on F"
```

Okay is freer, and the practical payoff shows all over the tree: a
signature can be *anything with an answer type*. `Take` is a
two-constructor GADT; `Writer` is a one-constructor GADT;
`Produce[A]` is literally `Id[A]` (`Generate.scala:64`) — an identity
signature with **zero** wrapper allocation per emitted element, which
no functor-constrained free monad could express. The Writer chapters of
`docs/existentials.md` — six attempted encodings, five refuted — are
entirely a story about what an unconstrained signature may and may not
claim about its answer type; they happen *because* freer permits
signatures that carry no evidence.

## The left-nested-bind problem

Free monads have a famous performance trap: `(((m >>= f) >>= g) >>= h)`
built by a left fold makes each `fold`/`resume` re-walk the spine, and
naïve implementations go quadratic. Janis Voigtländer diagnosed it and
proposed the codensity transformation [Voigtländer 2008]; van der Ploeg
and Kiselyov's "Reflection without remorse" [van der Ploeg & Kiselyov
2014] gave the type-aligned-sequence answer that `freer` systems in
Haskell adopted.

Okay takes a third road, already visible in chapter 1: **normalize in
the one interpreter**. `Free.fold` (`Free.scala:51–58`) is a `@tailrec`
loop whose first two cases *are* the monad laws used as rewrite rules:

```scala
// associativity
case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).fold(p)(h)
// left identity
case Bind(Pure(a), f)    => f(a).fold(p)(h)
```

Every pass rotates left-nests right and discharges pure prefixes, so
the interpreter only ever confronts three normal shapes — `Pure(a)`,
`Inject(e)`, `Bind(Inject(e), k)` — and handler code across the
library matches exactly those (the comment at `Effects.scala:409–411`
makes the normal form explicit). What it buys is measured in
`docs/benchmarks.md` §1 against cats' `Free`, cats-effect IO, ZIO and
kyo on the worst case (10 000 left-nested binds); what it costs — one
`Bind` allocation per `flatMap`, as chapter 1 noted — is what chapter 6
claws back where it matters.

Chapter 2 showed the same two moves on `Cont` — defunctionalize, then
normalize in the runner — plus a third (closure fusion under a budget)
that `Free` deliberately lacks: effect programs are inspected by
handlers, and a fused closure cannot be split on a row. The pair of
types is one design at two points: `Cont` optimizes for *running*,
`Free` for *being interpreted*.

## Freer's second dividend: GADT refinement

Storing operations bare means an operation's constructor can carry its
answer type as a GADT index, and *pattern-matching recovers it*.
`Take.Await() extends Take[V, Option[V]]` (`Pipe.scala:14–17`) lets
every consumer loop write `case Left(Take.Await()) => k(oi)` with no
cast — the match refines the existential answer type to `Option[V]`.
The Writer signature converted to the same shape
(`case Say(w: W) extends Writer[W, Unit]`, `Writer.scala`) precisely to
collect this dividend, and `docs/existentials.md` records the
measurement that justified it: the wrapper costs nothing detectable on
the real benchmark (198.0µs against 203.2 for the identity encoding),
and twelve casts plus a row-splitting caveat disappeared.

## References

- Wouter Swierstra. *Data types à la carte.* JFP 18(4):423–436, 2008.
- Oleg Kiselyov, Hiromi Ishii. *Freer monads, more extensible
  effects.* Haskell Symposium 2015.
- Janis Voigtländer. *Asymptotic improvement of computations over free
  monads.* MPC 2008.
- Atze van der Ploeg, Oleg Kiselyov. *Reflection without remorse.*
  Haskell Symposium 2014.
- Rúnar Bjarnason. *Stackless Scala with free monads.* 2012.
