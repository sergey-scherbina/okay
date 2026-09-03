# 4. Free and freer

## Programs as data

A *free* monad turns "a program using operations from `F`" into a
data structure: leaves are values, nodes are operations, and `flatMap`
just grows the tree. The interpreter is then an ordinary function over
that tree, and *changing the interpretation* — run it, test it, print
it, optimize it — needs no change to the program. Wouter Swierstra's
"Data types à la carte" \[[Swierstra 2008](#ref-swierstra-2008)\] made this the standard recipe
for extensible interpreters: effects are functors, programs are free
monads over their coproduct, handlers are folds.

The classical construction requires `F` to be a **functor** — its
`Bind` stores `F[Free[F, A]]`, so sequencing must `map` into the
operation. Oleg Kiselyov and Hiromi Ishii observed that this
requirement is both a tax and a distortion \[[Kiselyov & Ishii 2015](#ref-kiselyov-2015)\]: the
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
proposed the codensity transformation \[[Voigtländer 2008](#ref-voigtlander-2008)\]; van der Ploeg
and Kiselyov's "Reflection without remorse" \[[van der Ploeg & Kiselyov
2014](#ref-ploeg-2014)\] gave the type-aligned-sequence answer that `freer` systems in
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

## Does the third road actually hold? (the linearity measurement)

The argument above is structural, and structural arguments about
asymptotics have a way of being believed rather than checked. The
rotation is *cheap per pass*, but it runs on every pull, and a
profiler had already put 38% of a merge benchmark's samples on
exactly those two lines. That is the shape of a question, not an
answer: 38% of CPU in the normalizer is what you would see both if
the tree were linear and the constant merely large, and if the tree
were quadratic and the library slowly drowning.

The two readings prescribe opposite work. If the cost per element
*grows*, the third road has failed on the shapes this library
actually builds, and the remedy is the published one — a
type-aligned sequence in place of the binary `Bind`, appending in
O(1), rotation abolished as a concept. If the cost per element is
*flat*, that rewrite removes an asymptotic that was never there,
and pays for it in a cast (or a heavy GADT) plus the 42 sites that
depend on `resume`'s three-shape invariant.

So it was measured, by sweeping the element count over an 8x range
and reading the numbers *per element* — with a bare `LazyList` walk
as the control for the platform's own scaling:

| per element | 500 el | 1000 el | 2000 el | 4000 el |
|---|---|---|---|---|
| `LazyList` (control) | 11.3ns | 11.4 | 11.0 | 10.6 |
| one `Source`, drained | 41.2ns | 39.6 | 41.5 | 40.6 |
| `Channel.merge` | 142.3ns | 121.9 | 127.9 | 131.8 |
| `Source` merge | 303.5ns | 299.7 | 300.7 | 291.6 |

Flat in every lane — drifting slightly *down*, as warm-up amortizes
over longer runs. The trees this library builds are linear, and the
third road holds.

The reason is worth stating, because it is the general lesson and
not a fact about okay. Reflection without remorse pays where binds
are **left**-nested, and a recursive stream producer is naturally
**right**-nested: each step's continuation contains the rest of the
walk, so `resume`'s first rule has almost nothing to rotate.
Left-nesting is what a `foldLeft` over a program builds — which is
why chapter 1's worst-case benchmark constructs exactly that, and
why `docs/benchmarks.md` §1 is careful to print the right-nested row
beside the left-nested one: a system without reassociation is
quadratic on the first shape (measured ×109 from N=1k to N=10k) and
linear on the second, so quoting only the first describes the
pathology rather than the library. The same discipline applies here,
pointed at ourselves: the trap is real, and whether you are *in* it
is a property of how programs are built, not of the encoding.

What the sweep found instead lives one level out. The `Writer` layer
costs ~30ns per element on its own (41 against the control's 11) and
~160ns per element *inside the merge* (292 against `Channel.merge`'s
132) — the same interpretation, some five times dearer once two
fibers contend for one channel cell. That is not a statement about
trees at all: a slower step simply spends longer in the window where
a competing CAS can land, which was measured directly as a retry
rate rising from 28.1% to 34.3% at matched capacity. The lever it
identifies is *fewer interpretation steps inside the contended
region* rather than a cheaper step — which is what a chunked stream
already is, one queue operation per chunk instead of per element,
and it measures 10.7µs against the per-element merge's 299.7µs on
the same work. The per-element price buys per-element semantics; the
chapter's honest summary is that the encoding was not the thing to
fix. `docs/benchmarks.md` §6 carries the full numbers.

## The upcast that is not free

One more thing the sweep's frame makes visible, and it is a statement
about free monads rather than about this library. `Free` is invariant
in its signature, so widening a program into a larger row walks the
tree and re-injects each operation. That looks like pure tax, and the
type-level cure is available: `F` occurs only covariantly in the
three cases, `enum Free[+F[+_], A]` passes the variance check, and
the row subtyping then holds pointwise at concrete rows — the two
`widen` calls in a merge would become coercions and the pass would
disappear.

Measured, removing that pass makes the merge **slower** — 5–7% on
2×2000 elements, bars non-overlapping across two runs. The reason is
that the walk is not only a re-injection; it is a *normalization*.
It resumes every node on the way through, so what reaches the
consuming loop is already in the head-normal, right-nested form the
interpreter wants, and the rotation it would otherwise perform — per
pull, inside the region where two fibers contend — has been done once
in advance, outside it.

So the two readings of a widening pass are not "cost" and "no cost"
but *where the same work is done*. A coercion that the type system
performs for free performs no normalization either; the interpreter
then pays it later, in the worse place. This is the practical edge of
the same fact chapter 1 states about `fold`: normalization is real
work with real value, and a design is entitled to *place* it. The
invariance of `Free`'s row is therefore a choice with a number behind
it, not a limitation to be engineered away — which is only knowable
by measuring the cure rather than reasoning about the disease.

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

- <a id="ref-swierstra-2008"></a>Wouter Swierstra. *[Data types à la carte.](https://doi.org/10.1017/S0956796808006758)* JFP 18(4):423–436, 2008.
- <a id="ref-kiselyov-2015"></a>Oleg Kiselyov, Hiromi Ishii. *[Freer monads, more extensible
  effects.](https://okmij.org/ftp/Haskell/extensible/more.pdf)* Haskell Symposium 2015.
- <a id="ref-voigtlander-2008"></a>Janis Voigtländer. *[Asymptotic improvement of computations over free
  monads.](https://doi.org/10.1007/978-3-540-70594-9_20)* MPC 2008.
- <a id="ref-ploeg-2014"></a>Atze van der Ploeg, Oleg Kiselyov. *[Reflection without remorse.](https://okmij.org/ftp/Haskell/zseq.pdf)*
  Haskell Symposium 2014.
- <a id="ref-bjarnason-2012"></a>Rúnar Bjarnason. *[Stackless Scala with free monads.](http://blog.higher-order.com/assets/trampolines.pdf)* 2012.

---

← [3 · Parameterised monads](03-parameterised.md) · [Contents](index.md) · [5 · Algebraic effects and handlers](05-effects-handlers.md) →
