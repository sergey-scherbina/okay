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
// Free.scala:82
enum Free[F[+_], +A] {
  case Return(a: A)
  // an operation, bare
  case Inject(a: F[A])
  case Bind[F[+_], A, B](a: Free[F, A],
                         f: A => Free[F, B]) extends Free[F, B]
  case Delay(thunk: () => Free[F, A])
}
// Free.scala:75 — the point, in one comment:
// "Free[F, *] is a Monad for every signature F, with no constraint on F"
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
the one interpreter**. `Free.resume` (`Free.scala:137`) is a
`@tailrec` member of the enum — a member wins resolution, so every
interpreter in the library reaches the one loop — whose first two
cases *are* the monad laws used as rewrite rules:

```scala
// associativity
case Bind(Bind(a, f), g) => Bind(a, f(_).flatMap(g)).resume
// left identity
case Bind(Return(a), f) => f(a).resume
// a deferred subprogram: forced here, continued AS IS
case Delay(t) => t().resume
case Bind(Delay(t), g) => Bind(t(), g).resume
```

Every pass rotates left-nests right and discharges pure prefixes, so
an interpreter only ever confronts three normal shapes — `Return(a)`,
`Inject(e)`, `Bind(Inject(e), k)` — and handler code across the
library is a three-case match over exactly those (`Free.fold`,
`Free.scala:153`, is the model; the comment on `resume` states the
invariant and why every such match is written `@unchecked`). The
fourth case, `Delay`, is the trampoline: `!.tailcall` builds one, a
capturing handler reifies the rest of the program into one, and the
runner forces it without composing anything onto it — chapter 11
explains why "defer, then wrap in `Pure`" was a tax on every bind that
followed, and what replacing it measured. What the rotation buys is
measured in `docs/benchmarks.md` §1 against cats' `Free`, cats-effect
IO, ZIO and kyo on the worst case (10 000 left-nested binds); what it
costs — one `Bind` allocation per `flatMap`, as chapter 1 noted — is
what chapter 6 claws back where it matters.

The same laziness is what makes a LOOP a program without a trampoline
of its own. `!.loop(s)(f: S => Either[S, A] ! F): A ! F` is
`tailRecM` — the operation Freeman showed every stack-safe monad can
be given \[[Freeman 2015](#ref-freeman-2015)\], and PureScript and cats
made part of `Monad` — spelled as the two-line recursion `f(s).flatMap
{ case Left(next) => loop(next)(f); case Right(a) => Pure(a) }`. The
recursive call is inside the `flatMap`'s continuation, so building
the program allocates one `Bind` and returns; the call is made when
the interpreter resumes that node, in the runner's loop rather than
on the caller's stack — Bjarnason's trampoline again, but paid by the
tree every program already is instead of by a `Delay` per round. A
million rounds at the `Pure` row run on the default stack
(`TestBangLoop`). The "state decides when to stop" form recurs at
two other seams: `FoldUntil` over an input (chapter 7) and
`Proc.Iter` over a free arrow (`okay-workflow`, the one constructor
the literature's selective functor lacks).

Chapter 2 showed the same two moves on `Cont`, and since 2026-09-15
they are the *same* moves: `Cont` is this tree at the signature "a
function of the continuation", with one refinement `Free` itself
lacks — a fresh leaf absorbs its first bind — because effect programs
are inspected by handlers, and a fused closure cannot be split on a
row. The pair of types is one tree at two uses: `Cont` optimizes for
*running*, `Free` for *being interpreted*, and chapter 11 is the
account of how that became one enum rather than two.

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
in its signature, so the type system cannot see that a program at `F`
is one at `F + G`; for years of this library `!.widen` answered that
by WALKING the tree and re-injecting each operation. That looks like
pure tax, and the type-level cure is available: `F` occurs only
covariantly in the three cases, `enum Free[+F[+_], A]` passes the
variance check, and the row subtyping then holds pointwise at
concrete rows — the two `Writer.widen` calls in a merge would become
coercions and the pass would disappear. (Since widen-split,
2026-09-23, `!.widen` itself IS a coercion — `Row`'s one cast,
sound by erasure, nothing forced — and the walk keeps the name
`!.normalize`; what the paragraph below is about is `Writer.widen`,
the element-type re-tell a merge still walks.)

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

The *answer* axis is the other story, and it went the other way
(free-answer-variance, 2026-09-23). `Free[F, +A]` also passes the
check — `A` sits in `Return`'s field, under a covariant `F` in
`Inject`, and in `Bind`'s result — and on this axis there is no walk
to lose: nothing rebuilds a tree to move an answer up, so the only
thing covariance could change is typing. Three doors had been paying
an identity `map` — a `Bind` node built for the type checker alone —
to move a GADT-proved `A' <: A` into an `A ! Row`; they are gone, and
`javap` shows the seven core classes byte for byte unchanged by the
annotation. The price was four sites where inference had leaned on
invariance to pin an answer: a handler whose first arm now inferred
`Right[E, A]` instead of `Either[E, A]` (pinned), a GADT match in a
`direct` block whose first arm is concrete (ascribed `: X`), and the
`direct` macro's two element comparisons (`<:<`, not `=:=`, since an
inlined `raise` is now an `Inject[…, Nothing]`). So `A ! F` reads as
it should: covariant in what it answers, invariant in what it may do —
the first by the ordinary rule, the second by the number above.

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
- <a id="ref-freeman-2015"></a>Phil Freeman. *[Stack safety for free.](https://functorial.com/stack-safety-for-free/index.pdf)* 2015.

---

← [3 · Parameterised monads](03-parameterised.md) · [Contents](index.md) · [5 · Algebraic effects and handlers](05-effects-handlers.md) →
