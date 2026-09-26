# The user guide

Okay is built in layers, each founded on the one below. This guide
walks them bottom-up; the [tutorial](tutorial.md) walks them by use.

## 1. Delimited control: `Cont`

`Cont[A, S, R]` is the parameterised (Atkey-style) continuation monad:
a computation of `A` that changes the answer type from `S` to `R` —
i.e. `(A => S) => R`, defunctionalized so that running any flatMap
chain is stack-safe. `shift` captures the continuation, `reset`
delimits it. `Control[M[_,_,_]]` is the final-tagless interface;
`Cont` (data, stack-safe) and `Func` (the raw function encoding) are
its instances. You rarely touch this layer directly — it is what
handlers are made of. When you DO want it, the door is `Delim`
(multi-prompt delimited control as an effect): cancellable Dialog
scopes (`Scope`), the streaming cut (`Cut`), the agent stepper and
the sim scheduler are its shipped consumers — and the prompts can
be AMBIENT (`Scope.bounded { … Scope.exit(v) … }` exits the nearest
scope by nesting; a bound prompt still crosses).

## 2. Effects: `A ! F`

A computation of `A` performing operations of the signature `F` is
`A ! F` — a freer-monad tree, covariant in `A` (an `Int ! F` is an
`Any ! F`, no node added) and invariant in `F` (widening a row is
`.at[R]`/`.plus[G]` or the `!.widen` walk — a measured choice,
specs/writer-covariance.md). Signatures combine as unions:
`A ! State % Int + Throws % String`. The empty signature is `Pure`
(`A ! Pure` is a pure computation; `F + Pure = F`).

A handler interprets operations into continuations — `F !> S` is
literally a natural transformation into `Cont`, and `Cont` is this
same freer tree at the signature "a function of the continuation"
([theory ch. 11](theory/11-one-tree.md)): a program and its meaning
are made of the same nodes. Three ways to run:

- `runWith` — a per-operation `Handler[F]` (comonadic: each operation
  answers with a value);
- `!.relay` — tail-resumptive handling: the answer-polymorphic handler
  must resume exactly once, so the loop is tail-recursive; the fastest
  path (Reader and Async run at relay speed);
- `Effects.handle` — the general form: abortive handlers (Throws),
  multi-shot handlers (Choice explores every branch), forwarding.

Two ENCODINGS, one interface (`Effects[M]`): `Free` (the tree — for
stepping, relaying, stack safety on any bind shape) and the opt-in
`Eager` (`import Eager.given` — the kyo trick: pure binds apply at
construction, 10x under kyo on pure chains, with kyo's hazards stated:
construction evaluates, so self-referential programs diverge). Fused
build-and-run speed is an inline handler-passing program over
`Control` (`Interpr`, for a program static at its call site), not a
third encoding: the Church one was measured slower than the fused
tree loop and removed.

A loop is a program too. `!.loop(s)(f: S => Either[S, A] ! F): A ! F`
runs `f` from `s`, continues from a `Left` and answers a `Right` — the
`tailRecM` of cats and PureScript, at this library's row. It needs no
trampoline of its own: the recursive call sits inside the `flatMap`'s
continuation, so it is made when the interpreter resumes that `Bind`,
never on the caller's stack — a million rounds at the `Pure` row run
on the default stack, and every iteration may perform `F`:

```scala
val steps: Int ! Nothing = !.loop[(Int, Int), Int, Nothing]((27, 0)) { (n, k) =>
  pure(if n == 1 then Right(k) else Left((if n % 2 == 0 then n / 2 else 3 * n + 1, k + 1)))
}
!.run(steps)   // 111 — the Collatz steps from 27, the count carried in the state
```

The state is what the loop remembers between rounds; a dialog that
re-shows itself until the user answers (`Toolkit.prompt` in okay-ui),
a receive loop, a retry with a budget are all this shape, and writing
them over `!.loop` is what makes the "when do I stop" decision a
VALUE (`Right`) rather than a branch that forgets to recurse. The
same form over an input is `FoldUntil` (§3), and over a free arrow
`Proc.Iter` (durable-workflows.md).

The standard effects: `Reader` (environment), `Writer` (telling IS
streaming — see below), `State` (+ the type-changing `PState`),
`Throws` (typed errors, `runEither`), `Choice` (nondeterminism,
multi-shot), `Async` (Loom-style, below), `Resource` (the region:
releases run at the scope's end, in reverse, surviving handled aborts).

The floor of the library is available as an effect too: **`Delim`**
is multi-prompt delimited control — `Prompt[R]` tags a delimiter and
carries its answer type, `push` installs one, and
`shift`/`shift0`/`control`/`control0` capture the continuation up to
a NAMED prompt (so a capture can cross an intervening delimiter,
which nested handlers cannot express). The captured continuation
comes back as a PROGRAM, hence multi-shot for free. This is the door
through which a user defines their own effects: a generator is a
prompt plus a shift, with no signature and no handler added.

Over `Choice` sits BACKTRACKING as a library (`Logic`, LogicT-style):
`msplit` splits a search into its first answer and a program for the
rest, and everything derives — `cut`, `ifte` (soft cut /
negation-as-failure), `interleave` and `fairBind` (fair search: two
infinite branches take turns, so a witness is found where the plain
bind diverges), `observe(n)`. A `LazyList` of alternatives is an
infinite choice point.

**Probabilistic programming is a weighted choice, and inference is a
HANDLER over it** — the Hansei design (Kiselyov & Shan, "Embedded
probabilistic programming", DSL 2009). `Prob.dist` names the choice,
`Prob.observe` conditions on evidence (a false observation prunes the
branch — no alternatives, zero weight, exactly `Choose`'s own `empty`),
and `Prob.runExact` answers the full posterior by EXHAUSTIVE MULTI-SHOT
capture — the one thing a one-shot effect runtime cannot host at all:

```scala
import Prob.*
val wetGrass: Boolean ! Dist =
  for
    rain <- dist(true -> 0.3, false -> 0.7)
    sprinkler <- dist(true -> 0.4, false -> 0.6)
    _ <- observe(rain || sprinkler)
  yield rain
!.run(runExact[Boolean, okay.Pure](wetGrass)).posterior(true)   // 15.0/29.0
```

`Prob.runRejection` is the other reading of the same program — single-
shot, comonadic sampling, no capture at all — and both agree on the
textbook wet-grass Bayes net and a small two-state HMM, checked
against hand-computed posteriors (specs/prob-effect-hansei.md).

And the typeclass hierarchy earns its keep in
the generic combinators — `traverse`/`sequence`/`replicateA`,
`guard` (the pruning conditional of every search), `*>`/`<*`,
`whenS`/`unlessS` — written once, running over any instance. The rung
below the monad is where two of them earn their keep. `Validated[E, A]`
collects EVERY error instead of stopping at the first, because an
applicative has no way to stop (it needs a `Semigroup[E]`, has no
`flatMap` by design, and `okay-conf` uses it to report every bad
environment variable in one run). okay-codec's schema walk speaks
it too: `Validate.validated(schema)(json)` is the accumulating decode
read as a `Validated[Validate.Errors, A]`, so two schemas' errors —
each at its dotted path — combine under one `app`:

```scala
val o: Validated[Validate.Errors, Order] = Validate.validated(summon[Schema[Order]])(bad)
val a: Validated[Validate.Errors, Address] = Validate.validated(summon[Schema[Address]])(addr)
val both = Validated.valid[Validate.Errors, Order => Address => (Order, Address)](x => y => (x, y)).app(o).app(a)
```

The
rung below the monad is worth reaching for on purpose: a program
written as a `Static` (the free selective — `Pure | Op | Ap | Select`,
no `Bind`) can be READ before it runs — `leaves` lists the operations
it may perform, `toFree` runs it the ordinary way, and `foldMap` into
an accumulating carrier turns N leaves into one round trip.

### Your own effect

The short version is below; the full worked tutorial — four
interpretations of one program, the handlers, the interpreters, and
what bites — is **[Your own effect](your-own-effect.md)**.

A signature is an enum whose cases carry their own answer types, and
that is the whole declaration:

```scala
enum Users[+A] derives Effect:
  case Find(id: Long) extends Users[Option[String]]
  case Save(id: Long, name: String) extends Users[Unit]

object Users:                       // optional: `Users.Find(id).perform`
  inline def find(id: Long): Option[String] ! Users = effect(Find(id))
  inline def save(id: Long, name: String): Unit ! Users = effect(Save(id, name))
```

`derives Effect` writes the runtime test a row split needs (a row is an
untagged union, so a handler for F meeting an operation in `F + G`
decides by class), and registers the signature for direct-style
auto-coloring. `derives TypeableK` gives the first without the second.
It is not optional: there is no generic fallback, so a signature that
declares nothing is a compile error where it is USED — which is the
right place for it, since the alternative was a warning at every use
site that the effect's author never saw. A parameterised signature says the same thing:
`enum Your[S, +A] derives Effect` abstracts the LAST parameter, and
its test is then by class only — so a row may hold one `Your`, not
two at different S.

**Putting an operation in a wider row.** `p.plus[R]` adds R to whatever
row `p` has; `p.at[R]` names the target row instead — better when
several operations land in the same row, and required when that row is
known only by MEMBERSHIP, as a row-polymorphic helper's is. A BIND
whose continuation answers in another row is `p.bind(x => q)`: the
result lands in the union of the two rows, the other row read off
`q` with nothing written (`Reader.ask[Int].bind(e => Writer.tell(
s"$e"))`, `import okay.Row.bind` as with `plus`/`at`); `andThen`
is the same with the answer dropped. Inside a
`direct` block none of this is spelled — marks widen
(`[R[+_] : Has[State % Int]]`).
Both are one cast under a witness, measured at the same B/op as
constructing the operation at R. Row ORDER is not a thing: `+` is a
union, so `A ! Users + Abort` and `A ! Abort + Users` are the same
type.

**Stopping.** `Abort` (= `Throws % Unit`) is failure carrying no
information. With it in the row, a refutable pattern and an `if` guard
work in a for-comprehension — both desugar to `withFilter`, which needs
somewhere for the dropped step to go — and `runOption` answers with
what happened:

```scala
def rename(id: Long, to: String): Option[String] ! Users = runOption {
  for
    case Some(old) <- Users.find(id).plus[Abort]
    _              <- Users.save(id, to).plus[Abort]
  yield old
}
```

`save` cannot run for a missing id because it is not REACHABLE. Outside
a for-comprehension the same demand is `ensure[R](cond)`, and a failure
is answered in the row by `p.orElse(q)` / `p.recover(h)`. Where the row
carries `Choose` instead, the same pattern PRUNES the branch and the
search goes on.

**Absence — `Maybe`** (specs/core-gaps.md). `Abort` is a failure; a
value that is simply not there is `Maybe`, an effect of its own lifted
from an `Option`. `Some(x).maybe` answers `x`, `None.maybe` stops the
program, and `Maybe.run` answers the `Option` at the end:

```scala
def age(name: String): Int ! Maybe = ages.get(name).maybe
val both = for a <- age("ann"); b <- age("bob") yield a + b
val stops = for a <- age("ann"); b <- age("eve") yield a + b
// Maybe.run(both) answers Some(73), Maybe.run(stops) None
```

Because it is not a `Throws`, it shares a row with one — which `Abort`
cannot, since a row holds one `Throws` — and "not there" and "broken"
come out of two handlers:

```scala
type R = Maybe + Throws % String
def adult(name: String): Int ! R =
  age(name).at[R].flatMap(a => if a >= 18 then pure(a) else raise[String, Int]("minor").at[R])
// runEither(Maybe.run(adult("eve"))) == Right(None); a minor is Left("minor")
```

Stopping is only one reading of an absence. The handler's SCOPE decides
what `None` ends, and a narrow scope SKIPS instead. `Maybe.collect` runs
each element under its own handler and keeps the ones that were there.
`Maybe.prune` turns an absence into a dead branch of a search, so
`runChoice` answers the branches where everything was found:

```scala
assertEquals(!.run(Maybe.collect(List("ann", "eve", "bob"))(age)), Vector(31, 42))
val known: Int ! Maybe + Choose = for n <- choose("ann", "eve", "bob").plus[Maybe]; a <- age(n).plus[Choose] yield a
assertEquals(!.run(runChoice(Maybe.prune(known))), Seq(31, 42))
```

`p.orElse(q)` tries `q` where `p` found nothing, `p.getOrElse(a)` ends
in a default, and a refuted `case Some(x) <-` in a `Maybe` row stops
through `Maybe` (it wins over `Abort` where a row has both). An
`Either` from a library joins a `Throws` row with `.orRaise` — the
`Right` answers, the `Left` is raised:

```scala
val sum = for a <- ok.orRaise; b <- bad.orRaise yield a + b
// runEither(sum) == Left("no")
```

**Fresh values — `Supply`.** One operation, `Supply.next[S]`: each draw
answers a value not answered before (Launchbury's supply; `Fresh` in
fused-effects and polysemy). `Supply.run(first)(step)` makes the
values; `Fresh` is the numbers, from 0. Unlike `State % Long` there is
no `set`, so the code that draws cannot rewind the supply, and the seed
is threaded through the answer the way `State`'s is, so under `Choose`
each branch continues from where it was captured:

```scala
val three = for a <- Fresh.next; b <- Fresh.next; c <- Fresh.next yield List(a, b, c)
assertEquals(!.run(Fresh.run(three)), List(0L, 1L, 2L))
```

**Scoped effects — `recover`×`State`'s order, and `Reader.local`**
(specs/scoped-effects-laws.md). A `catch`/`local` whose body performs
OTHER effects raises a question algebraic operations never do (Wu,
Schrijvers & Hinze, "Effect handlers in scope", Haskell 2014): does a
`State` write made INSIDE survive the catch, or roll back with it?
`recover` answers it by its own definition — GLOBAL by default, since
it forwards `State`'s operations unchanged, never buffering them to
undo:

```scala
type Row = State % Int + Throws % String
val p: Int ! Row =
  State.set[Int](5).at[Row].flatMap(_ => raise[String, Int]("boom").at[Row])
    .recover(_ => State.get[Int].at[Row])
// State.run(0)(runEither[Int, State % Int, String](p)) == (5, Right(5)) — the set(5) sticks
```

For a SCOPED, transactional attempt instead, run `State` INSIDE the
guarded block rather than outside it — its writes are then that
sub-run's own business, discarded whole on a raise. `Reader.local`
overrides `ask` for one block the same way `recover` answers `raise`
for one block — built on the same tool, `Effects.handle`:

```scala
val p = Reader.local[Int, Int, okay.Pure](_ * 10)(Reader.ask[Int])
// Reader.run(5)(p) == 50; asking OUTSIDE the local still sees 5
```

Two things about it are worth knowing before reaching further:
nesting `local(f2)(local(f1)(p))` composes INSIDE-OUT (`f1(f2(r))`,
not the mtl-style `f2(f1(r))` a first guess assumes) because `local`'s
own bookkeeping ask is an ordinary `ask`, reachable by an enclosing
`local` exactly like a user's; and `local`/`recover` cannot reach
inside another effect's OPAQUE operation payload (a `Delim.push`'s
`body`, chiefly) — a plainer, more basic limit than Kiselyov, Shan &
Sabry's "Delimited dynamic binding" (ICFP 2006), which is about a
narrower case one level past this one. specs/scoped-effects-laws.md
has both, found (and one first written down wrong) against tests
that were run rather than assumed correct.

**Handling.** `runWith(using h)` for a per-operation `Handler[F]`;
`h.tracing(log)` makes any handler a recording one, since the
operations are already data; `!.translate` interprets each operation
into a PROGRAM in another row (a `Handler` answers with a value, so it
cannot itself tell or get), and `!.interpret` is the same with the
widening done for you, for when the target row is BIGGER than the
source's:

```scala
def tracked[A, F[+_]](p: A ! (Users + F)): A ! (State % Store + Writer % String + F) =
  type R = State % Store + Writer % String + F
  !.interpret(p):
    [X] => (e: Users[X]) => e match
      case Find(id) =>
        for
          store <- State.get[Store].at[R]
          _     <- Writer.tell(s"find($id)").at[R]
        yield store.get(id)
      ...
```

The expected type solves every row, so there is no type argument and
`F` — whatever the caller was already doing — rides through untouched.

Interpreters compose, and are better small. `!.tracing(p)(show)`
records every operation into a `Writer` and then performs it exactly
as before — it answers nothing, and knows nothing about the effect
beyond `show` — so the storage half can be written without a Writer
anywhere in it:

```scala
def tracked[A, F[+_]](p: A ! (Users + F)): A ! (State % Store + Writer % String + F) =
  stored[A, Writer % String + F](!.tracing(p)([X] => (e: Users[X]) => e.toString))
```

Order is the meaning: recording happens BEFORE interpretation, so the
log holds what the program ASKED, not what the store did about it. One program, four handlers — SQLite, a Map,
State + Writer, and a trace of any of them — is
`okay-jdbc/src/test/scala/okay/demoeff/UsersDemo.scala`, runnable with
`sbt "okayJdbc/Test/runMain okay.demoeff.UsersDemo"`.

**Several instances of one effect.** A row is split by a RUNTIME test,
so two members of the same signature are told apart exactly when the
operation carries something to compare. `Tag` is that, for any
signature: a key, and a test that reads it.

```scala
type Small = Tag.Of["small", State % Int]
type Big   = Tag.Of["big",   State % Int]

// an ordinary function, written against a plain State, run twice
// at two different states in one program:
val twice: (Int, Int) ! (Small + Big) =
  for
    a <- Tag.tag["small", State % Int](bump(1)).plus[Big]
    b <- Tag.tag["big",   State % Int][Int, okay.Pure](bump(10)).at[Small + Big]
  yield (a, b)
```

The second clause spells its types, `[Int, okay.Pure]`: a program of
`State` alone infers `State` back as its rest-of-row instead of `Pure`, and
`.at` needs the `Pure` row. (The first clause gets that from `.plus`.)

`tag` walks a finished program and puts every operation of F under the
key — which is the point: the function did not have to be written for
this. Handling needs no new handler: `untag` strips one key and hands
back the plain signature, and the effect's own handler takes it from
there.

Where the instances are MADE rather than named, `Refs` is the
counterpart — cells created at run time, one row member however many,
identity by cell, at the price of a heap and one stated cast. And the
third route is the one `Delim` already has: a fresh prompt per handler
installation, scoped dynamically, with the program carrying the
prompt.

And any type constructor is a signature: `List(1, 2).perform` is
nondeterminism, handled by `runSeq` — which is `runChoice`'s handler
unchanged, because `Choose[+A](as: Seq[A])` is a box around exactly
this.

## 3. Streams: codata by `uncons`

A stream is defined by ONE observation:
`Stream[S[_], F[+_]] { def uncons(s): Option[(A, S[A])] ! F }` — the
next element and the rest, inside the effect `F` (`Pure` for pure
streams, `Async` when the next element must be awaited). LazyList is
the final coalgebra every stream unfolds into (`toLazyList`, memoized);
`iterator` is the linear consume-once view (fused, faster).

`Writer` is the stream with a RESULT: `A ! Writer % W` computes `A`
telling `W` — a one-constructor GADT whose tell answers nothing,
observed by `Writer.uncons: Either[A, (W, rest)]`. `Producer` is the
diagonal cousin. Effect handlers forward the telling, so State,
Reader and Throws handlers ARE stream transformers.

The diagonal has one trap, and it is worth one paragraph because a
seam typed on it (`Blob.put`) caught a consumer with it. `Produce` is
the identity signature — an operation IS its element — so a
producer's element type sits in the ANSWER position, and `pure(a)`
type-checks wherever `produce(a)` does. It emits nothing: a
producer's `Pure` is its END, read as `None`. `produce(a)` is the
emit; in a wider row, `produce(a).plus[Async]` (Row's zero-cost
coerce). And a producer's answer is phantom, so `uncons` drops it at
its `None` — `Producer.each(p)(f)` runs every element through `f` and
KEEPS the answer, which is what `Blob.get`'s outcome needs. The two
carriers convert in one walk each: `Source.ofProducer` /
`fromProducer` (element type named apart from the answer, since the
identity signature cannot) and `Source.toProducer(s)(end)`.

The bridge goes both ways: a writer program IS a `Stream` (in `Pure`,
or in whatever effects it also performs), and `Writer.of(s)` turns any
stream — a List, a LazyList, a Producer, a Channel — back into the
program shape that `pipe`, `through` and the stages consume, its own
effects forwarded into the row rather than run behind the caller.
`Writer.map` transforms the told values in place, where `Stream.map`
lands in LazyList and forgets that the elements are still to be
performed. An asynchronous stream has a name of its own —
`Source[W] = Unit ! Writer % W + Async`, built by `Source(a, b, c)`
or `Source.of(stream)` — and is an ordinary `Stream` in `Async`.
`Source.unfold(s)(f)` generates one directly from a step function
(`ZStream.unfold`'s shape — 3x ahead of it, measured, since `unfold`
pays a chunk-of-one tax any array-native representation does);
`Source.range` is that specialised to `Long` and costs one fewer
tuple allocation per step.

One unfold, four carriers: `generate(a)(f)(g): F[B]` runs a loop from
seed `a`, telling `f(a)` each round and continuing from `g(a)`, into
whichever `F` has a `Put` instance — `nats`/`fibs` are the two
examples. `Put[S[_]] { def put[W](w: W): Unit /> S[W] }` answers
`Unit`, not the element: the obvious diagonal signature
(`put[A](a: A): A /> F[A]`) forces the carrier to answer with what it
was just told, which is why the instances are `LazyList` (laziness:
`put` captures the continuation in the lazy tail), `Producer` (the
identity signature, `put` an ordinary emit), `Feed[W] = Unit ! Writer
% W` (`put` an ordinary tell) and `Source` (the same tell, widened
onto the row that also admits `Async`) — a live, asynchronous
generator for free, which a diagonal `Put` could never have given
`Source`: its own answer is always `Unit`, never the element.

Which carrier for a NEW seam (producer-to-writer-carrier, stage 1):
name the element in the type, not the answer. `Feed[W]` when the
stream performs no other effect, `Source[W]` when it performs `Async`
— both make the `pure(a)` trap VISIBLE rather than closing it by a
type error: their answer is always `Unit`, so `pure(w)` for an
element `w` needs Scala's own value-discard adaptation to compile at
all, and `-Wall`'s `[E190]` lint flags exactly that (verified by a
real compile, not `compileErrors` — munit's macro reports hard errors
only and drops warnings entirely), which this repo's gate then
refuses as any other warning. `Producer`'s identity signature has no
such tell — `pure(a)` type-checks as an ordinary, unflagged answer and
emits nothing (the paragraph above) — which is why it is the PURE
SPECIAL CASE for code already written against it, not the default for
code being written now.

Two terminals read the whole thing while
staying IN the program — `runCollect: Vector[A] ! Async` and
`runForeach(f: A => Unit ! Async): Unit ! Async`, this library's own
`run`-prefix (`Writer.run`, `Async.run`) at the shape `ZStream`'s and
fs2's terminals have — for a caller composing further with `flatMap`
rather than reading synchronously; `toLazyList` still blocks per pull
and measures faster in a single-threaded run (`runCollect` is 30%
slower building a `Vector` this way), so reach for the terminal that
matches the caller, not by default (docs/benchmarks.md §6c).

Consumption is algebra: `Fold[A, S]` (a start and a step; every
`Monoid` gives one; `Group` adds the inverse that makes sliding
windows subtract instead of recompute), `Foldable` (the push side),
`Aggregator[-In, Acc, +Out]` (Fold + merge + present — the merge is
`(zero, seqOp, combOp)`, which is why one aggregator runs over Chunks,
on Spark and on Flink unchanged; `zip` computes several statistics in
one pass; sketches — HyperLogLog, Count-Min, t-digest — are the
approximate ones, honest monoids with stated error). The road TO an
aggregation is a seam of its own, `Bulk[D[_]]` (specs/bulk.md): read a
CSV, map, filter, join, expand, cache, aggregate — written once against
`D`, run on `Chunks` in one JVM, on Spark (`SparkBulk`) or on a
machine's cores (`okay.java.Parallel`) by the instance in scope. No
evidence per element type: a platform stores objects, and the Spark
instance says so in its type rather than asking a `ClassTag` per step.
Over the seam the same road is an EFFECT, `Tables`: the plan is a value
(`!.tracing` prints it), the handler is one translation into
`State % Heap[D]` for every platform, and an operation the seam lacks —
`Sort` — is a new signature in the row, answered through the primitives
or natively, with no platform's build touched. The heap holds plans, not
values: an action forces a table's whole lineage as one `Tables.Plan`
tree, rewritten first — a `columns(...)` projection pushed into the
read so the platform prunes at the parser, the smaller side of a join
turned to the right — and measured to pay on both platforms.

A `Fold` reads its whole input by construction — `Fold.exists` keeps
scanning after the first hit, and says so. Where the consumer knows
when it has seen enough there is `FoldUntil[A, S, R]`
(specs/fold-until.md): the same start-and-step plus `done(s)`, asked
before the first element and after every one, and `end(s)`, the
result from wherever the walk stopped. A consumer pulls NOTHING once
`done` answers — the chunk after the satisfying one is never
produced, the Async operation after the satisfying tell is never
performed — and `take(0)` pulls nothing at all. `find`, `headOption`,
`exists`, `forall`, `take(n)` are instances; `FoldUntil.until(z)(step)
(finish)`, with `step: (S, A) => Either[S, R]`, is the shape a caller
usually has in hand, kept as an adapter rather than the primitive so
that no consumer pays a `Left` per element. One instance runs on every
carrier: `Stream.foldUntil` (any `Stream`), `xs.foldUntilTo` (any
`Foldable` — a `List`, an `Iterator` left positioned after the stop, a
`Producer`), `program.foldUntil` (a writer program — pure, or
effectful with its `Handler` in scope), `Chunks.foldUntil`,
`Take.foldUntil` (the fold as an iteratee, §5), and `Writer.foldUntil`,
`Producer.foldUntil`, `Source.runFoldUntil` (the effectful ones,
answering `R ! F`):

```scala
Source.range(0, 1000000).runFoldUntil(using FoldUntil.take[Long](3))   // Vector(0, 1, 2) ! Async

Chunks.foldUntil(Chunks.nats[Int]())(using
  FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 100 then Right(s) else Left(s + a))(identity))
// 91 — the running sum stopped itself before 105; one chunk of the infinite stream was ever filled
```

A writer program read by a stopping fold is a **generator** in
Python's sense — the body runs to its next tell when asked and no
further — and `Gen[W]` (specs/generators.md) is that program with the
words a for-comprehension uses, a `stop`, and the readers above as
methods. Three ways to write one: a for-comprehension over `Gen`
(`map`/`flatMap`/`withFilter` are element-wise, no macro), a
`generator[W] { … }` block in okay-direct where `Gen.emit`, `Gen.stop`
and `for … yield` are the words, and `Gen.of` over any writer program
you already have. `iterator` is the Python semantics made literal:
`next()` runs the body to its next tell and holds the continuation for
the call after.

```scala
val squares: Gen[Long] = for n <- Gen.unfold(1L)(i => Some((i, i + 1))) yield n * n
squares.take(3).toList                        // List(1, 4, 9) — the body ran three steps, then stopped

val countdown: Gen[Int] = generator[Int] {    // a block: while/if/recursion, emit, stop
  var i = 3
  while i > 0 do { Gen.emit(i).?; i -= 1 }
}
countdown.iterator.next()                     // 3 — the body has run to its first yield and no further
```

A generation ends three ways — the body returns, `Gen.stop` from
inside a loop, or the reader stops (`take`, `first`, a `foreach` that
throws) — and a `Gen` is a value: reading it twice runs the body twice
(`toLazyList` memoises). The whole story, with the papers, is in
[direct style](direct-style.md#generators-yield-pulled-by-the-reader).

**`zip`, without materializing either side** — the one construction
fold-based fusion cannot express (element-wise stages fold ONE source
to completion; `zip` needs both advanced in lockstep), and strymonas's
own hard case (Kiselyov, Biboudis, Palladinos & Smaragdakis, "Stream
fusion, to completeness", POPL 2017): zipping a generator built by
`flatMap` — several elements per outer step — without a list in
between:

```scala
val left = Gen(1, 2, 3, 4).flatMap(i => Gen.from(Vector.fill(i)(i)))   // 1, 2,2, 3,3,3, 4,4,4,4
val zipped = left.zip(Gen('a', 'b', 'c', 'd', 'e', 'f')).toList
// zipped == List(1 -> 'a', 2 -> 'b', 2 -> 'c', 3 -> 'd', 3 -> 'e', 3 -> 'f')
```

The left side would run forever if it came from `Gen.unfold` with no
end — and does, correctly, since `zip` pulls it one step at a time
through the same `.resume` every other reader uses, with no special
case for where a step came from (specs/strymonas-zip-fusion.md).

Why `done` and not a step that answers `Either` at the bottom: a
`Left` per element is an allocation in every consumer, and this
library has measured that price out of every walk it has (`split`
not `<|>` in `Writer`, no `Option` per element in the specialised
iterators, the boxed accumulator as the whole cost of a fold);
`done` is one branch — measured at most ~10% over the boxed `Fold`
on 10k Longs, inside the error bars. And the box IS the cost here as
it was for `Fold`: the same loop with the state declared `long` runs
25x faster, so `FoldUntil.OfLong`/`OfInt`/`OfDouble`/`OfBoolean` exist
(`FoldUntil.long(z)(f)(stop)(finish)` builds one; `exists`/`forall`
are `OfBoolean`) and the chunk, stream and `Foldable` walks dispatch
on them. The theory chapter on streams places the
stopping fold in the literature — Kiselyov's iteratee as data, a
Moore machine, the `foldl` triple with a halt
([theory ch. 7](theory/07-logic-streams.md#iteratees-the-consumer-as-a-program)).

An aggregation over an event STREAM needs one thing the algebra does
not carry: when a window is COMPLETE. `Windows`
(specs/event-time-windows.md) is that operator — keyed tumbling or
sliding windows over any `Aggregator`, closed by a bounded
out-of-orderness watermark (`max(seen) - lateness`, monotone),
emitting a `Pane(start, end, key, value)` as the watermark passes each
window's end, and DROPPING (and counting) an element that arrives
after every window it belongs to has closed. It reads no clock —
windowing is a function of the data's own time — so it runs on all
three platforms, and it comes in two forms: the class
(`w.add(a)(emit)` / `w.close()(emit)`, the fast loop) and
`Windows.stage`, a `Stage[A, Pane[K, O], Unit]` that composes under
`through` like any other stage and costs 2.6x the loop for it.
docs/benchmarks.md §20 prices both against Flink's own window operator
on the same job over the same data.

Both sides of that algebra are specialized, and the split is the same
one everywhere: where the step is written at the call site, `inline`
takes it (`Chunks.foldLeft(p)(z)(f)`, 38.2us -> 7.0 per 10k Longs);
where the fold arrives as data and nothing can inline — an
`Aggregator`'s, a java `Collector`'s, one chosen at run time — the
accumulator is declared where it is already primitive
(`Fold.OfLong` and its siblings, `Aggregator.OfLong` and its). Only
the accumulator, because that is measured to be nearly the whole cost;
boxing the element read costs almost nothing. See
[the typepedia](typepedia.md) for the shapes and
[existentials.md](existentials.md) for why the one remaining cast
cannot be removed.

## 4. Chunks: the tree steps per batch

`Chunks[A] = Feed[Chunk[A]]` — a pure writer stream of array batches
(`Producer[Chunk[A]]` until producer-to-writer-carrier, 2026-09-19). The
freer tree steps once per CHUNK and an element costs an array index,
which is where the benchmark numbers come from — every lane below
chunked the way its own author intended (pipeline 8.2us for one chunk,
10.2 at the default 64, vs fs2 `emits` 21.9 / `ZStream.range` 35.8 /
kyo `Stream.range` 65.9, Iterator floor 15.2; merge 13.3us vs ZIO 51.5
and chunk-native fs2 94.4). Generators
(`Chunks.generate/range/nats/fibs`), transformers
(`Chunks.map/filter/take/drop/takeWhile/dropWhile` — chunk-in,
chunk-out array passes), `zip` (realigns boundaries), `rechunk`,
`fold`, `pipe`. `Pipeline` reifies a chunk pipeline as a typed
operator tree and `Pipeline.optimize` rewrites it (fusion, take
pushdown into sources) before compiling back onto the transformers.

When the pipeline's shape is known where it is written, `Staged` goes
the last mile — whole-stage codegen by inline partial evaluation:

```scala
Staged.fold(
  Staged.take(
    Staged.filter(Staged.map(Staged.range(0, 1000000), _ * 2), _ % 3 == 0),
    1000))(0L)(_ + _)
```

Nested calls beta-reduce into ONE while-loop with every lambda
inlined: 1.6us where `Iterator` takes 19.3 and the interpreted tree
15.9 (the standard map/filter/take/sum lane). The choice rule is the
library's usual one, one level up: the `Pipeline` TREE is for tools
(optimize, inspect, ship to another node), the INLINE SHAPE is for
speed — a GADT tree cannot partially evaluate through `inline match`
(pattern binding erases the inline-ness of subtrees), so the two
stay separate on purpose.

## 5. Coroutines: `Take`, `pipe`, `Stage`

`Take.await` is the dual of `Writer.tell`; `pipe(producer)(consumer)`
pairs them one element at a time — the consumer drives, a finite
consumer ends an infinite producer. `Take.each[I]` is the input as a
`Pull` — a source a direct block reads with `for i <- Take.each[I] do
…` (direct-loops v3), and `Pull.of(stream)` does the same for any
`Stream` carrier. `Stage[I, O, A]` is a transducer
as a program (awaits I, tells O); `through` composes stages
demand-driven. Tokenizers and parsers are stages (okay-lex,
okay-parse). If you know these as Kiselyov's **iteratees**: `Take`
is the iteratee, a `Writer` program the enumerator, `Stage` the
enumeratee, and `FoldUntil` the iteratee that is a fold with a stop
— [theory ch. 7](theory/07-logic-streams.md#iteratees-the-consumer-as-a-program)
has the paper and the side-by-side example.

`Stage.transduce(z)(step)(flush)` is the skeleton they all share —
carry a state, step it per input telling whatever that input is
worth, flush at the end. A stream with PHASES (a header before
rows) gets `Stage.phased`/`phased3`: the accumulator CHANGES TYPE
at the switch, the wrong-phase step does not compile, and the
transition runs through `PState` — typestate on the stream. The step ANSWERS the new state and is itself
a stage, so it may tell nothing, one, or many, and nothing is
allocated per element to say which; the lexer's scanner, SSE framing,
`chunked` and the demo's stream join are all this one call.
`Stage.mapAccumulate` is fs2's 1:1 special case on top of it, kept
for people who arrive with the name — and it is the special case, not
the primitive, because of the five stages written here NONE are
one-output-per-input: conditional emission has to say "nothing here"
with an `Option` that `transduce` never allocates.

**Typestate on a program: `Prog`.** `PState` puts a state's TYPE in a
continuation's answer; `Prog[F, A, S, R]` puts one on any program — the
same `Free` tree behind an opaque facade with two phantom indexes, "`S`
before, `R` after", so that `flatMap` joins a step ending at `R` only to
one starting at `R`. Nothing is allocated and nothing is matched:
`Prog.diag(p)` enters at any index, `p.free` leaves at the diagonal
(a move left open has no way out), and a protocol is written as smart
constructors that call `Prog.transition` once each and keep it
private. okay-sql's transaction is the first, over any `Sql`:

```scala
val tx = Tx(db)
val n = Tx.run(
  tx.begin().flatMap { g =>
    tx.update("insert into t values (1)").flatMap(_ => tx.commit()).map(_ => g.granted)
  }).runWith
```

`tx.begin().flatMap(_ => tx.begin())` does not compile — the
`IllegalStateException("nested transaction")` that `PgSql.begin` throws
is unrepresentable — nor does `Tx.run(tx.commit())`, nor a program that
ends inside a transaction, nor `tx.begin().free`. The same facade puts
`Delim`'s prompt stack in the type, so a `shift` to a prompt that is
not installed is a compile error rather than `NoPrompt`
([continuations in practice](continuations-in-practice.md#the-stack-in-the-type)).
One trap: write `import okay.Prog.{flatMap, map}` where you sequence
`Prog`s. Found only through the facade's companion, `flatMap` does not
infer the next step's index (`Required: Prog.Rep[Async, B, R, T]` with
`B` and `T` uninstantiated); imported, it does. (This was once blamed
on a package-level `Comonad[Id]` capturing `.map` — that instance now
lives in `Comonad`'s companion, and the import is still needed.)

> Atkey, *Parameterised notions of computation*, JFP 19(3–4), 2009,
> [doi:10.1017/S095679680900728X](https://doi.org/10.1017/S095679680900728X)
> — the parameterised monad `PState` and `Prog` are two instances of.

A stage whose STEP may end it is `Stage.transduceUntil(z)(step, end)`
(specs/fold-until.md, stage 3): the step answers `Left(next)` to go on
or `Right(r)` to stop, the stage ends there — so `through` pulls
nothing more from upstream — and `end` is the answer when the input
ends first. A header parser is the shape: read `k: v` lines, stop at
the blank one, and the body after it is never pulled:

```scala
val header: Stage[String, (String, String), Either[Int, Int]] =
  Stage.transduceUntil[String, (String, String), Int, Either[Int, Int]](0)((n, line) =>
    if line.isEmpty then pure(Right(Right(n)))          // the blank line: stop, n fields read
    else
      val Array(k, v) = line.split(": ", 2)
      Stage.tell[String, (String, String)]((k, v)).map(_ => Left(n + 1)),
    n => Left(n))                                         // the input ended first

Writer.run(through(lines("host: a", "port: 1", "", "body"))(header))   // (Seq((host,a), (port,1)), Right(2))
Writer.run(through(lines("host: a"))(header))                          // (Seq((host,a)), Left(1))
```

What `through` answers is a program like any other — a value. Nothing
runs until you run it, and running it twice runs it twice, each run
starting every stage from its first node: a stage that allocates a
buffer or a pane map on its first element (`Stage.chunked`,
`Windows.stage`, a JDK gatherer under `Gather.stage`) makes a fresh one
per run. It was not always so — `through` used to drive the stage to
its first output when it was *built*, and a second run of the built
program then fed that first run's state again, which lost a window
pane without a word (specs/stage-pipeline.md, Decisions). Hold the
built pipeline, run it as often as you like.

`transduce` is this with a step that never answers `Right`; the
server-driven UI session (`Wire.serveClosing`, okay-ui) is one — the
state is the server's own plus the tree it showed, `Right` on a Close
from either side. And the consumer end of `pipe` has the same door: `Take.foldUntil(using fo)`
is a `FoldUntil` as a consumer PROGRAM — an iteratee, written over
`!.loop` — so `pipe(producer)(Take.foldUntil(using fo))` is
`Writer.foldUntil(producer)(using fo)` by the coroutine road, pulling
exactly the same elements.

A stage is also a JDK **Gatherer** (JDK 24, [JEP 485](https://openjdk.org/jeps/485)) —
`stream.gather(g)`, the user-defined intermediate operation, whose
integrator may push any number of elements and answer `false` to end
the stream. That is a stage's `tell` and its answer, so okay-java's
`Gather` translates, both ways: `Gather.gatherer(stage)` runs a stage
inside a `java.util.stream` pipeline — a stage that answers stops
even an infinite stream — and `Gather.stage(g)` runs any JDK gatherer
(`Gatherers.windowFixed`, `scan`, `mapConcurrent`, a library's own)
inside an okay one:

```scala
// a stage that answers after three: the JDK's integrator returns false
val firstThree: Stage[Int, Int, Unit] =
  Stage.transduceUntil[Int, Int, Int, Unit](0)(
    (n, i) => Stage.tell[Int, Int](i * 10).map(_ => if n + 1 >= 3 then Right(()) else Left(n + 1)),
    _ => ())

val out = Stream.iterate(1, _ + 1).gather(Gather.gatherer(firstThree)).toList   // [10, 20, 30]

val windows = through(lines("a", "b", "c"))(Gather.stage(Gatherers.windowFixed[String](2)))
// Writer.run(windows) — (Seq([a, b], [c]), ()); run it again: the same
```

No combiner, deliberately: a suspended stage is a position in the
stream and two cannot be merged, and a combiner-less gatherer is
evaluated in encounter order even in a `.parallel()` stream. A
pipeline built with `through` is a VALUE: it starts the gatherer when
it is run, once per run, so running it twice windows twice. What is
refused by name is resuming a continuation from INSIDE a run after
that run finished — a JDK gatherer's state is an opaque mutable
object that cannot be snapshotted. `docs/modules/okay-java.md` has
`Windowed.gatherer`, the event-time window that hands each pane on
the moment the watermark closes it.

The same translation reaches **Clojure**: a gatherer's integrator and
finisher are a transducer's step and completion arities, and okay-clojure's
`Transducers.of(stage)` hands a stage to `into`, `sequence` or `comp`
as an ordinary transducer, while `Transducers.stage(xf)` runs
Clojure's own (`(partition-all 3)`, `(dedupe)`) in an okay pipeline —
and a Clojure program that needs okay's EFFECTS writes them with
`okay.core` (a thin program-as-data namespace in the jar, `mlet` in place
of `do`), walked by okay like Frege's `Prog` below —
docs/modules/okay-clojure.md.

And to **Frege**, a Haskell for the JVM, from the other end: there the
okay tree itself is written in Frege — `okay.frege.Prog`, with `await`,
`tell`, `perform` and `liftIO` for existing Frege IO — and okay-frege's
driver walks it, so a Frege `do` block becomes a `Stage` or an `A ! F`.
Its continuations are Frege functions, so `Choose` resumes them per
branch, and no thread is involved — docs/modules/okay-frege.md.

Stages may be EFFECTFUL: a row `Take % I + (Writer % O + G)` carries
arbitrary operations G (Async above all) between awaits and tells,
and the `through` overloads forward them through composition in the
order the pull crosses them — laziness intact, associativity intact.
A pure stage joins an effectful row by `!.widen` (plus a union-ACI
ascription); an SSE line stream through an event-framing stage is
this shape in production (okay-llm).

## 6. Concurrency: Loom first

`Async` has one blocking operation (`Run`) and one universal callback
operation (`Await`); on the JVM the handler parks a virtual thread —
blocking IS asynchrony. The Await callback carries an ERROR CHANNEL
(`Either[Throwable, A] => Unit` — a Left fails the program at that
operation, a failure is a value on the wire) and its registration
answers with a CANCELLER, so cancelling a fiber also unregisters the
timer or I/O completion it was parked on. The simple top-level
`await(k => ...)` keeps the success-only shape; `Async.await` is the
full form.

When the programs are independent, the instance can say so instead of
the plumbing: `Par` reads `A ! Async` as one leaf of an applicative
spine, so `Par.traverse`/`Par.sequence` — and any generic code over
`Applicative` — run their leaves at once, while `traverse` at the
program's own instance still sequences. `Par.map2` joins two leaves of
different types. It has no `flatMap` by design; `.map` is the
instance's own. For a FLAT sequence of same-typed programs on the JVM,
`parAll`/`parTraverse` (one fiber per leaf, joined in order) are
measurably cheaper; `Par` is for spines with leaves of different
types and for code that never heard of `Async`
(theory ch. 12, specs/applicative-static.md).

Blocking is evidence-gated (`CanBlock`, given on JVM/Native only): on
JS the SAME programs run through the event loop by
`Async.runAsync(prog): Future[A]`, and a blocking join is a compile
error, not a frozen loop. `Fiber` is onComplete/cancel everywhere
plus `joinAsync` (the effect-world join — itself an Await, good on
every platform); the parking `join`/`joinEither` exist only under the
evidence. `Scheduler` takes the PROGRAM — which is exactly what lets
the event loop be a scheduler (`Schedulers.loom` by default on the
JVM, forkJoin and plain threads for JVMs without Loom, one OS thread
per fiber on Native; the cats-effect and ZIO runtimes plug in as
Scheduler instances from the interop modules).

The combinators are cross-platform: `spawn`, `par` (pairs by
completion callbacks; EITHER side's failure fails the pair at once and
cancels the sibling — it watched only the left until par-fail-fast,
BUGS.md), `race` (first SUCCESS wins and cancels both; two failures
fail the race instead of hanging), `timeout`, `sleep` (an Await on
the platform `Timer` — a sleeping virtual thread, setTimeout, a
thread), `bracket`. One shared-source Await suite runs on the JVM,
under Node and as a linked Native binary in CI.

`Channel` is the queue between fibers — `merge` combines streams by
readiness, `buffer` runs a producer ahead. `source merge source` is that
merge in the program shape: two sources in, one source out (so a
stage consumes it directly), the fibers starting at the first pull —
and the two element types need NOT agree, because the result tells
their UNION, which the consumer splits by an ordinary type test.
Both derived merges are BOUNDED by default (64), because the channel
takes whatever is offered and an unbounded merge of an endless source
is unbounded memory — measured, ten pulls deep, 1.27M elements
produced against 74 at the bound, and the bound costs nothing the
benchmark can see. `Channel.merge` underneath keeps its unbounded
default: there the capacity is the caller's explicit business.
Chaining merges does not serialize them — each hop is its own fiber,
so eight chained sources overlap as eight (2.4s of parked work in
0.3s). What the program shape COSTS is measured and flat: ~300ns per
element, against ~130ns for the bare `Channel.merge` under it and
~11ns for a native `LazyList` walk — and flat is the operative word,
since a scaling sweep over 8x found the per-element price constant
(the Free tree is linear, `docs/theory/04-free-freer.md`). Most of
that gap is not the interpretation itself but the interpretation
INSIDE the contention: the same layer costs ~30ns per element alone
and ~160ns once two fibers race for the channel's cell. So where
throughput is the point rather than per-element semantics, merge the
CHUNKED streams — `Chunks.merge` is one queue operation per chunk
instead of per element, and measures 10.7us against 299.7us for the
same 2x500. For a source that is elementwise BY NATURE but consumed
in bulk, `merge`'s own `chunked = true` does the same trick
underneath and still hands back an ordinary `Source`: 2.6x at the
default capacity, 5.1x with `capacity = 1024`, since `capacity`
counts elements either way and the rest of the win is bought
explicitly with memory. It is off by default because chunking on its own has no
flush on time — on a slow or unending source an element waits for 15
others that may never come. `flushAfter = Some(millis)` bounds that
wait and makes chunking safe on a live source; it costs nothing when
it does not fire, and it never races the source's pull (cancelling an
in-flight `uncons` could lose an element), taking only what has
already accumulated. A timer is a guess, though, and a producer often
KNOWS where the boundary is — this token ended the model's turn, this
byte ended the frame. `Flush.now` says so as an OPERATION (not a
distinguished element, which would widen every element type and make
every consumer match on something that is not its data): a source in
the `Flushing` row emits it, and `a.mergeFlushing(b)` puts what that
side holds on the wire at exactly that point. It costs the ordinary
chunked path nothing — the two feeds are separate walks precisely
because routing both through the flushing one measured 11% dearer.

A merge does not have to buy a fiber per side. `Source.mergeReady`
(specs/ready-merge.md) keeps the ring of the SOURCES themselves and
steps them on the consumer's own thread of control: an element that
is ready is told out and its source goes to the back of the ring; a
source that is not ready yet — an `Async.Await`, which is exactly
"pending, and here is how to wake me", the shape Rust's `Poll::Pending`
plus `Waker` has — parks in its slot, and its callback wakes the
merge. What crosses threads is one index per wake-up, never an
element. No `Scheduler` is needed, so it runs on JS as it is, and
when no source ever waits the turns are a strict round-robin:

```scala
val merged = Source.mergeReady(Source.of(List(1, 2, 3)), Source.of(List(10, 20)))
val out = merged.runCollect.runWith                 // Vector(1, 10, 2, 20, 3)
```

The price is a single thread's: a source that COMPUTES before its
next element holds the others meanwhile. So parallelism is chosen
per SOURCE, not per merge — buffer the side that deserves a core, and
the merge reads it as one more source that is sometimes not ready:

```scala
val offCore = Channel.buffer(64)(LazyList.range(0, 1000)).drained
val both = offCore mergeReady Source.of(List(-1, -2))
```

`source merge source` is the case that buffers every side.

Chunking is a property of the STREAM, not a parameter of whatever
consumes it: `s.chunked(size)` gives `Source[Chunk[A]]` and
`.unchunked` gives the elements back, so `merge`, `buffer` and
anything else that crosses a channel gets batching without a flag of
its own — `s.chunked(8).buffer(4)` needs nothing added to `buffer`.
Reading a channel back is batched the same way and with no trade at
all: `c.drained` takes what is already buffered under one
transaction instead of one per element (2.4x on a buffered
producer), delaying nothing, since what is in the buffer is
already late.
`merge(chunked = true)` is the fused spelling of exactly that
composition, and it exists because the TIMED case would otherwise
need a second channel (a timer has to fire while the source is
silent); composing costs nothing where no timer is involved (222.3us
against the fused 223.7 on 2x2000). Keep the size modest — a stage
that accumulates without emitting is bounded by `PullBudget` but a
chunk in the thousands buys nothing here anyway. And where throughput
really is the point, the deeper answer is not to chunk a per-element
source at all but to start chunked: `Chunks.merge` never builds a
program node per element and measures 22.3us against ZIO's own
chunk-native default of 126.2 on the same 2x2000 — okay ahead by
5.7x on equal footing, not the 2.5x an earlier, unfair comparison
claimed (docs/benchmarks.md §6b). For a source that really is
elementwise (a live feed, arriving one token at a time, nothing to
pre-chunk), `Source.range` generates a half-open range with no
collection underneath. Forcing `ZStream` onto that footing
(`chunkSize = 1`) costs it 12x, but read that as what the forced mode
costs a library with no per-element representation rather than as a
scoreboard — nobody writes it. Reached through `ZStream.unfold`, where
the same chunk-of-one is ZIO's OWN mechanism and nobody's forcing, the
gap is 3x in okay's favour (docs/benchmarks.md §6c). On JVM/Native it parks
(bounded, backpressure by parking); JS gets the Await-based channel
behind the same surface (capacity advisory — a JS sender cannot
park). `parMap` maps a chunked stream with a fiber per chunk; `retry`
takes its policy as a STREAM of delays (`Retry.async` is the same as
an Async program, so JS has it too); `retryChunks` recomputes a
failed chunk from the stream's own program — the value is the
lineage, Spark-style. One level up, `Cluster.distribute` (okay-cluster)
rides the same fact across machines: workers behind one seam
(`Chunk[A] => Acc`; a dead worker throws), a failure hands the chunk
— still in hand, the source is a value — to a survivor, partials
merging by the Aggregator's combOp.

`type Blocking[A] = CanBlock ?=> A` names the trade as a
first-class VALUE: a returned `Blocking[A]` is storable and
composable, and only an edge holding the capability can force it.

## 7. Text is a stream: lex → parse → codec

The P5 stack is three small modules over the coroutine layer, all
TOTAL — errors are data in the result, never faults.

**okay-lex.** A lexer is a pure step function `Scan[K, S]`
(`step(s, c) => (S, tokens)`, `flush` finishes the tail): the state
is a VALUE, so it crosses chunk boundaries (`Scan.chunks` — a tight
while per chunk, a token spanning chunks is emitted once, where it
completes) and snapshots for free. `Scan.relex` resumes from the
nearest snapshot before an edit and RECONVERGES — past the edit and
the next newline, a state equal to the old run's state means the old
tokens are reused with shifted spans. Everything is a token, garbage
included (the Error channel); concatenated lexemes of all channels
equal the input.

**okay-parse.** Both parsing surfaces — a hand-written driver and
combinators — emit the ONE instruction language (`Open/Emit/Close/
Bad`), and the total builder folds any instruction stream into a
lossless CST: a Close with nothing open is an error leaf, open nodes
at the end close with an "unclosed" marker — a truncated stream (the
LLM case) is a tree with holes. Incremental reparse is the same
discipline one level up: `Parse.full` snapshots the persistent
builder at root-level node boundaries (a snapshot is a pointer), and
`Parse.reparse` relexes, resumes before the damage and SPLICES once
the token stream is the old one again — unchanged subtrees come back
BY REFERENCE for a length-preserving edit, with rebased spans
otherwise. The contract making token-level reconvergence sound: the
driver is a per-token function, no cross-token state — the stateful
part of parsing is the builder, and the builder is what snapshots.

**okay-codec.** Typeclass codecs are ALGEBRAS OVER A SCHEMA:
`Schema[A]` reifies a datatype's shape once (via Mirrors, thunked
fields for recursion), and every format folds it — `Json` renders
text, `Cbor` renders RFC 8949 binary, one derived Schema serving
both with equal semantic content. Dialects prove the model:
`Json.cst/render` is the byte-for-byte lossless layer; `Markdown`
proves REFRAMING (crossing emphasis `*a _b* c_` closes the inner
frames tokenless, closes the target with its token and reopens the
inner frames — well-nested, lossless, no faults). okay-llm rides the
same totality: a BPE tokenizer is just another `Scan`, and a cut-off
model answer still decodes because a tree with holes projects the
fields that are there.

## 8. The laziness contract

Programs are values: construction does no work (the opt-in Eager is
the sole, stated exception). An infinite program constructs in O(1);
`take(3)` computes three elements; re-observation repeats work rather
than caching (only the LazyList bridge memoizes). This contract is
load-bearing: it is what makes handlers stream transformers, chunk
retry a lineage recompute, and the whole lex/parse stack incremental
— and it is exactly where eager runtimes crash (see
compare/TestLaziness).

A `direct` block asks its carrier only for what the block uses: a run
of independent binds needs an `Applicative`, and only a bind that
mentions an earlier name needs a `Monad`. So `Validated` — which
refuses a monad on purpose — can be written in direct style and still
collect every error (specs/applicative-do.md).

A `direct` block emits one bind after another. `import
Direct.parallelBinds.given` changes that for INDEPENDENT ones: a
maximal run of consecutive `val x = m.reflect` binds whose right-hand
sides mention no earlier name in the run becomes N spawns then N
joins, the flat shape `parAll` uses and measurably the same. Without
the import the emission is unchanged to the byte
(specs/applicative-static.md, stage 3).

## 9. Capabilities: context functions

The stack's implicit evidence (`CanBlock`, `Scheduler`) generalizes:
`A ?=> B` is a capability arrow — returnable, storable,
self-applying where a given `A` is in scope. Three shipped routes:
`Traced.route` (a handler written against `using Tracer`),
`Secure.granted` (the principal ambient in a protected route), and
the ambient prompts of `Scope`/`Cut`. They COMPOSE: one stored
`(Principal, Tracer) ?=> Route` is protected and traced at every
installation site. `provide` is the installer half of
the pair — expression-scoped, nearest-wins:

```scala
provide(prodHttp, Secrets.env) { app }     // the edge
provide(stubHttp, testSecrets) { app }     // the test — same program
```

And installers COMPOSE as values — `providing[A](a)` holds one
`given` for later, `and` chains them flat (the right side wins on
overlap), so a base environment is built once and overridden per
use, with no nesting and no arity cap:

```scala
val base = providing[Http](prodHttp) and providing[Secrets](Secrets.env)
base { app }                                        // the edge
(base and providing[Http](stubHttp)) { app }        // override just Http
```

The consumer side is one line too: `wire[Http]` pulls the ambient
capability by naming its type — `val get: Http ?=> Response =
wire[Http].send(req)` is a door with no `summon` and no parameter.

A context function IS a pure Reader program, and the two spellings
convert: `Reader.lift(cf: E ?=> A): A ! Reader % E` asks once and
applies; `Reader.unlift(p): E ?=> A ! F` runs a Reader program under
the ambient `E`, forwarding the rest of the row. Functions, not
`Conversion`s — a context function auto-applies before a conversion
could see it (specs/context-functions.md, E10).

The payoff, on one page (executable: TestShowcase in okay-obs) —
ONE value whose needs are its type, living in two worlds without
changing a letter:

```scala
val api: (Principal, Tracer) ?=> Traced.Route = {
  case r if r.url.contains("/quote") =>
    okay.async {
      wire[Tracer].span("db.lookup") { () }
      Response(200, Nil, Http.one(s"for:${wire[Principal].name}".getBytes))
    }
}

// production: the doors install from the wire — a verified JWT
// becomes the Principal, a traceparent becomes the Tracer
Traced.route(tracer)(Secure.granted(verify, Policy.scoped("read"))(api))

// unit test: provide installs the SAME needs directly — no token,
// no HTTP machinery; a missing capability would not compile
provide(ada, tracer)(api)

// environments are values: one base, override one layer
(base and providing[Principal](bob)) { api }
```

Together the doors and `provide` are the DEPENDENCY-INJECTION
story: compile-time resolution (a missing dependency is a type
error, not a container exception), given-scopes as the object
graph, modules as ordinary values, zero framework. The rules that
keep it honest — the environment-vs-resource line, no newtypes for
strings, the eager-auto-application trap — plus the two-line recipe
for adding a door to any API are in [typepedia](typepedia.md); the
linear-context patterns and the experimental base in
specs/context-functions.md; the whole story, told in one place with
its theory and boundaries, is [capabilities](capabilities.md).

## 10. Optics: naming a path once

A nested `copy` names the path three times; a read chain and a write
chain of one path are two different expressions; and `case s => s` is
a promise re-made at every call site. An optic is that path written
down once, as a value you compose:

```scala
val city = Lens[Person](_.address).andThen(Prism.some).andThen(Lens[Address](_.city))
city.preview(p)                  // the read
city.modify(_.capitalize)(p)     // the write, same path, absent is a no-op
```

Composition takes the INTERSECTION of what each part needs — a lens
asks for `Strong`, a prism for `Choice`, and their composition is an
affine without anyone declaring it. The effects come in through one
slot: `traverseOf` asks for an `Applicative` and nothing more, so the
same optic walks at `Validated` (every error), `Par` (at once) and
`Static` (what it WOULD do).

Price, in one line: name the path in code and the compiler emits the
update a person would write, allocation identical to the byte; choose
the optic at run time and you are paying a small interpreter. The
pairs, the numbers and the case where a `copy` still wins are in
[optics.md](optics.md); the theory is [ch. 10](theory/10-optics.md).

When the program is a WALK rather than an edit — into a node, next,
edit, back out — the path an optic recomputes each time is a cursor's
state, and that cursor is `Zipper[T]` over a `Plate[T]` (how the tree
exposes its children; `Ui` has one in scope, and `Json`'s is
`import okay.codec.JsonOptic.plate`):

```scala
Zipper(doc).first.flatMap(_.right).map(_.set(JStr("grace")).root)   // into, next, edit, fold in
State.zoom(Zipper.focus)(prog)        // a State % T program run AT the focus
Zipper.at[Ui](path)                   // the path back as an affine — Ui.path
```

`TypedZipper(order).down(customer).down(address)` is the same cursor
with its position as a TYPE — the focus is an `Address`, `set` takes
one, `up` gives the `Customer` cursor back, `field("city")` is the
Mirror lens as a frame, `at(i)`/`downCase[B]` are the partial moves —
for a program written against a part that must run at that part;
`c.asAffine` is the walk back as an optic, `c.pathKey` the same walk
as the form's dotted key (`Form.drillAt(c)` opens a drill-down there),
`at(i)` on a `Vector` focus
gives an element frame with `left`/`right` among its siblings, and
`TypedZipper.Poly` is the cursor whose `set` changes the whole's TYPE
with the focus's.

`JsonEditor(json)(done)` in okay-ui is the product form: a `Screen`
over a `Zipper[Json]` with move/edit/add/delete buttons and the focus
marked in an outline. Theory and the Huet/McBride references:
[ch. 10, "The zipper"](theory/10-optics.md#the-zipper-the-residual-carried).

## Direct style, in one paragraph

Any monad in this library can be written as plain code:
`direct[F] { val x = m.reflect; ... }` compiles the block into the
reflect/reify chain of `Monadic` (Filinski's construction over the
`Cont` of chapter one), so short-circuit, multi-shot and handlers
all behave exactly as in the monadic spelling. Effects are
first-class (`Writer("a")` on its own line tells; loops and `while`
work; `!prog` performs a program in one glyph and involves no implicit
conversion), auto-coloring can remove marks entirely behind explicit
gates, a block may recurse on its own def a million deep because a
self-call is deferred into the tree, and every refusal is a
positioned compile error naming the workaround. Two more doors on the
same block: `Direct.staged(stager) { … }` compiles each operation to
its handler's arm when the handlers are known at the call site
(`Stager.All` for Reader/State/Writer/Throws; 2.24x–2.56x over the
Free block, parity with the hand-written program), and
`generator[W] { … }` makes `for … yield`, `Gen.emit` and `Gen.stop`
a Python-style generator read lazily through `Gen` (§3). The whole
story, with the reasoning and the graveyard of refuted alternatives:
[direct-style.md](direct-style.md); the theory with the literature:
[theory ch. 8](theory/08-direct-style.md).

## With other JVM languages, in one paragraph

A `Stage` is a JDK gatherer (okay-java's `Gather`) and a Clojure
transducer (okay-clojure's `Transducers`), both ways; okay's effects
reach Clojure and Frege as a small program-as-data library written in
that language — `okay.core`, `okay.frege.Prog` — which okay walks, so a
multi-shot handler resumes their continuations and no thread is
involved; and lazy data (a Clojure seq, a Frege list, okay `Chunks`)
crosses both ways, pure by type. [okay with other JVM
languages](jvm-languages.md) is the guide.

## From Scala 2.13, in one paragraph

A Scala 2.13 build cannot use `A ! F` directly. Two things stand in
the way. The row is a union type, which Scala 2 cannot spell. And the
combinators are `inline`, which Scala 2's TASTy reader refuses to call.
`okay-scala2` gives such a build `Prog[A]`: a program over
`Async + Throws % Throwable` with plain `map`/`flatMap`, so a 2.13
for-comprehension works over it. A throw inside `Prog.delay` is a
failure that `attempt`, `recover` and `runEither()` see, and `run()`
executes the program on the calling thread. For several effects in one program there is `Eff[-R, A]`. Its row is
an intersection that Scala 2 can write (`+` is a one-line alias for
`with`), `A ! (State[Int] + Writer[String])`, and it is taken apart one
handler at a time (`State.run`, `Writer.run`, ...). Continuations are
`okay.scala2.Cont`, with `shift`, `reset` and answer-type modification.
A 2.13 user's own effect is plain Scala 2: its operations extend
`Op`, and `object Console extends Effect[Console]` declares it. Its
handler receives each operation and the continuation, and can resume
it once, never, or several times. Streams are `okay.scala2.Source`: the core's own `Source`, with the
usual vocabulary (`map`, `filter`, `take`, `merge`, `runCollect`). A
source can also be written as a for-comprehension of `Writer.tell` and
`Async(a)` and wrapped with `Source.fromEff`. Concurrency is `Async.fork`, `par`, `race` and `timeout`, together with
a bounded `Channel` whose `send` and `receive` are programs that wait.
Above that, sixteen `okay-scala2-*` modules cover the rest of the
library the same way, from codecs, HTTP and SQL to durable workflows,
optics and the service libraries. All of it is in the one package
`okay.scala2`, under the library's own names. The build needs both standard libraries, 2.13's first and 3.9's
behind it. The walkthrough is [okay from Scala 2.13](scala2.md), the
reference is [modules/okay-scala2.md](modules/okay-scala2.md), and why
the row becomes an intersection is
[theory ch. 13](theory/13-rows-without-unions.md).
