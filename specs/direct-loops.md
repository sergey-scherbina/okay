# direct-loops — effectful iteration in direct blocks

## Overview

The 2026-09-01 codebase survey (docs/direct-style.md) found that the
most common REAL monadic pattern is not a linear chain but an effect
per element: ChatDemo's recursive `seed`/`go`, every protocol's
send-each loop. In direct blocks these were v1 refusals — the body
is a lambda (`foreach`), or a `while`. This task closes them by
WHITELISTED-COMBINATOR coloring: the macro rewrites the specific
shapes `xs.foreach(x => body)` (= `for x <- xs do body`),
`xs.map(x => body)` (= `for x <- xs yield body`) and
`while cond do body` when marks appear inside — dotty-cps-async's
"shifted functions" idea, at the scoped price this macro always
pays: named shapes, everything else keeps the clear refusal.

## Interface

No new API. Inside `direct[F] { ... }`:

```scala
for t <- reply.split(' ') do Writer(t + " ")     // foreach: each tells
val ids: Seq[Int] = for u <- users yield lookup(u).!?   // map: traverse
while retry.!? do backoff()                        // while: effectful cond
```

## Behavior

- [x] `for x <- xs do eff(x).!?` (and bare-statement bodies, layer-4
  style) runs the effect once per element, in order, effects
  sequenced left to right
- [x] `for x <- xs yield eff(x).!?` collects results in order — the
  traverse shape; the result is emitted as List and accepted where
  the original type is List/Seq; other collection shapes are a v1
  refusal naming the workaround
- [x] a marked receiver hoists first: `mkList().!?.foreach(...)` binds
  the receiver before the loop
- [x] `while cond do body` with marks in cond and/or body: cond
  re-evaluates per iteration; the loop is emitted as a recursive
  def over F's own flatMap (direct-flatmap-emission retargeted it
  from Cont, 2026-09-02) — a lazy F (Free) defers the recursive
  call inside its flatMap, so the loop inherits F's stack
  discipline exactly as a hand-written one would
- [x] multi-shot safety: the emitted loops recurse over an IMMUTABLE
  materialized List, never a live iterator — a List-monad reflect
  inside a loop body re-runs the rest of the loop per element
  without exhausted-iterator corruption
- [x] nested loops work (the desugaring nests lambdas; the rewrite
  recurses)
- [x] a lambda that is NOT a whitelisted combinator argument keeps
  the v1 refusal, message unchanged

## v2 — guards, several generators, the other combinators, the other collections (direct-loops-v2, 2026-09-22)

The consumer named the shapes (operator, 2026-09-22): "многогенераторные,
HOF и yield". Every loop keeps v1's shape — an immutable LazyList, a
recursive def, the body compiled per element against the loop's own
tail — and gains one thing each:

- [x] GUARDS: `for x <- xs if p(x) do/yield …` (`xs.withFilter(x => p)`,
      peeled off the receiver, chained for several) — the guard runs
      per element in source order, with marks allowed in it (a marked
      guard binds before the body), and the body runs only when every
      guard holds.
- [x] SEVERAL GENERATORS: `for x <- xs; y <- ys yield f` (the outer
      `flatMap`, the inner `map`) — results in the comprehension's own
      order, a guard between generators honoured, and a short-circuit
      inside the inner generator ends the whole comprehension (the
      Option law: nothing after the `None` runs).
- [x] HOFs with a marked lambda: `exists`/`forall` stop at the first
      element that decides; `find` answers the first match and stops;
      `filter` keeps the matches; `foldLeft(z)(f)` threads the
      accumulator, a marked `z` binding first. Each is a loop of the
      same family; the log proves where the walk stopped.
- [x] YIELD SHAPES: a for-yield answers the node's own collection type
      — List/Seq/Iterable as is, Vector/IndexedSeq, Set, Map (of
      pairs) — and anything else is refused naming the workaround.
- [x] A marked RECEIVER under guards hoists first, as in v1.
- [x] v1's suite unchanged in what it asserts, except the four tests
      that used `filter`/`exists` as the canonical refused lambda:
      they assert the refusal on `sortBy`/`count` now, and the `lazy
      val` demand under `filter` is a positive test (the cell forced
      once). okay-direct: 340 green (`TestDirectLoops2`, 12).

## Out of scope

- `collect` with a partial function, `zip`/`zipWithIndex`, `groupBy`,
  `sortBy` and every other HOF — a consumer first, as before.
- Lazy yield targets (`LazyList`, `Iterator`, a stream): a strict
  traverse would force them; the generator lane (specs/generators.md)
  is the lazy road.
- Guards on `while`, and pattern generators that are refutable
  (`for Some(x) <- xs` is a `withFilter` on a partial function and
  keeps v1's refusal).

## Design

- Interception happens in compileMarked BEFORE the lambda refusal:
  the shapes `Apply(TypeApply(Select(xs, "foreach"|"map"), _),
  List(Lambda(param, body)))` with `hasMark(body)`, receiver
  `<:< IterableOnce`.
- foreach emits: take `xs.iterator.to(LazyList)` once, then
  `def loop(rest: LazyList[T]): F[Unit] = rest match
  { case h #:: tl => M.flatMap(bodyF(h))(_ => loop(tl)); case _ =>
  M.pure(()) }` (the Cont-typed original retargeted by
  direct-flatmap-emission) — recursion through flatMap inherits F's
  stack discipline, and the immutable, memoized LazyList is what makes
  multi-shot re-entry sound. Lazy (audit-fixes, 2026-09-02; was
  `.toList`): an unbounded receiver — `LazyList.from(1)`, an
  iterator over a stream — is forced only as far as the monad
  drives the loop, so `for i <- LazyList.from(1) do
  (if i < 5 then Some(()) else None).reflect` stops at 5 under
  Option instead of hanging at materialization.
- map emits the same loop with an accumulator, `acc.reverse` at the end.
- while emits `def loop(): Cont[Unit, F[A], F[A]] =
  condCont.flatMap(c => if c then bodyCont.flatMap(_ => loop())
  else Pure(()))` — the spliced cond/body terms sit inside the def
  body, so they re-evaluate per iteration by construction.
- The lambda body compiles with the parameter substituted by the
  emitted binder (the subst machinery vals already use); owners are
  corrected at the splice as everywhere else.

## Decisions

- **Whitelist, not general HOF coloring** — foreach and map are the
  shapes the codebase survey actually found; each further shape
  (exists/fold/flatMap-comprehensions) waits for a consumer. The
  refusal message for everything else is unchanged.
- **Materialize, then recurse immutably** — the emitted loops walk
  a List built once from `.iterator` (built by NAME, so ArrayOps
  receivers — the `split(' ')` case — serve alongside IterableOnce);
  a live iterator would be corrupted by multi-shot re-entry.
- **Assign joined the rewrite** — `sum += eff().!? * i` appeared in
  the first loop test and was refused; an Assign with a marked rhs
  now binds then assigns. Loops made effectful assignment
  unavoidable one test in.
- **for-yield emits List** — accepted where the node's type can hold
  it (List/Seq); Vector/Map-typed comprehensions are refused with
  the workaround named, until a consumer names the shape. (v2: the
  consumer named it; the List is still what the loop accumulates,
  converted once at the end to the node's Vector/Set/Map.)
- **v2: every step is built by reflection, not by a nested quote** —
  chosen because a quote nested in a splice may name the outer
  quote's symbols but not carry a type tree of the pattern-bound
  element type (`(b: u) => …`, `x :: acc`, `Some(h)`): the pickler
  reports "unresolved symbols: given instance u$given", three times
  in one lane. `bind` (DirectEmit's own quote), `consTo`,
  `Apply(loopFn, …)` from the parts of a harmless `loop(tl, acc)`
  quote — and the rule is written at the top of the v2 section of
  DirectLoops.scala.

- [x] loop and while BODIES carry statement semantics (fixed
  2026-09-01, found by the ChatDemo migration): a bare op as the
  body — `for t <- xs do Writer(t)`, `while c do Writer(x)` — RUNS;
  and a fully MARKLESS block whose loop body is the block's own
  effectful type is intercepted too (the interception used to gate
  on hasMark, so such loops built and dropped each op natively)

## Results

- 8 new tests in TestDirect (35 total across the two suites): for-do
  in order, Array receiver, mid-loop None short-circuit (the loop
  STOPS — 2 hits of 3), for-yield traverse, while with effectful
  condition (re-evaluated per iteration, 4 evaluations observed),
  multi-shot re-entry into a loop body (2x2 continuations, immutable
  iteration state), nested loops in row-major order, and the
  non-whitelisted refusal (`exists`) intact.
- The ChatDemo migration immediately found the two holes above —
  the worked example doing its job as a test bed. Also found there:
  `.?` is AMBIGUOUS in scopes where okay's Throws machinery is
  imported (it has its own postfix `?` via the throws Conversion);
  `.reflect` is the collision-free spelling and the demo uses it —
  recorded in specs/direct-macro.md Decisions.
- Two v1 tests were retired BY the feature: the lambda-refusal
  example had used `map` (now a feature — moved to `filter`), and
  the while-refusal test asserted an error that no longer exists.

## v3 — a loop over a SOURCE (direct-for-over-source, 2026-09-23)

### Overview

v1 and v2 iterate COLLECTIONS: the receiver has an `iterator`, the
loop is a recursive def over an immutable `LazyList` of it, and a
generator (specs/generators.md) rides the same road through its
stepping reader. What none of them can read is a source with no
iterator — one whose next element is a PROGRAM: a `Stream[S, G]`
carrier (`uncons` answers in `G`), a writer program's told values
under another effect, or the `Take` side of a `Stage` (`await` is an
operation). Today such a loop is written by hand as `!.loop` over
`uncons`/`await` — theory ch. 7 says so in its last paragraph — and
this stage is the `for` that replaces it.

### Interface

Core, `Pull.scala`:

```scala
/** a source a program reads one step at a time */
trait Pull[A, G[+_]]:                       // invariant: Free is invariant in its answer
  def step: Option[(A, Pull[A, G])] ! G
  def withFilter(p: A => Boolean): Pull[A, G] // what a guard desugars to
  /** the loop with a pure body AS A PROGRAM — outside a block, by name */
  def loop(f: A => Unit): Unit ! G
object Pull:
  def of[S[_], A, G[+_]](s: S[A])(using Stream[S, G]): Pull[A, G]
  def told[W, A](a: A ! Writer % W): Pull[W, Pure]                       // first-order, as Stream.scala's overloads
  def toldIn[W, G[+_]: TypeableK, A](a: A ! Writer % W + G): Pull[W, G]
```

okay-stream, `Pipe.scala`: `Take.each[I]: Pull[I, Take % I]` — the
consumer side of an iteratee as a source.

okay-direct, `Direct`: `extension [A, G[+_]](src: Pull[A, G]) def
foreach[F[_]](f: A => Unit)(using DirectCtx[F]): Unit` — the `for`'s
`foreach`, typed Unit and present ONLY where a block's ambient
`DirectCtx` is (Decisions: the lint). Inside a `direct` block, `for x
<- src do body` where `src: Pull[A, G]` — with or without marks in
`body` — is emitted as a program: `loop(p) = bind(p.step) { case Some((h, tl)) => body(h);
loop(tl); case None => pure(()) }`, the step bound through the same
row lift a mark takes (`G` must be in the block's row, and the
refusal is the mark's), the body compiled against the recursive call
as its tail like every v2 loop, guards honoured.

### Behavior (`TestDirectSource`, okay-direct)

- [x] `Pull.of(LazyList(1, 2, 3)).loop(f)` is a program: nothing
      runs until `!.run`, then `f` sees 1, 2, 3 in order.
- [x] `for x <- Pull.of(xs) do say(x).!?` in a block: the body's
      effects once per element, in order.
- [x] a source with effects of its own: `Pull.toldIn(producer)` whose
      producer performs `Async` between tells, read in a
      `direct[Async + State % Int]` block whose body performs `State` —
      the producer's effects and the body's interleave per element.
- [x] `Take.each`: `Take.each[Int].loop(f)` piped to a producer of
      three sees 2, 4, 6 and the end; its `step` reads three of an
      infinite producer and no more. (The direct-block form over
      `Take` is the same macro road as over `Pull.of`, tested in
      okay-direct; okay-direct and okay-stream do not depend on each
      other, so the block-over-`Take` spelling is asserted by type in
      `TestTakeEach`'s step test and by the macro tests, not by one
      test holding both.)
- [x] a guard: `for x <- src if x % 2 == 0 do …` skips without
      consuming a bind.
- [x] no marks in the body: the loop still runs as a program (a
      `Unit ! G` bare statement of a NARROWER row would not run by
      itself, so the source road fires on the receiver's type, not on
      marks).
- [x] `for x <- src yield …` over a `Pull` does not typecheck (`Pull`
      has no `map`), and the message is the compiler's.
- [x] a `Pull` whose `G` is not in the block's row is refused with the
      row-lift message, at the loop.

### Decisions (v3)

- **An explicit `Pull`, not a silent road for every Stream carrier** —
  `s.foreach(f)` on a `Stream` carrier already MEANS "run through the
  Handler here" (Stream.scala:241); changing what it means inside a
  block would fork one spelling into two semantics. `Pull.of(s)` says
  "read this as a program", and outside a block it IS one. Rejected:
  summoning `Stream[S, G]` from the receiver in the macro (also
  impossible for the writer carriers — their instance is a type
  lambda inference does not reach, which is why `told`/`toldIn` are
  first-order overloads).
- **The road fires on the receiver's TYPE, not on marks** — the v2
  loops fire only when something is marked, because an unmarked
  collection loop is plain Scala and correct as written; an unmarked
  `Pull` loop is a `Unit ! G` in statement position, which does not
  run bare unless `G` is the whole row. Rejected: marks-only.
- **`do` only** — `map`/`flatMap` over a source is a transformation
  of the stream, which `Stream`'s combinators are for; a `Pull` has
  no `map`, so the comprehension does not parse to one.

### Results (v3)

2026-09-23, one lane. `TestDirectSource` (okay-direct, 7) and
`TestTakeEach` (okay-stream, 2), green through `scripts/gate.sh`, no
warnings. THE DESIGN CHANGED ONCE AGAINST THE CODE, and the code was
right: `Pull.foreach: Unit ! G` as a member made `for x <- src do …`
in statement position a discarded PROGRAM, which build.sbt's lint
escalates to an error at typer — before any macro runs — exactly as
it refuses a bare `Writer.tell`. The loop's `foreach` is therefore
`Direct`'s extension needing the ambient `DirectCtx`: typed Unit
inside a block, absent outside, and the program form is `loop(f)` by
name. `HofCall` learned the extension-call shape (receiver, lambda,
then the using clause). The type pattern `'[type g[+x]; Pull[t, g]]`
did not match a pure source (`G = Pure = Nothing` is not a
higher-kinded argument); the loop takes the receiver's type whole and
selects `step` by name. `Pull` is invariant because `Free` is. The
interleaving law reads the producer's `State` cell inside the body:
"got 1 after 0 steps, got 2 after 1, got 3 after 2" — a step runs the
producer up to its next tell and no further.
