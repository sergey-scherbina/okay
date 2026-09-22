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
