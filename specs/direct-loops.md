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
  Cont, stack-safe through Bind spill
- [x] multi-shot safety: the emitted loops recurse over an IMMUTABLE
  materialized List, never a live iterator — a List-monad reflect
  inside a loop body re-runs the rest of the loop per element
  without exhausted-iterator corruption
- [x] nested loops work (the desugaring nests lambdas; the rewrite
  recurses)
- [x] a lambda that is NOT a whitelisted combinator argument keeps
  the v1 refusal, message unchanged

## Out of scope

- `flatMap`/`withFilter` in multi-generator for-comprehensions over
  collections — nested single-generator loops express the same
  programs; add shapes when a consumer names them.
- `exists`/`find`/`fold` and other HOFs — same rule: a consumer
  first.
- Non-Seq-convertible yield results (Vector-typed, Map-typed) —
  refused with the workaround (`.toList` the receiver or collect
  manually) until a consumer names the shape.

## Design

- Interception happens in compileMarked BEFORE the lambda refusal:
  the shapes `Apply(TypeApply(Select(xs, "foreach"|"map"), _),
  List(Lambda(param, body)))` with `hasMark(body)`, receiver
  `<:< IterableOnce`.
- foreach emits: materialize `xs.iterator.toList` once, then
  `def loop(rest: List[T]): Cont[Unit, F[A], F[A]] = rest match
  { case Nil => Pure(()); case h :: tl => bodyCont(h).flatMap(_ =>
  loop(tl)) }` — recursion through flatMap rides Cont's Bind spill
  (stack-safe), and the immutable List is what makes multi-shot
  re-entry sound.
- map emits the same loop with an accumulator, `acc.reverse` at Nil.
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
  the workaround named, until a consumer names the shape.

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
