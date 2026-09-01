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
val ids: Seq[Int] = for u <- users yield lookup(u).?   // map: traverse
while retry.? do backoff()                        // while: effectful cond
```

## Behavior

- [ ] `for x <- xs do eff(x).?` (and bare-statement bodies, layer-4
  style) runs the effect once per element, in order, effects
  sequenced left to right
- [ ] `for x <- xs yield eff(x).?` collects results in order — the
  traverse shape; the result is emitted as List and accepted where
  the original type is List/Seq; other collection shapes are a v1
  refusal naming the workaround
- [ ] a marked receiver hoists first: `mkList().?.foreach(...)` binds
  the receiver before the loop
- [ ] `while cond do body` with marks in cond and/or body: cond
  re-evaluates per iteration; the loop is emitted as a recursive
  Cont, stack-safe through Bind spill
- [ ] multi-shot safety: the emitted loops recurse over an IMMUTABLE
  materialized List, never a live iterator — a List-monad reflect
  inside a loop body re-runs the rest of the loop per element
  without exhausted-iterator corruption
- [ ] nested loops work (the desugaring nests lambdas; the rewrite
  recurses)
- [ ] a lambda that is NOT a whitelisted combinator argument keeps
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

(fill as they are made)

## Results

(fill after verify)
