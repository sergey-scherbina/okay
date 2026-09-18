# `direct` at an applicative-only carrier

## Overview

The applicative arc (specs/applicative-static.md) argued that the rung
below the monad is worth standing on, and then left the sugar behind:
`direct[F] { ... }` takes `using M: Monad[F]`, so a carrier that has
no monad cannot be written in direct style at all. Probed 2026-09-18,
a `direct` block at `Validated` is refused before the macro runs:

    no Monad[V].

That is awkward in a specific way. `Validated` has no `Monad`
**by design** — the consistency law would force `app` to agree with
the `flatMap` derivation, which stops at the first error, undoing the
one thing the type exists for (specs/validated.md). So the types where
direct style would read best are exactly the ones it refuses.

The fix is not a second entry point. It is to ask the carrier for what
the BLOCK actually uses: a run of independent binds needs only
`Applicative`, and only a bind whose right-hand side mentions an
earlier name needs `Monad`. That is ApplicativeDo
\[Marlow et al. 2016\], and the analysis it needs already exists here
— `parallelBinds` (applicative-static stage 3) finds independent runs
today, it just emits `Async.spawn` rather than `app`.

## Interface

```scala
final class DirectApply[F[_]]:
  inline def apply[A](inline block: DirectCtx[F] ?=> A)
                     (using inline A: Applicative[F], inline d: Deferral,
                      inline b: Binds): F[A]
```

One change: `Monad[F]` becomes `Applicative[F]`. Every existing call
site keeps working untouched, because `Monad extends Selective extends
Applicative`, so a `Monad` in scope satisfies the new parameter.

`Monad[F]` is then summoned INSIDE the macro, at the point a bind is
emitted, and only there. When it is absent and the block needs one,
the refusal names the bind that forced it:

    direct: `val y = ...` uses `x`, which this block binds, so it needs
    Monad[V] and there is none. An applicative can run independent
    binds only — reorder, or make the carrier a monad.

## Behavior

- [x] Every existing direct test passes untouched, and the emitted
      tree is unchanged where a `Monad` is available. This is the
      first item because it is the risk: the parameter of a macro used
      across the repository is changing.
- [x] A block at `Validated` compiles and COLLECTS: two independent
      invalid binds report both errors, which is the whole point and
      is impossible through `flatMap`.
- [x] A dependent bind at an applicative-only carrier is a compile
      error naming the offending val, not a confusing "no Monad" at
      the call site.
- [x] The emission at an applicative is the idiom bracket:
      `fmap(m1, a1 => a2 => … body).app(m2)…app(mN)` for a run of N,
      asserted on the ANSWER and on the order a recording carrier
      sees, not inferred.
- [x] A carrier with BOTH instances keeps the monadic emission. The
      reason is measured and already recorded: `Monad.app` is
      `f.flatMap(g => fmap(a, g))`, so emitting `app` for `A ! F`
      builds the same binds plus a node (applicative-static stage 3
      Design). Nothing regresses for programs.
- [x] A block with no marks at all is unchanged at either carrier.
- [ ] Cost, predicted before measuring: the existing direct lanes move
      by less than 2% and not at all in bytes, because the emitted
      tree for a monadic carrier is the same tree. If bytes move, the
      refactor changed something it should not have.

## Out of scope

- Choosing the applicative emission when a `Monad` IS available. That
  is what `parallelBinds` does for `Async`, and doing it generically
  needs a way to say "this carrier's `app` is not derived from its
  monad", which the compiler cannot see. Named here so the next reader
  does not look for it.
- `Selective` emission for `if` inside a direct block. The rung is
  real and `ifS` exists, but the macro's conditional handling is its
  own body of work (`direct-macro`'s `compileMarked`), and nothing
  asks for it yet.
- Changing `Validated` in any way. It is the consumer, not the
  subject.

## Design

**Why one entry point and not `directAp`.** A second name would have
to be chosen by the author before they know whether their block is
independent, which is the question the macro answers. Degrading the
requirement puts the decision where the information is. It also keeps
`Deferral` and `Binds` from having to exist twice.

**Why the `Monad` moves inside the macro.** A `using` parameter is
resolved by the typer before expansion, so a missing `Monad` is a call
site error about a type the author may never have mentioned. Summoning
it where the bind is emitted lets the refusal quote the line that
needs it. `Expr.summon` searches the splice site's implicit scope,
which is the same scope the parameter would have used.

**What the analysis reuses.** `independentRun` and `mentionsAny`
already exist for `parallelBinds`; they are the same question
(does this right-hand side mention a name bound earlier in the run)
and they move up beside `compileBlock` rather than being written
twice.

## Decisions

- **`Applicative[F]` in the signature, `Monad[F]` summoned in the
  macro** — chosen so existing call sites do not move and the refusal
  can name the line. Rejected: a second entry point (`directAp`), and
  an overload pair (ambiguous for carriers that have both).
- **Applicative emission only when there is no `Monad`** — chosen
  because emitting `app` at a monadic carrier is a measured loss and
  a silent behaviour change. Rejected: always emitting `app` for
  independent runs (costs a node per join for every program in the
  repository).

## Results

Stage 0 (this spec): written 2026-09-18, out of the operator's
question "what about direct style for applicatives", which the arc
had left open.

### Landed 2026-09-18

**The split is by whether a `Monad` EXISTS, not by a flag.** The entry
asks for `Applicative[F]`; the macro summons `Monad[F]` and, finding
one, runs the existing pipeline untouched — so no program in this
repository changed emission, which was the first Behavior item and the
real risk. Finding none, it takes a separate, small road that emits
the bracket and refuses everything else BY NAME.

**MARKS IN THE RESULT HAD TO BE LEAVES TOO.** The first cut refused
them, and `direct[V](check(2).reflect + 1)` — the bracket's most
natural spelling — did not compile. Each mark in the result is now
replaced by a reference to a fresh name, in evaluation order, and the
names join the run. They are independent by construction: separate
subexpressions of one expression cannot mention each other's answers.

**Three things the reflection API decided rather than the design**,
all caught by a failing splice rather than by reading:

- the curried lambda must be BUILT at the type `fmap` will ask for.
  Leaving the result at `Any` was refused with "Expected
  `Int => Int => Any`, Actual `Int => Any`": a function type is not
  inferred from a nested lambda's body after the fact.
- `fmap[X, R]` takes R as the function's RESULT, not the function.
  Passing the whole type was off by one, and the splice printed both
  types side by side.
- the carrier's element is its LAST type argument. Reading the only
  one refused every two-parameter carrier — which is to say
  `Validated[E, A]`, the consumer this lane exists for — with
  "expected the carrier applied".

**What v1 refuses**, each with a message naming what it found rather
than a type error about a class the author never mentioned: a
dependent bind, a statement that is not a marked val, and a mark
inside another mark. The refusal reads "this block's carrier has an
Applicative but no Monad, so it can run INDEPENDENT binds and nothing
else — `b`, whose right-hand side uses a name this block binds, needs
flatMap."