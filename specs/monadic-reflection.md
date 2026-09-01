# Monadic reflection — direct style for any monad, no macros

## Overview

Filinski's construction ("Representing Monads", POPL 1994): given
delimited control, ANY monad runs in direct style — `reflect` delivers
the `A` of an `F[A]` as a plain value, `reify` delimits a block back
into `F`. The Cont paramonad types the construction precisely by
answer-type modification: a reflected `F[A]` is `Cont[A, F[B], F[B]]`
— "A now, F[B] eventually". This is direct style *relative to Cont*:
inside one for-comprehension over Cont, values of any foreign monad
are ordinary values; the monadic plumbing happens once, at the
`reify` delimiter. No macros, no CPS rewriting, no platform limits —
it is two one-liners over `shift` and `/`.

## Interface

```scala
object Monadic:
  /** μ: the monadic value as a direct value */
  inline def reflect[F[_] : Monad, A, B](m: F[A]): Cont[A, F[B], F[B]] =
    shift(k => m.flatMap(k))

  /** the delimiter: a direct-style block back into its monad */
  inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
    p / (a => M.pure(a))
```

`Monad[F]` is okay's own (Monad.scala). The names are Filinski's; they
live inside `object Monadic` because package-level `reflect`/`reify`
are already taken by the Effects encoding round-trip (Effects.scala) —
a different construction that happens to deserve the same words.

## Behavior

- [ ] `reify(reflect(m)) == m` — the round trip is the identity, for
  every monad tested (Option, Either, List, Eff)
- [ ] short-circuit: reflecting a `None`/`Left` aborts the rest of the
  block — the continuation is dropped, exactly `flatMap`'s semantics
- [ ] multi-shot: reflecting a `List` re-runs the continuation once
  per element — two reflects make the cartesian product; multi-shot
  is PRESERVED, not forfeited (see Decisions)
- [ ] the reflected values mix with plain code: `if`/`val`/arithmetic
  between reflects behave as in direct style
- [ ] stack safety: a fold of 100_000 reflected binds runs — Cont's
  Bind spill carries reflection like any other chain
- [ ] okay's own monads reflect too: an `Eff[F, A]` program via the
  `Monad[[A] =>> Eff[F, A]]` instance

## Out of scope

- **Surface syntax without for-comprehension** (`val x = m.!` in a
  plain block) — that is the macro road; feasible scoped (see the
  corrected Decision in specs/context-functions.md), not this task.
- **A Control[M]-generic version** — `reflect`/`reify` only need
  `shift` and `/`, so the tagless generalization is mechanical; do it
  when a second carrier wants it, not before.
- **Answer-type-modifying reflection** (F changing along the block) —
  Kobori–Kameyama–Kiselyov territory; the diagonal is what direct
  style means to a user today.

## Decisions

- **Namespaced `Monadic.reflect`/`reify`, Filinski's names kept** —
  chosen because the literature's names are right and the collision
  with Effects.scala's `reflect`/`reify` is real; an object scopes it
  the way `Delim.shift` already does. Rejected: extension `.!` or
  `.value` sugar (speculative; the for-comprehension is the surface
  for now).
- **Multi-shot is an argument FOR this road, not against** — the
  continuation `k` in `shift(k => m.flatMap(k))` is a pure closure;
  `List.flatMap` calls it once per element and nothing breaks. The
  prior note in specs/context-functions.md ("direct style would
  forfeit [multi-shot]") is true of Loom/fiber direct style only —
  one-shot runtime continuations, JVM-only. This construction and a
  macro-CPS both preserve multi-shot on all three platforms.
- **No new Monad instances in the core** — Option/List/Either
  instances live in the test; the core stays free of stdlib
  commitments (a footgun precedent: the Comonad[Id] givens already
  contest `.map`).

## Results

(fill after verify)
