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
  extension [F[_] : Monad, A](m: F[A])
    /** μ: the monadic value as a direct value — one definition, both
     * spellings: `m.reflect` and `reflect(m)` */
    inline def reflect[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))
    /** the symbolic μ: `m.?` — Rust's postfix question, generalized */
    inline def ?[B]: Cont[A, F[B], F[B]] =
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

- [x] `reify(reflect(m)) == m` — the round trip is the identity, for
  every monad tested (Option, Either, List, Free)
- [x] short-circuit: reflecting a `None`/`Left` aborts the rest of the
  block — the continuation is dropped, exactly `flatMap`'s semantics
- [x] multi-shot: reflecting a `List` re-runs the continuation once
  per element — two reflects make the cartesian product; multi-shot
  is PRESERVED, not forfeited (see Decisions)
- [x] the reflected values mix with plain code: `if`/`val`/arithmetic
  between reflects behave as in direct style
- [x] stack safety is the REFLECTED monad's, not Cont's: a strict
  flatMap (Option) invokes the continuation in place and costs a
  frame per reflect (1_000 binds tested, deep chains are the
  monad's budget); a trampolined monad (`A ! F`) returns a tree and
  100_000 reflected binds run flat
- [x] okay's own monads reflect too: an `A ! F` program via
  `Monad[Free[F, *]]` — Writer effects survive the round trip and
  run under their handler afterwards

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
  the way `Delim.shift` already does. reflect is ONE extension
  serving both spellings — `m.reflect` postfix (asked for by the
  first user) and `reflect(m)` prefix (an extension is a method;
  the prefix form is its desugared call). REFUTED by the compiler:
  a separate prefix def alongside the extension — ambiguous
  overload at every prefix call site. The symbolic form is `.?` —
  Rust's postfix question generalized from Result to any monad,
  free in this codebase and idiomatic next to `/`, `!`, `%`, `^`.
  Rejected: `.!` (`!` is the program type and an object here) and
  `.value` (says nothing).
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

- 10 tests in TestMonadic, green at first compile on JVM — the ATM
  types inferred cleanly through for-comprehensions with only a
  result-type ascription at the `reify`; no per-reflect annotations
  were needed.
- FINDING (predicted from the runner, then confirmed): reflection's
  stack discipline is inherited from the reflected monad. A strict
  flatMap (Option) calls the continuation inside the enclosing `/`
  frame — O(chain) stack, ~1k binds is comfortable, 100k is not; a
  trampolined monad (`A ! F`: flatMap builds a Bind node and
  returns) runs 100_000 reflected binds in 0.14s. Cont's own Bind
  spill is not the limiter either way.
- Multi-shot confirmed live: two reflected Lists ran the
  continuation 6 times for the 3×2 cartesian product, side effects
  counted per run.
