# interop-classes — okay's type-class ladder across cats, ZIO and kyo

Status: in progress, 2026-10-02. Owner lane: `interop-classes`.
Follows `specs/interop.md` (P3), which put instances on PROGRAMS only:
cats `Monad`/`MonadError` for `A ! F`, okay `Monad` for `ZIO`.

## Goal

Transparency at the level of the CLASSES, both directions:

1. **inward** — okay's own `Functor`, `Applicative`, `Selective`,
   `Monad` (and `Alternative`/`MonadPlus`) answer for THEIR types, so
   `okay.traverse`, `whenS`, `direct` and every other combinator written
   against our ladder runs over a cats `IO`, a `ZStream`, a kyo `A < S`;
2. **outward** — THEIR classes answer for OUR types where they have
   classes at all, so cats' `traverse`, `parTraverse`, `mapN` run over
   `okay.Validated`, `Static`, `Par`, `A ! Choose`.

Who has classes: cats (`Functor` … `Monad`, `Alternative`, `Parallel`).
ZIO core and kyo have NONE — ZIO's combinators are methods on the data
type (`zipPar`, `validatePar`), kyo's are functions over `<`. So the
outward direction exists for cats only; `zio-prelude` is a separate
library and adding it is a dependency decision this lane does not take.
No library of the three has `Selective` — ours goes inward only.

## Interface

### okay-cats

Default (`import okay.cats.given`), specific types, no ambiguity with
anything that existed:

| given | class | for |
|---|---|---|
| `catsValidated[E: okay.Semigroup]` | `cats.Applicative` | `okay.Validated[E, *]` — `ap` ACCUMULATES |
| `catsStatic[F]` | `cats.Applicative` | `okay.Static[F, *]` — stays static |
| `catsPar(using Scheduler)` | `cats.Parallel.Aux` | `A ! Async` ↔ `Par` — `parTraverse` forks |
| `catsChoose` | `cats.StackSafeMonad & cats.Alternative` | `A ! Choose` |
| `okayCatsValidated[E: cats.Semigroup]` | `okay.Selective` | `cats.data.Validated[E, *]` — real `select` |
| `okayIO` | `okay.Monad` | `cats.effect.IO` |

Conversions: `CatsInterop.toCatsValidated` / `fromCatsValidated`.

Generic bridges, ONE direction per import — importing both lets each
derive the other's instance from its own and the search diverges:

- `import okay.cats.FromCats.given` — okay's class from cats': MonadPlus
  ⇐ Monad + Alternative, Monad ⇐ Monad, Alternative ⇐ Alternative,
  Applicative ⇐ Applicative, Functor ⇐ Functor, in that priority.
- `import okay.cats.ToCats.given` — cats' class from ours: Alternative,
  Applicative, Functor. NOT `Monad`: cats' `Monad` demands `tailRecM`,
  which a generic okay `Monad` can only write as `flatMap` recursion —
  stack-safe exactly when the carrier defers, and the operator's rule
  (AGENTS.md, no unbounded stack recursion) refuses that for an
  arbitrary carrier. The deferring carriers (`A ! F` and its aliases)
  already have their `StackSafeMonad` by default.

### okay-zio

- `zstreamMonad[R, E]`: `okay.Monad[ZStream[R, E, *]]` (default given).
- `ZioInterop.parApplicative[R, E]`: `okay.Applicative[ZIO[R, E, *]]`
  whose `app` is `zipWithPar` — NOT a given (it would tie with the
  monad); passed explicitly, the way cats's `Parallel` is chosen.

### okay-kyo

- `kyoMonad[S]`: `okay.Monad[[A] =>> A < S]` (default given via
  `import okay.kyo.given`).
- `KyoInterop.parApplicative[E]`: `okay.Applicative[[A] =>> A < (Abort[E] & kyo.Async)]`,
  `app` by `Async.parallel` — explicit, like ZIO's.

## Behavior

- [ ] cats: `cats.Traverse[List].traverse` at `okay.Validated` collects
      EVERY error; cats-laws `ApplicativeTests` hold for it
- [ ] cats: `Static` under cats' `traverse` is still a `Static` whose
      operations are listed before running, and runs to the same answer
- [ ] cats: `parTraverse` over `A ! Async` runs its leaves at once
      (both started before either finishes), answers in order
- [ ] cats: `A ! Choose` — `combineK` is choice, `empty` prunes, and
      `cats.Applicative[A ! Choose]` resolves without ambiguity
- [ ] okay `Selective` at `cats.data.Validated`: `select` skips the
      handler on `Right`, `app` accumulates
- [ ] okay `Monad[IO]`: `okay.traverse` and `whenS` over an IO
- [ ] FromCats: `okay.traverse` over cats' `Eval`/`NonEmptyList`/`Chain`
      via the bridge; `okay.Alternative[List]` from cats'; precedence
      picks MonadPlus where cats has Monad + Alternative
- [ ] ToCats: cats' `traverse` over a carrier that only has OUR
      Applicative (`Const`-like test carrier); `okay.Validated` keeps
      its accumulation through the bridge
- [ ] ZIO: `okay.traverse` over a `ZStream` (monad, cartesian); the
      parallel applicative runs two sleeps in parallel time
- [ ] kyo: `okay.traverse` and `whenS` over `A < S`; the parallel
      applicative runs both leaves at once

## Decisions

- **The bridges are separate imports, not the default.** A given in
  lexical scope is found BEFORE the implicit scope of the type: with a
  default `FromCats`, `okay.Monad[A ! F]` would resolve through cats'
  instance for our own programs instead of okay's own in `Free`'s
  companion — the same answer, by a detour, and the staging that reads
  okay's instance at its precise type would stop seeing it.
- **kyo's `pure` bypasses `WeakFlat`.** `A < S` is `A | Kyo[A, S]`, so
  `pure(x)` of a value that is itself a kyo computation is not a new
  layer: the monad laws hold for every `A` that is not a `<`. That is
  kyo's own design (its `WeakFlat`/`Flat` evidence exists to refuse
  such an `A` at concrete call sites); a generic instance cannot ask for
  it, so it is documented on the instance instead.

## Results
