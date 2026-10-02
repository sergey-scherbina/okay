# interop-classes — okay's type-class ladder across cats, ZIO and kyo

Status: done, 2026-10-02. Owner lane: `interop-classes`.
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
| `catsChoose` | `cats.MonoidK` | `A ! Choose` (full `Alternative`: `CatsClasses.chooseAlternative`, explicit) |
| `okayCatsValidated[E: cats.Semigroup]` | `okay.Selective` | `cats.data.Validated[E, *]` — real `select` |
| `okayIO`, `okayEval` | `okay.Monad` | `cats.effect.IO`, `cats.Eval` |

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
- `ZioClasses.parApplicative[R, E]`: `okay.Applicative[ZIO[R, E, *]]`
  whose `app` is `zipWithPar` — NOT a given (it would tie with the
  monad); passed explicitly, the way cats's `Parallel` is chosen.

### okay-kyo

- `kyoMonad[S]`: `okay.Monad[[A] =>> A < S]` (default given via
  `import okay.kyo.given`); `Pending[S]` names the hole for inference.
- `KyoClasses.parApplicative[E]`: `okay.Applicative[[A] =>> A < (Abort[E] & kyo.Async)]`,
  `app` by `Async.parallel` — explicit, like ZIO's.

## Behavior

- [x] cats: `cats.Traverse[List].traverse` at `okay.Validated` collects
      EVERY error; cats-laws `ApplicativeTests` hold for it
- [x] cats: `Static` under cats' `traverse` is still a `Static` whose
      operations are listed before running, and runs to the same answer
- [x] cats: `parTraverse` over `A ! Async` runs its leaves at once (a
      rendezvous of four), and cats' sequential `traverse` does not
- [x] cats: `A ! Choose` — `<+>` is choice, `empty` prunes, `guard`
      through `chooseAlternative`; cats' `traverse` over it still resolves
- [x] okay `Selective` at `cats.data.Validated`: `select` skips the
      handler on `Right`, `app` accumulates
- [x] okay `Monad[IO]`: `okay.traverse` and `whenS` over an IO;
      `Monad[Eval]`: a 100 000-deep traverse
- [x] FromCats: `okay.traverse` over cats' `NonEmptyList`/`Chain`;
      precedence picks MonadPlus for `List`; cats' `ValidatedNel` stays
      accumulating through the Applicative bridge
- [x] ToCats: cats' `traverse` over a carrier only okay knows (a
      leaf-counting applicative); cats' `Alternative[LazyList]` from okay's
- [x] ZIO: `okay.traverse` over a `ZStream` (cartesian) and a ZIO; the
      parallel applicative passes a rendezvous the monad fails
- [x] kyo: `okay.traverse` over `A < Env` and `A < Emit`, `ifS` over
      kyo; the parallel applicative passes a rendezvous the monad fails

## Decisions

- **The bridges are separate imports, not the default.** A given in
  lexical scope is found BEFORE the implicit scope of the type: with a
  default `FromCats`, `okay.Monad[A ! F]` would resolve through cats'
  instance for our own programs instead of okay's own in `Free`'s
  companion — the same answer, by a detour, and the staging that reads
  okay's instance at its precise type would stop seeing it.
- **`A ! Choose` gets only `MonoidK` by default.** The first cut gave
  one instance that was `StackSafeMonad & Alternative`; it TIED with the
  program monad of CatsInterop.scala (`Ambiguous given instances` on
  `cats.Applicative[A ! Choose]`) — two top-level givens in two files
  have no priority between them, so cats' `traverse` over a choice
  program would have stopped compiling. `MonoidK` is not an
  Applicative, so it ties with nothing.
- **kyo needs the hole named.** `<[+A, -S]` has the value first and
  Scala infers `F[_]` by the last parameter: `traverse` over
  `Env.use(...)` inferred `F = [S] =>> Int < S`. `Pending[S]` is the
  alias a call site writes.
- **kyo's `pure` bypasses `WeakFlat`.** `A < S` is `A | Kyo[A, S]`, so
  `pure(x)` of a value that is itself a kyo computation is not a new
  layer: the monad laws hold for every `A` that is not a `<`. That is
  kyo's own design (its `WeakFlat`/`Flat` evidence exists to refuse
  such an `A` at concrete call sites); a generic instance cannot ask for
  it, so it is documented on the instance instead.

## Results

- 47 tests in five new suites (TestCatsClasses with the cats-laws
  Applicative rules, TestFromCats, TestToCats, TestZioClasses,
  TestKyoClasses), green. Each carrier's POINT is checked by a mutant
  that kills it: cats' `ap` keeping the first error fails the two
  accumulation tests; `zipWith` for `zipWithPar` and a `flatMap` for
  kyo's `Async.parallel` fail the rendezvous tests (each waits its 10 s
  and answers `false`).
