# okay-cats

> Instances inward, conversions outward, nothing more — and the
> ecosystem's own law suites proving the instances.

Depends on: `okay` (JVM), cats-free, cats-effect.

## Guide

**Instances inward.** Any Okay program is a lawful `cats.Monad`
(`StackSafeMonad`: `tailRecM` builds lazily — the recursion hides in
the flatMap closure — and Free runs it stack-safely), so cats syntax
(`mapN`, `traverse`, …) works on `A ! F` out of the box. A row
containing `Throws % E` is a lawful `MonadError[_, E]`: `raiseError`
injects the operation, recovery goes `runEither` then `!.widen` back
into the union.

**Conversions outward.** Loom is the meeting point with cats-effect:
`toIO` wraps a program as `IO.blocking` (their blocking pool runs it,
a virtual thread parks inside), `fromIO` runs an IO inside one async
op (our virtual thread parks for it). The free-monad bridge is by
initiality: `toCats` walks operations into `cats.free.Free`,
`fromCats` is `foldMap` through the injecting `FunctionK`.

**Their runtime as our Scheduler.** `CatsInterop.scheduler` makes
the cats-effect runtime an Okay `Scheduler`: fork runs the program
as `IO.blocking` via `unsafeToFutureCancelable`, completion callbacks
ride the future, cancel is the IO canceler. One `given`, and Okay
fibers, `parMap`, `merge` and supervision run on cats-effect.

**Laws.** cats-laws' Monad and MonadError rule sets run against the
instances — 90 properties. Programs are compared BY RUNNING them
(the only observation a program-as-value offers) and the generators
produce left-nested binds on purpose, so the Bind rotation is under
the laws too.

## Tutorial

```scala
import okay.given
import okay.cats.given
import cats.syntax.all.*

val a: Int ! State % Int = State.get[Int]
val b: Int ! State % Int = pure(2)
(a, b).mapN(_ + _)                      // cats syntax on okay programs

// errors through cats vocabulary:
type R = Throws % String + Pure
val safe: Int ! R = summon[cats.MonadError[[A] =>> A ! R, String]]
  .handleErrorWith(effect(Throws("boom")))(_ => pure(42))

// the runtime bridge:
given okay.Scheduler = CatsInterop.scheduler   // needs an IORuntime
Async.par(async(1), async(2)).runWith          // fibers on cats-effect
```

## API reference

| member | signature | meaning |
|---|---|---|
| `given StackSafeMonad[[A] =>> A ! F]` | for every `F` | cats Monad on programs |
| `given MonadError[[A] =>> A ! (Throws % E + F), E]` | needs `TypeableK[Throws % E]` | typed errors, cats-style |
| `CatsInterop.toIO` | `(=> A ! Async) => IO[A]` | run as blocking IO |
| `CatsInterop.fromIO` | `(IO[A])(using IORuntime) => A ! Async` | an IO as one async op |
| `CatsInterop.toCats` | `A ! F => cats.free.Free[F, A]` | operation for operation |
| `CatsInterop.fromCats` | `cats.free.Free[F, A] => A ! F` | foldMap by initiality |
| `CatsInterop.scheduler` | `(using IORuntime) => okay.Scheduler` | their runtime under our fibers |

## Gotchas

- `import okay.given` is required in every satellite for the
  extension methods of Okay's package-level givens (`runWith` above
  all); `import okay.cats.given` brings the instances.
- A row containing `Throws` has no `Handler` — `runEither` before
  `runWith`.
- discipline-munit 2.0.0 is inline-incompatible with munit 1.1 —
  the law suites unfold `RuleSet.all.properties` into plain
  munit-scalacheck properties instead.
