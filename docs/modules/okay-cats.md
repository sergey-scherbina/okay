# okay-cats

Instances inward, conversions outward, nothing more.

- `cats.Monad` for every `A ! F` (a `StackSafeMonad`: `tailRecM`
  builds lazily and Free runs it stack-safely) — `mapN`, `traverse`
  and the rest of the syntax work out of the box.
- `cats.MonadError[[A] =>> A ! (Throws % E + F), E]` — `raiseError`
  injects the operation; recovery goes `runEither` then `!.widen`
  back into the union.
- `CatsInterop.toIO` / `fromIO` — both directions block a virtual
  thread (Loom pays, their compute pool never blocks).
- `CatsInterop.toCats` / `fromCats` — `A ! F` ⇄ `cats.free.Free[F, A]`
  operation for operation (`fromCats` is `foldMap` through the
  injecting `FunctionK`, by initiality).
- `CatsInterop.scheduler` — OUR `Scheduler` on THEIR runtime: one
  `given`, and okay fibers, `parMap`, `merge`, supervision run on
  cats-effect (fork = `IO.blocking` on their blocking pool, join
  parks the okay caller).

The instances pass cats' OWN law suites: Monad over the pure row and
MonadError over `Throws % String` — 90 properties, programs compared
by running them (the only observation a value-program offers), the
generators producing left-nested binds so the Bind rotation is under
the laws too.

Caveats: a row containing `Throws` has no `Handler` — `runEither`
before `runWith`. discipline-munit 2.0.0 is inline-incompatible with
munit 1.1 — unfold the RuleSets into plain munit-scalacheck
properties instead.
