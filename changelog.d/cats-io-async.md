## cats-io-async — cats IO across without a parked thread, both ways

`CatsInterop.fromIO` is now an `Async.await` on `unsafeToFutureCancelable`
instead of `unsafeRunSync` in place: the callback runner parks no thread,
and cancelling the okay side cancels the IO (its `onCancel` finalizers
run; a late result resumes nothing). New `toIOAsync` runs a callback-driven
okay program as an `IO` via `IO.async`, and cancelling the IO fiber cancels
the okay drive. `toIO` stays `IO.blocking` for programs with a blocking
`Async.Run`. The cats twin of the ZIO bridge; a marked `io.?` in a direct
block now waits by callback too. Mutant-checked.

Also recorded: a PARKED Lost of `sentinel-single-consumer-lost-end`
(hasReady=true, size=2, a registered receiver never woken) — its priority
is HIGH again.

Spec: specs/cats-io-async.md. Docs: docs/modules/okay-cats.md,
docs/okay2.md, docs/direct-style.md.
