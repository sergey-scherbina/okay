- [ ] cats-io-async — cats IO crosses without a parked thread, both ways
      (specs/cats-io-async.md): `fromIO` as an `Async.await` on
      `unsafeToFutureCancelable` (cancelling the okay side cancels the IO,
      its finalizers run), and `toIOAsync` via `IO.async` (cancelling the
      IO fiber cancels the okay drive) — the ZIO bridge's shape.
