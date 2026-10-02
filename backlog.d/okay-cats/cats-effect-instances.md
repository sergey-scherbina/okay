- [ ] cats-effect-instances — THE gap of the cats-depth audit
      (2026-10-02): an okay program `A ! Async` is not an `F` for code
      written `F[_]: Async` (http4s, doobie, fs2's effectful streams, most
      of typelevel). okay-cats bridges at the BOUNDARY (`asIO`/`asOkay`,
      cancellation both ways) but gives no `MonadCancel`/`Sync`/`Spawn`/
      `Concurrent`/`Temporal`/`Async` for the program monad. Needs: errors
      as `Throwable` (an `Async` failure), `uncancelable`/`poll` on okay's
      cancellation, `start`/`join` on okay fibers, `Ref`/`Deferred` (cats'
      defaults over `Concurrent`), `sleep`/clock on the Timer, `async_`/
      `async` on `Async.await`, `evalOn`/`executionContext`. Acceptance:
      cats-effect-laws' suites on the instance; an fs2 stream and a
      `Resource` written generically run AT the okay program.
