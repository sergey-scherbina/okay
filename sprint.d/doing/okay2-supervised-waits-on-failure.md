- [ ] okay2-supervised-waits-on-failure — the Scala 3 core's fix of 2026-09-28
      (changelog.d/supervised-waits-on-failure.md, specs/cross-platform-
      async.md) has its twin here: `Async.supervised` (okay2-async
      Async.scala, `n.onFirstFailure = e => { n.cancelAll(); done(Left(e)) }`)
      answers the FIRST failure at once, before its cancelled children have
      answered, where the success path waits (`whenIdle`); and JS's
      `PromiseDrive` never settles a cancelled fiber's promise, so a `join`
      on it waits forever. Port the same three moves: `cancel` ANSWERS the
      fiber on every platform (PromiseDrive settles with a
      CancellationException; a pool task cancelled while queued completes
      its cell as it is skipped), `supervised` answers `Left` from `whenIdle`
      with the first error kept in a cell, and the cross tests (a cancelled
      parked fiber still completes; the scope's failure comes after every
      child answered; the FIRST failure wins over a later body failure).
