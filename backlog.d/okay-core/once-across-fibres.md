- [ ] once-across-fibres — `Once.run`'s cells are threaded state, so a
      fibre forked inside the program takes a snapshot and two fibres
      demanding one handle run it twice, each in its own store. A
      shared-cell variant (the store in an `Async` cell, a demand that
      WAITS for a running program instead of throwing) is the
      `Deferred`/`memoize` of the async libraries. Trigger: a consumer
      that shares a `!.once` across fibres; none yet (direct-once,
      2026-09-16).
