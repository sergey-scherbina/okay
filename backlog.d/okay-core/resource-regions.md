- [ ] resource-regions — PRIORITY: LOW (trigger). `Resource` is region-style (Resource.scala says
      so) but nothing stops a handle from ESCAPING its scope: a file
      handle stored in a `var` inside `Resource.scoped` and read after
      the region released it is a run-time error at best. Kiselyov &
      Shan, "Lightweight monadic regions" (Haskell 2008): a phantom
      scope on the handle and rank-2 scoping on the region make the
      escape a compile error. This is exactly the trick `Delim.Stacked`
      landed today for prompts (a lexical given as evidence the scope
      is open), so the lane is small: `Resource.open` hands the body an
      `Open[H]` evidence that `use(h)` requires; after the region
      returns no evidence is in scope. Nested regions, and a handle
      passed to an inner region, are the cases to pin (their `liftRegion`).
      TRIGGER: the first consumer that holds a handle in a val past the
      region, or a review that finds one. REVIEWED 2026-09-23: no `Db`
      exists in okay-jdbc — `JdbcSql` takes an already-open `Connection`
      from its caller and never touches `Resource` itself; okay-persist
      has no `Resource` import anywhere. The real handle-shaped consumer
      is `okay-sql`'s `Pool`/`Typed`: eight call sites across
      `Pool.borrow`/`Pool.pinned`, `Typed.transact`/`transactRetry`,
      `okay-script/Serve`, `okay-acme/Revoke`, `okay-outbox/Inbox`,
      `okay-demo/ChatDemo`, and the three scala2 adapters
      (Db/WebSocket/Http) — every one of them runs `Resource.run[A, F]`
      with `A` the region's plain result value, never the handle type,
      so the acquired connection cannot be the answer that escapes.
      `Pool.pinned` is the one primitive that DOES hand a live,
      unwrapped connection back inside the tracked program (by design,
      for "a connection pinned for the enclosing scope"); it has exactly
      one caller (`TestPool.scala`), which immediately wraps it in
      `Resource.run` and only touches the connection inside that
      `flatMap` body — scoped correctly, nothing held past the region.
      Trigger has not fired. Still worth building on the NEXT real
      consumer of `Pool.pinned` or a future handle-shaped effect,
      because `pinned`'s safety today rests on caller discipline the
      type does not enforce. (2026-09-23)
