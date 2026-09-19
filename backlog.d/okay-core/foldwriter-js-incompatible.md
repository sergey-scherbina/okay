- [ ] foldwriter-js-incompatible — `Chunks.foldWriter` (okay-stream/
      src/main/scala/Chunks.scala) requires `CanBlock`, which does not
      exist on JS (`src/main/scala-js/Platform.scala`: "There is no
      CanBlock on JS, so a blocking join is a compile error"). JS also
      has no `Handler[Async]` at all — it drives `Async` programs
      through a callback-based `Scheduler`/`Fiber` (`Async.PromiseDrive`
      in the same file), not the blocking one `foldWriter`'s
      `async { ... }` wrapper depends on.
      Found 2026-09-19 (foldwriter-js-incompatible-correction) while
      checking whether the three call sites the fix was built for
      (`Bulk.scala`, `Pipeline.scala` — both okay-stream/src/main/scala,
      cross-platform — and `Acceptance.scala`, in BOTH `okay-cluster`
      and `okay-http`, also cross-platform by design: "a JS client
      under Node and a JVM server use exactly this object") could
      actually migrate onto it. None of them can, as written — an
      earlier lane's "unblocked" claim was wrong (corrected in the
      spec and sprint item).
      A JS-compatible `foldWriter` needs a fundamentally different
      walk: driven by callbacks/the event loop, not an eager blocking
      `Iterator`. Not designed or attempted here — it's a real,
      separate question (does the whole eager-iterator-based fix this
      session landed even generalize to JS, or does JS need its own,
      differently-shaped combinator entirely), deserving its own
      deliberate design pass, not a quick patch riding this backlog
      entry. See specs/producer-to-writer-carrier.md's `## Results`
      for the full writeup of what led here.
