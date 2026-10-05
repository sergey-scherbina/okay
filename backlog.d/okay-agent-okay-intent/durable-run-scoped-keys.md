- [ ] durable-run-scoped-keys — P1 / correctness: isolate external
      idempotency keys between independent durable runs.
      BASELINE (source review, 2026-10-05): `Durable.keyFor/keyOf` in
      `okay-agent/src/main/scala/okay/agent/Durable.scala` uses operation
      name + sequence + `math.abs(fingerprint.hashCode)`. Identical calls
      at the same position in different runs get identical keys; the
      32-bit fingerprint hash also admits collisions. TopicJournal's
      `run` partitions journal storage, but is absent from this key.
      HOW: introduce an explicit stable run/namespace identity at the
      handler boundary and a provider-compatible key encoding without
      relying on the 32-bit hash for identity. Specify uniqueness scope,
      length limits and lifetime; run identity must survive restart and
      be isolated across tenants when providers share a key namespace.
      `okay-persist`'s `Dialogue.Attempt(id, index)` is the reference
      shape, not a reason to mix the two engines' journals.
      Existing `Entry.key` is authoritative on recovery: never regenerate
      old outstanding keys. Specify API/journal migration and span
      identity compatibility before implementation. Do not generate a
      fresh random identity on every replay or silently change live keys.
      DONE: same logical attempt across restart has the same key;
      independent runs with identical input have different keys;
      deliberately colliding old fingerprints cannot alias independent
      attempts; recovery honors legacy persisted keys; drift remains
      rejected. Gate TestDurable/TopicJournal consumers and affected
      behavior consumers. Pair with durable-withkey-first-attempt before
      claiming externally idempotent recovery.
