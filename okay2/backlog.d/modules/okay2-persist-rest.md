- [ ] okay2-persist-rest: the rest of okay-persist on okay2. okay2-jdbc-writes
      (2026-10-04, `af5a400e6`) made `okay2-persist` with the LOG only —
      `Topic`, `Store`, the typed CBOR view, `Offsets`, `Snapshots`,
      `MemoryStore` and the shared store suite, what Writes/Poll/SqlStore
      need. Not ported: the file store and the replicated stores, the
      wire, Raft, and the durable workflow over the log. Port when a
      caller on okay2 needs them; follow okay-persist's own tests.
      (2026-10-04)
