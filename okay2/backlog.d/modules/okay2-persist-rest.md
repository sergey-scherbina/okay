- [ ] okay2-persist-rest: what okay-persist has and okay2-persist still
      does not. Everything else landed on 2026-10-04 (specs/okay2.md
      stage 56: the file engine, replication, Raft, the wire and the
      durable workflow). Left: `Repair`, which signals okay's `Condition`
      and waits for okay2 to have one; and `TestWireTls`, which waits for
      a TLS module on okay2 — the seams it tests (`Wire.Server(socket =
      …)`, `Wire.Remote.connect(wrap = …)`) are in place. (2026-10-05)
