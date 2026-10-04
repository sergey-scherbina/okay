- [ ] okay2-persist-rest: the rest of okay-persist on okay2. okay2-jdbc-writes
      (2026-10-04, `af5a400e6`) made `okay2-persist` with the LOG only;
      okay2-persist-file, -raft and -wire (2026-10-04) added the file
      engine, `Configs`, `Streams`, `Replicated`, `Election`, the `Raft`
      core, the wire, `RemoteStore`, `RaftWire` and `RaftStore`. Left, in
      the order of specs/okay2.md stage 56: the durable workflow over the
      log (`Dialogue`, `Worker`, `Saga` and the rest). `Repair` needs
      okay's `Condition` and `TestWireTls` a TLS module, neither of which
      okay2 has. Follow okay-persist's own tests. (2026-10-04)
