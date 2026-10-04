- [ ] okay2-persist-rest: the rest of okay-persist on okay2. okay2-jdbc-writes
      (2026-10-04, `af5a400e6`) made `okay2-persist` with the LOG only;
      okay2-persist-file (2026-10-04) added the file engine (`FileStore`,
      `Segments`, `Doctor`, JVM), `Configs` and `Streams`. Left, in the
      order of specs/okay2.md stage 56: the wire (`WireProtocol`, `Wire`,
      `RemoteStore`), replication and Raft (`Election`, `Replicated`,
      `Raft`, `RaftStore`, `RaftWire`), and the durable workflow over the
      log (`Dialogue`, `Worker`, `Saga` and the rest). `Repair` needs
      okay's `Condition`, which okay2 lacks. Follow okay-persist's own
      tests. (2026-10-04)
