## okay2-persist-wire - the wire, RemoteStore, RaftWire and RaftStore in okay2-persist

Lane 3 of okay2-persist-rest (operator: "do everything needed for okay2,
all at once"; specs/okay2.md stage 56, docs/okay2.md section 37).

- `WireProtocol` (shared): the frames and a client over okay2-platform's
  `Net` that runs on the JVM and on Node. okay2-persist now depends on
  okay2-platform.
- On the JVM: `Wire.Server` and `Wire.Remote` (the capability list as
  the handshake's offer, refusals by name, replicated names routed to
  their coordinator), `RemoteStore` (a remote node as a replica),
  `RaftWire.Node` with `Stable` (Raft over real sockets), and
  `RaftStore` (a `Store` over the Raft log, with its own snapshot).
- Suites: TestWire, TestWireClient, TestWireRepl, TestWireNode (JS),
  TestStable, TestRaftWire and TestRaftStore. Every suite that binds a
  port is `Live` and runs with `liveOnly`.
- Not ported: TestWireTls, because okay2 has no TLS module. The seams it
  tests are here.
