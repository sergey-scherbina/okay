package okay.persist

/** stage 1b: the two fields Raft's proof assumes survive a crash */
class TestStable extends munit.FunSuite {

  test("a file Stable round-trips term and vote, and reads absent as the paper's initial state") {
    val dir = java.nio.file.Files.createTempDirectory("okay-stable")
    val path = dir.resolve("node-0.stable")
    val s = RaftWire.Stable.file(path)
    assertEquals(s.load(), (0L, None))
    s.save(7L, Some("2"))
    assertEquals(s.load(), (7L, Some("2")))
    assertEquals(RaftWire.Stable.file(path).load(), (7L, Some("2")))   // a new process reads the same
    s.save(8L, None)
    assertEquals(RaftWire.Stable.file(path).load(), (8L, None))
    assert(!java.nio.file.Files.exists(dir.resolve("node-0.stable.tmp")), "the sibling is renamed away")
  }

  test("a node started over a Stable that remembers term 5 begins at term 5 with its vote") {
    // PORT 0: the OS assigns one when the node binds, and nothing is
    // bound ahead of it. This used to ask for a free port, CLOSE it,
    // and hand the number to the node -- and between the close and
    // the node's own bind any process on the box may take it, which
    // is the `BindException` that failed a gate on 2026-09-09
    // (gate-honesty). `TestRaftWire` and `TestRaftStore` meet the same
    // race with three tries and say why; they have to, because their
    // nodes are told each OTHER'S ports before any of them starts.
    // This one has no peers and never reads its own port, so it can
    // do the better thing and leave the choice to the OS.
    val node = RaftWire.Node("0", 0, Map.empty, stable = RaftWire.Stable.memory(5L, Some("1")))
    try
      assertEquals(node.currentTerm, 5L)
      assertEquals(node.votedFor, Some("1"))
    finally node.close()
  }
}
