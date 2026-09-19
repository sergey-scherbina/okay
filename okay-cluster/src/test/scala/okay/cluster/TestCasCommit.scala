package okay.cluster

/**
 * A COMMIT THAT IS A COMPARE-AND-SET (specs/dataflow.md, stage 10's
 * last box).
 *
 * `Checkpoint.fenced` asks a lease and then writes. That is two
 * operations, and a leader deposed between them lands one stale
 * commit — `TestPersisted` writes exactly that commit by hand and
 * shows the READ side shadowing it (`Checkpoint.newest`, highest
 * (term, epoch) wins). The box asked for the other defence: a store
 * that REFUSES the write.
 *
 * The box also said no store here offers one. `okay-docs` does —
 * `Cond.IfVersion` — and `DocsJournal` is the forty lines that make
 * it a `Fencing` journal. What this suite asserts is the difference
 * that makes: over a journal that can, the ghost's write does not
 * land at all, and the engine chose that road by itself because
 * `fenced` asks whether the journal is `Fencing`.
 */
class TestCasCommit extends munit.FunSuite:

  def bytes(s: String): Array[Byte] = s.getBytes("UTF-8")

  test("a term that is still the highest writes; a lower one does not") {
    val j = DocsJournal()
    assert(j.saveIfTerm(1, bytes("a"), term = 1L), "the first writer was refused")
    assertEquals(j.latest.map(_._1), Some(1))
    // leader B takes over at term 2 and writes
    assert(j.saveIfTerm(2, bytes("b"), term = 2L), "the successor was refused")
    // THE GHOST: A wakes at term 1 and tries to commit. Today's fence
    // would let this through — it is a check before a write and A's
    // lease check happened before it lost the seat.
    assert(!j.saveIfTerm(3, bytes("ghost"), term = 1L),
      "a deposed leader's commit was applied")
    assertEquals(j.latest.map(_._1), Some(2), "the ghost's epoch is in the journal")
    assertEquals(j.latest.map(t => new String(t._2, "UTF-8")), Some("b"))
    assertEquals(j.term, Some(2L))
  }

  test("`fenced` takes the CAS road when the journal offers one — and the lease is not asked") {
    // A LEASE THAT LIES. It says the seat is still held, which is
    // exactly the state a deposed leader is in between the check and
    // the write; over a Fencing journal the answer comes from the
    // STORE instead, so the lie does not matter.
    val liar = new Lease:
      def take(): Option[Long] = Some(1L)
      def held(term: Long): Boolean = true
    val j = DocsJournal()
    assert(j.saveIfTerm(1, bytes("b"), term = 2L), "the leader could not write")
    val ghost = Checkpoint.fenced(term = 1L, liar, j)
    val no = intercept[Checkpoint.Deposed](ghost.save(2, bytes("ghost")))
    assertEquals(no.term, 1L)
    assertEquals(j.latest.map(t => new String(t._2, "UTF-8")), Some("b"),
      "the ghost's bytes are in the journal")
  }

  test("over a journal that CANNOT compare-and-set, the check is still what happens") {
    // the same lying lease over a plain `Checkpoint.Memory`: the old
    // road, and the stale commit lands — which is why
    // `Checkpoint.newest` exists and stays
    val liar = new Lease:
      def take(): Option[Long] = Some(1L)
      def held(term: Long): Boolean = true
    val mem = Checkpoint.Memory()
    val ghost = Checkpoint.fenced(term = 1L, liar, mem)
    ghost.save(2, bytes("ghost"))
    assertEquals(mem.latest.map(t => new String(t._2, "UTF-8")), Some("ghost"),
      "the check-only road refused a write it cannot refuse")
  }

  test("a deposed coordinator stops at its next epoch, from the store's answer alone") {
    val j = DocsJournal()
    // B is the leader at term 2
    assert(j.saveIfTerm(1, bytes("b1"), term = 2L))
    // A, at term 1, is handed a lease that still says yes
    val liar = new Lease:
      def take(): Option[Long] = Some(1L)
      def held(term: Long): Boolean = true
    val a = Checkpoint.fenced(term = 1L, liar, j)
    val stopped = intercept[Checkpoint.Deposed](a.save(2, bytes("a2")))
    assertEquals(stopped.epoch, 2, "the deposition names the epoch it stopped at")
    // and B carries on
    assert(j.saveIfTerm(2, bytes("b2"), term = 2L), "the real leader was blocked by the ghost")
    assertEquals(j.latest.map(t => new String(t._2, "UTF-8")), Some("b2"))
  }
