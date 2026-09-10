package okay.cluster

import okay.given

/**
 * THE ENGINE AS A STREAM (specs/dataflow.md, stage 6a).
 *
 * The bar is deliberately the batch answer. A streaming run that
 * differs from the batch run is wrong rather than different: the same
 * events in the same order must make the same panes, drop the same
 * elements and produce the same checksums, whether they arrive all at
 * once or a hundred at a time.
 *
 * That is a strong bar because it can only hold if two things are
 * true at once — a pane stays open across an epoch boundary, and the
 * watermark is right at every step of the way rather than only at the
 * end.
 */
class TestStream extends munit.FunSuite {
  import Feeds.*

  TestJobs.install()

  val feed: Feed = Feed(20000, Late - 1)
  val late: Feed = Feed(20000, Late * 8)

  def batch[R](job: Job[Feed, R], f: Feed, parts: Int): Run[R] =
    Flows.fan(job.flow(f, parts), job.sink(f)).runWith

  test("streamed in epochs, the answer is the batch answer") {
    for parts <- Vector(1, 2, 4, 8); take <- Vector(64, 512, 4096) do
      val here = batch(WindowJob, feed, parts)
      val there = Cluster.stream(WindowJob, feed, parts,
        Vector.fill(3)(Cluster.local), take).runWith
      assertEquals(there.value, here.value, s"$parts partitions, epochs of $take")
      assertEquals(there.dropped, here.dropped, s"$parts partitions, epochs of $take")
  }

  test("a fan of three sinks, streamed") {
    for parts <- Vector(1, 4, 8); take <- Vector(128, 2048) do
      val here = batch(FanJob, feed, parts)
      val there = Cluster.stream(FanJob, feed, parts,
        Vector.fill(3)(Cluster.local), take).runWith
      assertEquals(there.value, here.value, s"$parts partitions, epochs of $take")
  }

  test("the late feed: a stream drops FEWER, and that is not a bug") {
    // The batch engine reconstructs one global order out of its
    // slices — that is stage 1's seeding theorem, and it is why its
    // parallel answer equals its single-threaded one including the
    // drops. A STREAM has no such order to reconstruct: its
    // partitions are independent channels, each with its own
    // watermark, and a channel that has read less has a lower one.
    //
    // So on a feed with late elements the streamed run drops fewer
    // and therefore counts more. Asserting equality here would be
    // asserting that a stream is a batch.
    val here = batch(WindowJob, late, 8)
    assert(here.dropped > 0, "the late feed drops nothing — it asserts nothing")
    for take <- Vector(64, 1024) do
      val there = Cluster.stream(WindowJob, late, 8,
        Vector.fill(2)(Cluster.local), take).runWith
      assert(there.dropped <= here.dropped,
        s"epochs of $take: a stream dropped ${there.dropped}, the batch ${here.dropped}")
      assert(there.value.total >= here.value.total,
        s"epochs of $take: fewer drops must mean at least as much counted")
  }

  test("a pane handed over twice is not counted twice") {
    // an epoch asks the operator for what it has closed, every round.
    // If handing over did not EMPTY, the same pane would reach the
    // coordinator in every subsequent epoch and be merged again — so
    // the count of panes, not merely their sum, is asserted
    val one = Cluster.stream(WindowJob, feed, 4, Vector(Cluster.local), 1000000).runWith
    val many = Cluster.stream(WindowJob, feed, 4, Vector(Cluster.local), 100).runWith
    assertEquals(many.value.n, one.value.n, "the number of panes moved with the epoch size")
    assertEquals(many.value, one.value)
  }

  test("the sessions are let go when the stream ends") {
    val before = Sessions.count
    Cluster.stream(WindowJob, feed, 4, Vector.fill(2)(Cluster.local), 512).runWith: Unit
    assertEquals(Sessions.count, before, "a finished stream left its state behind")
  }

  test("over sockets: the same, with the state living in another party's memory") {
    val server = java.net.ServerSocket(0)
    val serving = Thread.ofVirtual().start(() => Served.serve(server, Cluster.local))
    try
      val wire = Vector.fill(2)(Served.connect("127.0.0.1", server.getLocalPort))
      val here = batch(FanJob, feed, 8)
      val there = Cluster.stream(FanJob, feed, 8, wire, 1024).runWith
      assertEquals(there.value, here.value)
    finally
      server.close()
      serving.join(2000): Unit
  }
}
