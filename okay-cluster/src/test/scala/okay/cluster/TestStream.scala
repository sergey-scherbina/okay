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

  // -----------------------------------------------------------------
  // stage 6b: a worker dies mid-stream
  // -----------------------------------------------------------------

  test("a worker that dies mid-stream is replaced, and the answer does not move") {
    // the replacement holds no state: it replays the partition and
    // discards the epochs the coordinator already absorbed. There is
    // no snapshot of an operator's insides anywhere in this.
    val here = batch(FanJob, feed, 8)
    val n = java.util.concurrent.atomic.AtomicInteger(0)
    val dies: Cluster.Serve = req =>
      if n.incrementAndGet() > 5 then throw java.io.IOException("worker gone mid-stream")
      else Cluster.local(req)
    val there = Cluster.stream(FanJob, feed, 8, Vector(dies, Cluster.local), 512).runWith
    assertEquals(there.value, here.value)
    assert(there.retried > 0, "the dying worker was never noticed")
  }

  test("every worker but one dies, at a seeded moment, and the stream still finishes") {
    val here = batch(WindowJob, feed, 8)
    for seed <- 1L to 12L do
      val dying = 1 + math.floorMod(mix(seed), 3L).toInt              // 1..3 of 4
      val workers = Vector.tabulate(4) { i =>
        if i >= dying then Cluster.local
        else
          val n = java.util.concurrent.atomic.AtomicInteger(0)
          (req: Req) =>
            if n.incrementAndGet() > 1 + math.floorMod(mix(seed * 7 + i), 9L).toInt then
              throw java.io.IOException(s"worker $i gone")
            else Cluster.local(req)
      }
      val there = Cluster.stream(WindowJob, feed, 8, workers, 256).runWith
      assertEquals(there.value, here.value, s"seed $seed, $dying of 4 dying")
  }

  test("a session asked for an epoch it has already answered answers the SAME thing") {
    // what a retry after a LOST REPLY must get. Without it the
    // coordinator would absorb one epoch twice, and with the naive
    // fix — asking for the next epoch instead — it would silently
    // lose one epoch's data and fail nothing, which is the mistake
    // this lane's claim predicted of itself.
    TestJobs.install()
    val opened = WindowJob.openAt(
      okay.codec.Codecs.cbor(WindowJob.params).encode(feed), 0, 4).toOption.get
    val bounds = Vector(Bounds(Long.MinValue, Long.MinValue))
    // BY CONTENT. `Resp.Epoch` carries an `Array[Byte]`, and Scala's
    // `==` on an array is reference equality — so two responses with
    // identical bytes compare unequal, and a test that used `==`
    // reports a difference that is not there. (This test did, and the
    // extents in its own failure message were identical.)
    def same(a: Resp, b: Resp): Boolean = (a, b) match
      case (x: Resp.Epoch, y: Resp.Epoch) =>
        x.bytes.toVector == y.bytes.toVector && x.extent == y.extent && x.drained == y.drained
      case _ => a == b

    val first = opened.advance(256, bounds, 1)
    val again = opened.advance(256, bounds, 1)
    assert(same(again, first), "asking for an epoch twice gave two different answers")
    val second = opened.advance(256, bounds, 2)
    assert(!same(second, first), "epoch 2 answered epoch 1's partial")

    // and a FRESH session catches up to the same place
    val fresh = WindowJob.openAt(
      okay.codec.Codecs.cbor(WindowJob.params).encode(feed), 0, 4).toOption.get
    assert(same(fresh.advance(256, bounds, 2), second),
      "a replayed session did not arrive where the original was")
  }

  test("the sessions are let go when the stream ends") {
    val before = Sessions.count
    // `val _ =`, not `: Unit`: the ascription silences the non-unit
    // STATEMENT lint but not value discard, and this line has been
    // warning on a cold compile since 6a (master's, not this lane's)
    val _ = Cluster.stream(WindowJob, feed, 4, Vector.fill(2)(Cluster.local), 512).runWith
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
