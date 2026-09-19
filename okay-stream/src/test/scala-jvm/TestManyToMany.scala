package okay

/**
 * P producers, C consumers, ONE channel, and the stream must END for
 * every consumer (adversarial-lanes, 2026-09-06).
 *
 * Every channel law before this one had a single consumer. The first
 * many-to-many benchmark under the adaptive default hung for 45
 * minutes at `cs.foreach(_.join())`: a consumer parked in
 * `receiveBlocking` after `close()` and was never told the stream had
 * ended. This is that shape as a law, over every builder that can
 * produce a bounded channel, with a deadline instead of a hang.
 */
class TestManyToMany extends munit.FunSuite {

  private def run(name: String, mk: () => Channel[Long], p: Int, c: Int, total: Int): Unit =
    val ch = mk()
    val per = total / p
    // `sendBlocking` ANSWERS whether the channel took the element, and
    // this loop used to discard it (channel-lost-part): a refused send
    // and a lost delivery then looked identical in the failure message
    // — which is exactly the question the 2026-09-09 flake asked.
    val refused = java.util.concurrent.ConcurrentHashMap[Int, Integer]()
    val died = java.util.concurrent.ConcurrentHashMap[Int, String]()
    val sent = java.util.concurrent.ConcurrentHashMap[Int, Integer]()
    val ps = (0 until p).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      try
        while i < per do
          if !ch.sendBlocking(w.toLong * per + i) then refused.merge(w, 1, (a, b) => a + b): Unit
          i += 1
      catch case t: Throwable =>
        died.put(w, s"${t.getClass.getName}: ${t.getMessage} at i=$i FRAMES " +
          t.getStackTrace.nn.take(6).map(String.valueOf).mkString(" <- ")): Unit
      sent.put(w, Integer.valueOf(i.toInt)): Unit
    })
    val sums = new Array[Long](c)
    val seen = java.util.concurrent.ConcurrentHashMap[Long, Integer]()
    val cs = (0 until c).map(j => Thread.ofVirtual().start { () =>
      var s = 0L; var go = true
      while go do
        ch.receiveBlocking() match
          case Some(v) => s += v; seen.merge(v, 1, (a, b) => a + b): Unit
          case None => go = false
      sums(j) = s
    })
    ps.foreach(_.join())
    // A PRODUCER THAT DIED IS THE ANSWER, not a missing thousand
    // (channel-lost-part, 2026-09-09). `join` is happy with a thread
    // that threw, so this law used to report a producer's whole output
    // as elements the channel had lost — twice, before anyone asked
    // whether the producer had finished. The throw was real: a first
    // send met a part slot that `claimPart` had counted but not yet
    // published, and `AdaptiveFifo` now waits for it.
    assert(died.isEmpty, s"$name ${p}x$c: a producer threw instead of sending: $died")
    ch.close()
    val deadline = System.currentTimeMillis() + 10000
    cs.foreach { t =>
      t.join(math.max(1, deadline - System.currentTimeMillis()))
      assert(!t.isAlive, s"$name ${p}x$c: a consumer never saw the end -- parked for good after close()")
    }
    val expect = (0 until p).map(w => (0L until per.toLong).map(i => w.toLong * per + i).sum).sum
    if sums.sum != expect then
      import scala.jdk.CollectionConverters.*
      val dups = seen.asScala.filter(_._2 > 1)
      val missing = (0 until p).flatMap(w => (0L until per.toLong).map(i => w.toLong * per + i)).filterNot(seen.containsKey)
      val byPart = missing.groupBy(v => (v / per).toInt).view.mapValues(_.size).toMap
      import scala.jdk.CollectionConverters.given
      fail(s"$name ${p}x$c: received ${seen.size} distinct of ${p * per}; missing ${missing.size} " +
        s"(by producer: $byPart, first ${missing.take(5)}); duplicated ${dups.size}; " +
        s"refused by sendBlocking: ${refused.asScala.toMap}")

  private val shapes = Seq((1, 1), (1, 4), (4, 1), (4, 4), (16, 16))

  test("the default channel ends for every consumer") {
    for (p, c) <- shapes do run("Channel.apply", () => Channel[Long](1024), p, c, 16000)
  }

  test("a plain ring ends for every consumer") {
    for (p, c) <- shapes do run("ring", () => Queues.strong[Long].bounded(1024).build, p, c, 16000)
  }

  test("the adaptive buffer ends for every consumer") {
    for (p, c) <- shapes do run("adaptive", () => Queues.strong[Long].adaptive.parts(16).each(1024).build, p, c, 16000)
  }

  test("the relaxed buffer ends for every consumer") {
    for (p, c) <- shapes do run("relaxed", () => Queues.strong[Long].relaxed.parts(4).each(256).build, p, c, 16000)
  }

  // the shape the deadlock was reproduced at (round 5 363 of the
  // probe): four producers, four consumers, a SMALL per-part capacity
  // so senders park often, many rounds. `adaptive-p-x-c-deadlock`
  test("P x C over the adaptive buffer, senders parking, 400 rounds") {
    var round = 0
    while round < 400 do
      val ch = Queues.strong[Int].adaptive.each(4).build
      val got = java.util.concurrent.atomic.AtomicInteger()
      val consumers = (0 until 4).map(_ => Thread.startVirtualThread { () =>
        var on = true
        while on do ch.receiveBlocking() match
          case Some(_) => val _ = got.incrementAndGet()
          case None => on = false
      })
      val producers = (0 until 4).map(p => Thread.startVirtualThread { () =>
        var i = 0
        while i < 32 do { val _ = ch.sendBlocking(p * 100 + i); i += 1 }
      })
      producers.foreach(_.join())
      ch.close()
      consumers.foreach(_.join())
      assertEquals(got.get, 128, s"round $round")
      round += 1
  }
}
