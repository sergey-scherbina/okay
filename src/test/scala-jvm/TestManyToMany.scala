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
    val ps = (0 until p).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = ch.sendBlocking(w.toLong * per + i); i += 1 }
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
      fail(s"$name ${p}x$c: received ${seen.size} distinct of ${p * per}; missing ${missing.size} (by producer: $byPart, first ${missing.take(5)}); duplicated ${dups.size}")

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
}
