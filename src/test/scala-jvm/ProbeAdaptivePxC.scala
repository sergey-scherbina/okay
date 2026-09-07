package okay

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

/** DEBUG-PROBE (losing-rows): the P x C shape the JMH fork hung at,
 * run for thousands of rounds with a watchdog that says where every
 * thread stood when it stopped. `adaptive-p-x-c-deadlock`. */
class ProbeAdaptivePxC extends munit.FunSuite {
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val P = 4
  private val C = 4
  private val perProducer = 64

  private def round(idx: Int)(using CanBlock): Unit =
    val ch = Queues.strong[Int].adaptive.each(16).build
    val got = AtomicInteger()
    val consumers = (0 until C).map(_ => Thread.startVirtualThread { () =>
      var on = true
      while on do
        ch.receiveBlocking() match
          case Some(_) => val _ = got.incrementAndGet()
          case None => on = false
    })
    val producers = (0 until P).map(p => Thread.startVirtualThread { () =>
      var i = 0
      while i < perProducer do { val _ = ch.sendBlocking(p * 1000 + i); i += 1 }
    })
    producers.foreach(_.join())
    ch.close()
    consumers.foreach(_.join())
    assertEquals(got.get, P * perProducer, s"round $idx lost elements")

  test("P x C over the adaptive buffer, many rounds".ignore) {
    given CanBlock = summon[CanBlock]
    val rounds = 20000
    val done = AtomicLong()
    val watchdog = Thread.startVirtualThread { () =>
      var last = -1L
      var quiet = 0
      while quiet < 12 do
        Thread.sleep(5000)
        val now = done.get
        if now == last then
          quiet += 1
          if quiet == 2 then
            println(s"[PROBE] STALLED at round $now — dumping")
            Thread.getAllStackTraces.forEach { (t, st) =>
              if t.getName.startsWith("Virtual") || st.exists(f => f.getClassName.startsWith("okay.")) then
                println(s"[PROBE] ${t.getName} ${t.getState}")
                st.take(6).foreach(f => println(s"[PROBE]     $f"))
            }
        else { quiet = 0; println(s"[PROBE] round $now") }
        last = now
    }
    var i = 0
    while i < rounds do { round(i); val _ = done.incrementAndGet(); i += 1 }
    watchdog.interrupt()
    println(s"[PROBE] finished $rounds rounds with no stall")
  }
}
