package okay

/**
 * REPRODUCER, ignored by default (adversarial-lanes, 2026-09-06): the
 * chunked merge over the ADAPTIVE buffer spun at 100% CPU once in
 * 20 000 rounds (round 147, a platform thread) and once for twelve
 * minutes inside a JMH warmup; 150 000 further rounds did not repeat
 * it. The one dump caught (virtual threads included, via jcmd on this
 * JVM) had the closer's fiber inside `wakeAll -> receiveAsync ->
 * AdaptiveFifo.popScanning -> Ring.pop`. To hunt it: make the merge
 * below build over `Queues.strong[Long].adaptive...` (it uses the
 * default channel, which is a ring again), un-ignore, and run
 * `okayJVM/testOnly okay.ProbeMergeSpin` in a loop; on a stall the
 * test dumps every thread and prints the okay.* frames by count.
 */
class ProbeMergeSpin extends munit.FunSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(20, "min")
  private val N = 2000L
  private def l = Source.of(LazyList.range(0L, N))
  private def r = Source.of(LazyList.range(N, 2L * N))
  private val expect = (0L until 2L * N).sum

  private def dumpSelf(tag: String): Unit =
    val pid = ProcessHandle.current().pid()
    val out = s"/tmp/okay-spin-$tag.txt"
    val jcmd = sys.props("java.home") + "/bin/jcmd"
    val p = new ProcessBuilder(jcmd, pid.toString, "Thread.dump_to_file", "-format=text", "-overwrite", out).redirectErrorStream(true).start()
    p.waitFor()
    val text = scala.io.Source.fromFile(out).getLines().toVector
    val okayFrames = text.map(_.trim).filter(_.startsWith("okay.")).groupBy(identity).view.mapValues(_.size).toSeq.sortBy(-_._2).take(15)
    println(s"[spin] dumped ${text.size} lines to $out; okay.* frames by count:")
    okayFrames.foreach((f, n) => println(f"  $n%5d  $f"))

  test("chunked merge, 150000 rounds on a platform thread as JMH runs it, none stalls".ignore) {
    for round <- 1 to 150000 do
      var out = -1L
      val th = Thread.ofPlatform().start(() => { out = l.merge(r, capacity = 1024, chunked = true).toLazyList.foldLeft(0L)(_ + _) })
      th.join(8000)
      if th.isAlive then
        dumpSelf(s"round$round")
        fail(s"round $round stalled")
      assertEquals(out, expect, s"round $round wrong sum")
  }
}
