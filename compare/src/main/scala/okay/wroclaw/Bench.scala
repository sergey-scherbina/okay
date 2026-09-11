package okay.wroclaw

/**
 * THE MEASUREMENT HALF, shared by every engine's lane.
 *
 * §20's table used to be produced by one test in one JVM, and the
 * methodology fixes the operator asked for are all about that:
 *
 *  1. **A JVM per lane.** Every lane is a `main` in its OWN interop
 *     module, so each runs in its own forked JVM: no lane inherits
 *     another's heap, its JIT state or its garbage. It is also the only
 *     arrangement in which Spark can be measured at all — its
 *     `SparkSession` needs a two-stdlib classpath that breaks the
 *     compilation of anything inlining okay's core.
 *  2. **A floor.** `OkayLane.floor` is the same job as a bare `while`
 *     loop over the array — no stream machinery of any kind. Every
 *     other number is then readable as a multiple of the work itself.
 *  3. **Allocation per event**, not only time. Bytes/event is nearly
 *     independent of what else the machine is doing, which on a shared
 *     box makes it the more honest of the two numbers, and it explains
 *     the time.
 *  6. **Cores as a column.** Every row says how many it used, so
 *     one-against-one and four-against-four are visible rather than
 *     reconstructed.
 *
 * Each row also carries the peak heap the run reached and, for lanes
 * that can answer it exactly, the number of panes live at once.
 *
 * A lane asserts its own answer against `OkayLane.run` before it
 * prints anything: a row that computed something else is not slow or
 * fast, it is wrong, and it never reaches the table.
 */
object Bench {

  /** one measured lane, as the script's table reads it */
  final case class Row(lane: String, cores: Int, events: Long, ms: Long,
                       bytesPerEvent: Long, peakHeapMb: Long, note: String):
    def tsv: String =
      f"ROW\t$lane\t$cores\t$events\t$ms\t${if ms == 0 then 0L else events * 1000L / ms}\t" +
        f"$bytesPerEvent\t$peakHeapMb\t$note"

  /** the JVM's total allocation so far, across every thread — the
   * lanes that fan out to fibres or task slots allocate on threads a
   * per-thread counter would miss */
  private val threads: com.sun.management.ThreadMXBean =
    java.lang.management.ManagementFactory.getThreadMXBean
      .asInstanceOf[com.sun.management.ThreadMXBean]

  private def allocated: Long =
    if threads.isThreadAllocatedMemorySupported then threads.getTotalThreadAllocatedBytes else -1L

  /** what a lane's `main` is asked for: the feed every JVM in a run
   * replays, how many rounds, the answer to check against, and which
   * lanes were asked for (empty = all of this module's) */
  final case class Ask(feed: Feed, rounds: Int, expect: Job.Result, only: String):
    def wants(lane: String): Boolean = only.isEmpty || lane.contains(only)

  /**
   * Run one lane: `rounds` times, the best time kept, the allocation
   * of the best round, and the greatest heap any round reached.
   * `None` when this run did not ask for the lane.
   */
  def measure(ask: Ask, lane: String, cores: Int, note: String = "")
             (run: => Job.Result): Option[Row] =
    if !ask.wants(lane) then None else
      var bestNs = Long.MaxValue
      var bestBytes = 0L
      var peak = 0L
      var round = 0
      while round < ask.rounds do
        System.gc()
        val watch = new java.util.concurrent.atomic.AtomicLong(0L)
        val rt = Runtime.getRuntime
        val sampler = Thread.startVirtualThread { () =>
          try while true do
            val used = rt.totalMemory - rt.freeMemory
            if used > watch.get then watch.set(used)
            Thread.sleep(20)
          catch case _: InterruptedException => ()
        }
        val bytes0 = allocated
        val t0 = System.nanoTime()
        val got = run
        val ns = System.nanoTime() - t0
        val bytes = allocated - bytes0
        sampler.interrupt()
        require(got == ask.expect, s"$lane computed a different answer:\n  got $got\n  want ${ask.expect}")
        if ns < bestNs then { bestNs = ns; bestBytes = bytes }
        if watch.get > peak then peak = watch.get
        round += 1
      val n = ask.feed.events.length.toLong
      Some(Row(lane, cores, n, bestNs / 1000000L,
        if bestBytes < 0 then -1L else bestBytes / math.max(1L, n),
        peak / (1024L * 1024L), note))

  /**
   * The shape every lane's `main` takes: `<days> <rounds> <fraction>
   * [only]`, where the fraction is the denominator of the prefix to
   * replay (1 = the whole feed, 4 = a quarter) and `only` selects a
   * lane by substring. The feed is derived here, so every JVM in a run
   * replays exactly the same events.
   */
  def cli(args: Array[String])(lanes: Ask => Seq[Option[Row]]): Unit =
    val days = if args.length > 0 then args(0).toInt else 8
    val rounds = if args.length > 1 then args(1).toInt else 3
    val part = if args.length > 2 then args(2).toInt else 1
    val only = if args.length > 3 then args(3) else ""
    if !Gtfs.present then
      println("SKIP\tno GTFS snapshot (see Gtfs's scaladoc for the two-line download)")
    else
      val whole = Gtfs.events(days)
      val feed = if part <= 1 then whole
                 else whole.copy(events = whole.events.take(whole.events.length / part))
      val ask = Ask(feed, rounds, OkayLane.run(feed), only)
      for row <- lanes(ask).flatten do println(row.tsv)
}
