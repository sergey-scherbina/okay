package okay.wroclaw

/**
 * THE PLAIN JVM: a `while` loop over the array, and `java.lang.Thread`
 * when there is more than one core to use. No stream library of any
 * kind — this is the row every other row in §20 is read against,
 * because it is what the job costs when nothing carries it.
 *
 * It is also the honest ceiling for the four library lanes beside it.
 * They fold the SAME `Native.Fold` over the SAME slices; what they add
 * is a carrier, and what a carrier can do is cost something. A library
 * row faster than this one would mean the loop is leaving something on
 * the table, and that is worth knowing too.
 *
 * PARALLELISM HERE IS THE SAME SHAPE AS EVERYWHERE ELSE in this
 * benchmark: the arrival order is cut into P contiguous slices, each
 * folded independently, and the folds reduced in slice order
 * (`Native.combine`). It needs no coordination while running because
 * a pane is a key and nothing evicts: two slices that both touch a
 * window simply add their cells at the end. The one place order
 * matters is the bunching pair that straddles a boundary, which
 * `Fold.absorb` stitches — and the suite asserts the answer equals the
 * single-threaded one, so the reasoning is checked rather than
 * asserted.
 */
object JvmLane {

  /** one contiguous slice of the arrival order, folded */
  def slice(feed: Feed, tram: Array[Boolean], from: Int, until: Int): Native.Fold =
    val f = new Native.Fold(tram)
    val events = feed.events
    var i = from
    while i < until do
      val d = events(i)
      if f.known(d) then f.add(f.enrich(d))
      i += 1
    f

  /** one thread, one loop */
  def loop(feed: Feed): Job.Result =
    slice(feed, Native.tramTable(feed), 0, feed.events.length).result

  /**
   * The 8-thread run again, timed in two halves and reporting the
   * state it built. It is the measurement behind §20's claim that the
   * no-eviction shape is what stops these lanes scaling: the fold
   * itself parallelises, and then the reduction walks every pane the
   * run ever opened, in one thread.
   *
   * @return (fold ms, combine ms, cells of state)
   */
  def split(feed: Feed, lanes: Int): (Long, Long, Int) =
    val tram = Native.tramTable(feed)
    val n = feed.events.length
    val parts = new Array[Native.Fold](lanes)
    val t0 = System.nanoTime()
    val workers = (0 until lanes).map { i =>
      val t = new Thread(() =>
        parts(i) = slice(feed, tram, Native.bound(n, lanes, i), Native.bound(n, lanes, i + 1)))
      t.start()
      t
    }
    workers.foreach(_.join())
    val t1 = System.nanoTime()
    val cells = parts.map(_.cells).sum
    Native.combine(parts.toSeq): Unit
    val t2 = System.nanoTime()
    ((t1 - t0) / 1000000L, (t2 - t1) / 1000000L, cells)

  /** P threads, joined, reduced in slice order */
  def threads(feed: Feed, lanes: Int): Job.Result =
    if lanes <= 1 then loop(feed) else
      val tram = Native.tramTable(feed)
      val n = feed.events.length
      val parts = new Array[Native.Fold](lanes)
      val workers = (0 until lanes).map { i =>
        val t = new Thread(() =>
          parts(i) = slice(feed, tram, Native.bound(n, lanes, i), Native.bound(n, lanes, i + 1)))
        t.start()
        t
      }
      workers.foreach(_.join())
      Native.combine(parts.toSeq)
}
