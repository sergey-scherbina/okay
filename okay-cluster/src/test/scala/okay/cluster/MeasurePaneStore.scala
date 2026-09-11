package okay.cluster

import scala.collection.mutable

/**
 * WHAT A PACKED PANE STORE COULD BUY, BEFORE ONE IS WRITTEN
 * (BACKLOG: windows-packed-key).
 *
 * The entry says a `LongMap` keyed by the window index and the key
 * packed into one `Long` would take what §20's hand-written operator
 * takes, and it says MEASURE BEFORE WRITING IT. This is that
 * measurement, and it is deliberately the CEILING: the two map shapes
 * over the same update sequence, with nothing else in the loop. A
 * seam threaded through three places cannot beat this, so if the
 * ceiling is not worth having, the seam is not either.
 *
 * The update is the one the engine performs — `get` then `update`,
 * merging an accumulator — because that is what a boundary pane costs
 * when a second partition contributes to it.
 *
 * BYTES ARE THE RELIABLE HALF. Allocation is deterministic: the same
 * loop allocates the same bytes whatever the box is doing, where the
 * wall clock on this machine moves 10% between two runs minutes
 * apart. Both are reported, and the bytes are the ones to believe.
 */
class MeasurePaneStore extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  private val bean = java.lang.management.ManagementFactory.getThreadMXBean
    .asInstanceOf[com.sun.management.ThreadMXBean]

  val Rounds = 9
  val Warmup = 3

  /** the window index and the key in one Long — the packing §20's
   * hand-written lane uses, and the one the entry proposes */
  inline def packed(index: Long, key: Int): Long = (index << 32) | (key & 0xffffffffL)

  /** n boundary panes over k keys, each touched twice — one partition
   * puts it there and a second merges into it */
  def tupled(n: Int, k: Int): Long =
    val m = mutable.HashMap.empty[(Long, Int), Long]
    var i = 0
    while i < n do
      val id = ((i / k).toLong, i % k)
      m.update(id, m.get(id).fold(i.toLong)(_ + i))
      val again = (((i / k).toLong, i % k))
      m.update(again, m.get(again).fold(i.toLong)(_ + i))
      i += 1
    m.size.toLong

  def longMapped(n: Int, k: Int): Long =
    val m = mutable.LongMap.empty[Long]
    var i = 0
    while i < n do
      val id = packed((i / k).toLong, i % k)
      m.update(id, m.get(id).fold(i.toLong)(_ + i))
      m.update(id, m.get(id).fold(i.toLong)(_ + i))
      i += 1
    m.size.toLong

  def measure(f: () => Long): (Long, Long) =
    for _ <- 0 until Warmup do f(): Unit
    System.gc()
    val b0 = bean.getTotalThreadAllocatedBytes
    f(): Unit
    val bytes = bean.getTotalThreadAllocatedBytes - b0
    var lo = Long.MaxValue
    for _ <- 0 until Rounds do
      val t0 = System.nanoTime()
      f(): Unit
      lo = math.min(lo, System.nanoTime() - t0)
    (lo, bytes)

  /**
   * THE SAME TWO SHAPES WITH THE ENGINE'S OWN ACCUMULATOR.
   *
   * The rows above merge `Long`s with `+`, which makes the MAP as
   * large a share of the loop as it can possibly be. A boundary pane
   * in the engine carries an `Aggregator.Summary` — four longs and a
   * real merge — so the arithmetic beside the lookup is heavier and
   * the map's share is smaller. This is the number to compare against
   * a road's wall clock, and the one above is the number that flatters
   * the change.
   */
  val summary: okay.Aggregator[Long, okay.Aggregator.Summary, okay.Aggregator.Summary] =
    okay.Aggregator.summary[Long](identity)

  def tupledReal(n: Int, k: Int): Long =
    val m = mutable.HashMap.empty[(Long, Int), okay.Aggregator.Summary]
    var i = 0
    while i < n do
      val id = ((i / k).toLong, i % k)
      val one = summary.add(summary.init, i.toLong)
      m.update(id, m.get(id).fold(one)(summary.merge(_, one)))
      m.update(id, m.get(id).fold(one)(summary.merge(_, one)))
      i += 1
    m.size.toLong

  def longMappedReal(n: Int, k: Int): Long =
    val m = mutable.LongMap.empty[okay.Aggregator.Summary]
    var i = 0
    while i < n do
      val id = packed((i / k).toLong, i % k)
      val one = summary.add(summary.init, i.toLong)
      m.update(id, m.get(id).fold(one)(summary.merge(_, one)))
      m.update(id, m.get(id).fold(one)(summary.merge(_, one)))
      i += 1
    m.size.toLong

  test("the ceiling with the engine's own accumulator, which is the honest one") {
    println(f"%n  the same shapes carrying Aggregator.Summary — what a boundary pane really holds%n")
    println("  entries |  tuple us |  packed us | ratio |    tuple bytes |   packed bytes | saved")
    println("  --------|-----------|------------|-------|----------------|----------------|-------")
    for n <- Vector(122_679, 500_000) do
      val k = math.max(1, n / 8)
      val (ta, tb) = measure(() => tupledReal(n, k))
      val (la, lb) = measure(() => longMappedReal(n, k))
      println(f"  $n%,7d | ${ta / 1000}%,8d | ${la / 1000}%,9d | ${ta.toDouble / la}%4.2fx | " +
        f"$tb%,14d | $lb%,14d | ${(tb - lb) / 1024}%,5d KB")
    println()
  }

  test("the ceiling: a tuple-keyed HashMap against a packed LongMap") {
    println(f"%n  boundary panes, each touched twice — the CEILING a packed store could take%n")
    println("  entries |  tuple us |  packed us | ratio |    tuple bytes |   packed bytes | bytes/entry saved")
    println("  --------|-----------|------------|-------|----------------|----------------|------------------")
    for n <- Vector(10_000, 50_000, 122_679, 500_000) do
      val k = math.max(1, n / 8)                 // ~8 windows per key, Wrocław's shape
      val (ta, tb) = measure(() => tupled(n, k))
      val (la, lb) = measure(() => longMapped(n, k))
      println(f"  $n%,7d | ${ta / 1000}%,8d | ${la / 1000}%,9d | ${ta.toDouble / la}%4.2fx | " +
        f"$tb%,14d | $lb%,14d | ${(tb - lb).toDouble / n}%17.1f")
    println()
  }
