package okay.wroclaw

import okay.cluster.Flows
import okay.given

/**
 * WHAT THE WROCŁAW JOB ALLOCATES, EXACTLY
 * (BACKLOG: windows-packed-key).
 *
 * The synthetic feed in okay-cluster cannot price a pane STORE: even
 * over 8 000 keys only 0.7% of its panes reach the merge, because its
 * jitter is small against its window and a partition edge cuts
 * cleanly. Wrocław's boundary set is 7.1% — 122 679 accumulators of
 * 1 734 893 panes — because its second stage slides, so an event is
 * in three panes at once and the edges are ragged.
 *
 * So this is the only feed on which the question can be answered, and
 * allocation is the only instrument that can answer it: the wall
 * clock on this lane moves 10% between two runs minutes apart, and
 * the effect being looked for is smaller than that.
 */
class MeasureWroclawBytes extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !Gtfs.present
  override def munitTimeout = scala.concurrent.duration.Duration(20, "min")

  private val bean = java.lang.management.ManagementFactory.getThreadMXBean
    .asInstanceOf[com.sun.management.ThreadMXBean]

  val fixture = new TestWroclawFlow { override def days: Int = 4 }
  val Parts = 8

  def bytesOf(f: () => Any): Long =
    for _ <- 0 until 3 do f(): Unit
    System.gc()
    val before = bean.getTotalThreadAllocatedBytes
    f(): Unit
    bean.getTotalThreadAllocatedBytes - before

  test("the bytes the fan and the single-stage road allocate") {
    def rides = fixture.rides(Parts)
    val whole = Flows.fan(rides, fixture.wholeJob).runWith
    val n = fixture.feed.events.length
    println(f"%n  ${n}%,d events, $Parts partitions, ${whole.merged}%,d accumulators at the merge%n")
    for (name, f) <- Vector(
      ("the source alone (count)", () => Flows.fold(rides, okay.Aggregator.count[Ride]).runWith),
      ("route windows alone (tumbling)", () => Flows.fan(rides, fixture.routeSink).runWith),
      ("stop windows alone (sliding, 3 panes)", () => Flows.fan(rides, fixture.stopSink).runWith),
      ("THE FAN: route + stop + bunching", () => Flows.fan(rides, fixture.wholeJob).runWith),
    ) do
      val b = bytesOf(f)
      println(f"  $name%-40s | $b%,15d bytes | ${b.toDouble / n}%7.1f per event")
    println()
  }
