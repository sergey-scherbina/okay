package okay.cluster

import okay.given

/**
 * WHAT A WINDOWED PARTIAL ALLOCATES, EXACTLY
 * (BACKLOG: windows-packed-key, flows-pane-tuple).
 *
 * A wall clock cannot price this. The `Flows.run` lane on the Wrocław
 * feed is ~190 ms and the box moves 10% between two runs minutes
 * apart — the reference lane, which the change cannot touch, moved 9%
 * the WRONG way in the A/B that prompted this file. An allocation
 * change wants an allocation instrument.
 *
 * `getTotalThreadAllocatedBytes` sums every thread including the
 * virtual ones the driver runs partitions on, and allocation is
 * DETERMINISTIC in a way that time is not: the same plan over the
 * same feed allocates the same bytes, run after run, whatever else
 * the machine is doing. So this is a Live measurement that could
 * honestly be an assertion — and the number it prints is the one
 * `windows-packed-key` should be judged against, since it is the
 * traffic that entry is about.
 */
class MeasurePaneBytes extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  import Feeds.*

  private val bean = java.lang.management.ManagementFactory.getThreadMXBean
    .asInstanceOf[com.sun.management.ThreadMXBean]

  def bytesOf(f: () => Any): Long =
    for _ <- 0 until 3 do f(): Unit          // let the JIT settle; allocation is stable after
    System.gc()
    val before = bean.getTotalThreadAllocatedBytes
    f(): Unit
    bean.getTotalThreadAllocatedBytes - before

  /**
   * A FEED WITH MANY KEYS, which is the shape the boundary set cares
   * about.
   *
   * `Feeds.events` has sixteen keys, so after the completeness rule
   * only a handful of panes span a partition edge and the boundary
   * maps are nearly empty — a fine feed for correctness and the wrong
   * one for pricing a pane STORE. Wrocław's boundary set is 122 679
   * accumulators because it has 2 482 stop keys, not because it has
   * many events. This makes that shape: the same events over `keys`
   * distinct keys.
   */
  def wide(n: Int, keys: Int): IndexedSeq[Ev] =
    (0 until n).map { i =>
      val h = mix(i.toLong)
      Ev(i * 10L - math.floorMod(h, Late - 1), math.floorMod(h >>> 20, keys.toLong).toInt,
        math.floorMod(h >>> 40, 100).toInt)
    }

  test("the bytes a WIDE-KEY plan allocates — the shape a pane store is for") {
    for keys <- Vector(1_000, 8_000) do
      val xs = wide(100_000, keys)
      val run = Flows.run(Flow.slices(xs, 8).tumbling(Size, Late)(_.key)(_.ts)(value),
        paneSum).runWith
      val fan = Flows.fan(Flow.slices(xs, 8),
        Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith
      assertEquals(run.value, fan.value, s"$keys keys: the two roads disagree")

      val runBytes = bytesOf(() =>
        Flows.run(Flow.slices(xs, 8).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith)
      val fanBytes = bytesOf(() =>
        Flows.fan(Flow.slices(xs, 8),
          Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith)
      println(f"%n  ${xs.length}%,d events over $keys%,d keys: ${run.value.n}%,d panes, " +
        f"${run.merged}%,d of them at the merge (${100.0 * run.merged / run.value.n}%.1f%%)")
      println(f"    Flows.run  ${runBytes}%,14d bytes")
      println(f"    Flows.fan  ${fanBytes}%,14d bytes")
    println()
  }

  test("the bytes a windowed plan allocates, per road") {
    val xs = events(Feed(20000, Late - 1))
    val panes = Flows.run(Flow.slices(xs, 1).tumbling(Size, Late)(_.key)(_.ts)(value),
      paneSum).runWith.value.n

    val rows = Vector(
      ("the source alone (count)", () =>
        Flows.fold(Flow.slices(xs, 8), okay.Aggregator.count[Ev]).runWith),
      ("Flows.run, tumbling, 8 partitions", () =>
        Flows.run(Flow.slices(xs, 8).tumbling(Size, Late)(_.key)(_.ts)(value), paneSum).runWith),
      ("Flows.run, tumbling, 8 partitions, Shuffle(8)", () =>
        Flows.run(Flow.slices(xs, 8).tumbling(Size, Late, finish = Finish.Shuffle(8))(_.key)(_.ts)(value),
          paneSum).runWith),
      ("Flows.fan, the same window as a sink", () =>
        Flows.fan(Flow.slices(xs, 8),
          Sink.tumbling(Size, Late, (e: Ev) => e.key, (e: Ev) => e.ts, value)(paneSum)).runWith),
    ).map((name, f) => (name, bytesOf(f)))

    println(f"%n  ${xs.length}%,d events, $panes%,d panes — ALLOCATED BYTES (deterministic)%n")
    println("  road                                           |        bytes |  per pane")
    println("  -----------------------------------------------|--------------|----------")
    for (name, b) <- rows do
      println(f"  $name%-46s | $b%,12d | ${b.toDouble / panes}%8.1f")
    println()
  }
