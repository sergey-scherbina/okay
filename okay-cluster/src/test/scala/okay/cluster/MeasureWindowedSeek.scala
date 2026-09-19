package okay.cluster


/**
 * WHICH ROAD LETS A WINDOWED SINK SEEK — the measurement
 * specs/dataflow.md box 2b demands BEFORE anybody builds either
 * (`dataflow-windowed-seek`).
 *
 * A keyed sink hands over a DELTA each epoch and clears, so a fresh
 * session opens at a position with an empty map and is exactly right.
 * A windowed one keeps its open panes inside the partition and hands
 * them over only when they CLOSE — so a session opened at a position
 * has none of them, and their contributions from before it reach
 * nobody. That is why a windowed job still replays from zero, and
 * that replay is the cost this box exists to remove.
 *
 * TWO ROADS, and the spec refuses to choose between them by taste:
 *
 *   A — DELTA HANDOVERS. Hand over every OPEN pane each epoch, not
 *       just the closed ones. A fresh session then has them. The
 *       merge traffic grows from panes-closed to panes-OPEN per
 *       epoch, every epoch, for the life of the run.
 *   B — A REPLAY BOUNDED BY THE HORIZON. Record `(position, max event
 *       time)` per epoch; a fresh session seeks to the epoch whose
 *       maximum is below the oldest open pane's start and replays
 *       from there, seeded with that maximum so late-drop decisions
 *       do not move. The cost is paid ONLY on a resume, and it is the
 *       records between that epoch and now.
 *
 * A COSTS EVERY EPOCH AND B COSTS ONLY A RESUME. That asymmetry is
 * the whole question, so the numbers below are per-epoch traffic for
 * A against per-resume replay for B, both in BYTES, and the verdict
 * needs a resume RATE to be an answer. The file prints the rate at
 * which they break even rather than pretending to know it.
 *
 * WHY IT IS ARITHMETIC AND NOT A RUN: neither road exists, so there
 * is nothing to run. What the numbers price is the SHAPE of the feed
 * — how many panes are open when, and how far back the horizon sits —
 * and that is a property of the window and the event times, which
 * this file computes from the same `Feeds` every other measure uses.
 */
class MeasureWindowedSeek extends munit.FunSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))

  import Feeds.*

  /** the feed the cluster suites use, at the size the measures use */
  val feed: Feed = Feed(200000, Late - 1)
  val evs: IndexedSeq[Ev] = events(feed)

  /** `MeasurePaneBytes` prices a boundary accumulator; a pane handed
   * over carries (start, key, acc) and the accumulator here is a
   * Long. Sixteen bytes of payload plus the tuple's own header is the
   * conservative reading, and the ratio below is insensitive to it. */
  val BytesPerPane = 48L
  /** an `Ev` on the wire: three numbers and a record header */
  val BytesPerRecord = 32L

  /** the panes an event belongs to, for a SLIDING window */
  def panesOf(ts: Long): Seq[Long] =
    val first = math.floorDiv(ts - Size + Slide, Slide) * Slide
    (first to ts by Slide).filter(s => s <= ts && ts < s + Size)

  /**
   * Walk the feed in epochs and record, for each, what each road
   * would have cost. An epoch here is a fixed number of records,
   * which is what `Cluster.stream`'s batch is.
   */
  final case class Epoch(at: Int, records: Long, open: Long, closed: Long,
                         horizonRecords: Long)

  def walk(batch: Int): Vector[Epoch] =
    val out = Vector.newBuilder[Epoch]
    val open = scala.collection.mutable.HashSet.empty[(Long, Int)]
    // (records so far, max event time) per epoch — road B's journal
    val marks = scala.collection.mutable.ArrayBuffer.empty[(Long, Long)]
    var maxTs = Long.MinValue
    var seen = 0L
    var epoch = 0
    evs.grouped(batch).foreach: chunk =>
      epoch += 1
      chunk.foreach: e =>
        seen += 1
        if e.ts > maxTs then maxTs = e.ts
        panesOf(e.ts).foreach(s => open.add((s, e.key)): Unit)
      // the watermark: the largest event time seen, less the declared
      // lateness — the only number that is true of what has not
      // arrived yet
      val watermark = maxTs - Late
      val closing = open.filter((s, _) => s + Size <= watermark)
      closing.foreach(open.remove)
      marks += ((seen, maxTs))
      // road B: the oldest OPEN pane's start is the horizon; seek to
      // the last epoch whose maximum event time is below it
      val horizon = if open.isEmpty then maxTs else open.map(_._1).min
      val from = marks.findLast((_, m) => m < horizon).map(_._1).getOrElse(0L)
      out += Epoch(epoch, chunk.length.toLong, open.size.toLong,
        closing.size.toLong, seen - from)
    out.result()

  test("the two roads, priced") {
    for batch <- Vector(512, 4096) do
      val es = walk(batch)
      val perEpochA = es.map(_.open).sum * BytesPerPane / es.length
      val perEpochToday = es.map(_.closed).sum * BytesPerPane / es.length
      val perResumeB = es.map(_.horizonRecords).sum * BytesPerRecord / es.length
      val worstB = es.map(_.horizonRecords).max * BytesPerRecord
      // the whole replay, which is what a windowed job pays TODAY
      val today = evs.length.toLong * BytesPerRecord

      println(f"""
        |== batch $batch%d, ${es.length}%d epochs over ${evs.length}%d events
        |road A  per epoch, every epoch : $perEpochA%,12d B  (open panes: avg ${es.map(_.open).sum / es.length}%,d)
        |        what an epoch costs now: $perEpochToday%,12d B  (closed panes)
        |road B  per resume, avg        : $perResumeB%,12d B  (${es.map(_.horizonRecords).sum / es.length}%,d records)
        |        per resume, worst      : $worstB%,12d B
        |        a resume TODAY          : $today%,12d B  (the whole topic)
        |break-even: road A costs less than road B only above
        |            ${if perEpochA == 0 then 0.0 else perResumeB.toDouble / perEpochA}%.2f resumes per epoch
        |""".stripMargin)
      assert(es.nonEmpty)
  }
