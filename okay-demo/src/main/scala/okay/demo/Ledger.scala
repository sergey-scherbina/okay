package okay.demo

import okay.{!, Aggregator, Async, Chunks, Pane}
import okay.given
import okay.blob.{Backup, Blob}
import okay.cluster.{Cluster, Flow, Job, Jobs, Run, Wire}
import okay.codec.{Codecs, Schema}
import okay.persist.{Ack, Doctor, FileStore, Policy, Streams, Topic}
import okay.ui.{Frame, Ui}
import java.nio.file.Path

/**
 * ONE BINARY, THE WHOLE STORY (BACKLOG one-binary-story; the
 * small-business path of specs/federation.md's "what people need").
 *
 * A shop records sales; wants a daily report per item; wants to see
 * it; and wants a copy of the books somewhere that is not this
 * machine. Four modules the repository already has, run end to end
 * in ONE process with nothing between them but function calls:
 *
 *   record   — an okay-persist `FileStore`: the sale is a record in
 *              a log, durable before `record` returns;
 *   report   — okay-cluster's windowed engine over that log, through
 *              the stage-11 road (`Streams.chunks`): a tumbling
 *              window of a day, keyed by item, summing cents. The
 *              same `Job` would run on four machines unchanged; here
 *              the one worker is `Cluster.local`;
 *   page     — an okay-ui `Table`, rendered by `Frame` to lines —
 *              the same tree the web and desktop hosts draw;
 *   backup   — okay-blob's `Backup.copy` of the segments to a
 *              `Blob` (a directory here, S3 when the deployment says
 *              so), `restore` into a fresh directory, and the
 *              `Doctor`'s verdict that the copy is restorable BEFORE
 *              anyone needs it.
 *
 * What it does not pretend: a backup copies the ACTIVE segment too
 * (backup-active-segment), so a shop that records and then backs up
 * has its newest sales offsite — and the copy of a file still being
 * appended to can end mid-frame. That is the ordinary crash shape
 * this store already reads: `Doctor` names a torn tail on the last
 * segment restorable, and the test asserts the verdict rather than
 * arranging a fixture that happens to roll at the end. `backup(blob,
 * active = false)` is the strict road — closed segments only, so a
 * second run copies nothing at all and `segmentBytes` bounds what a
 * lost disk costs.
 */
object Ledger {

  /** one sale: when (epoch millis), what, how much */
  final case class Sale(ts: Long, item: String, cents: Long) derives Schema

  /** one line of the report: the day (epoch millis of its start), the
   * item, the day's total */
  final case class Line(day: Long, item: String, cents: Long) derives Schema
  final case class Report(lines: Vector[Line]) derives Schema:
    def sorted: Report = Report(lines.sortBy(l => (l.day, l.item)))

  /** what a worker needs to build its partition: the log's place and
   * the roll size — the job is a name plus these, nothing shipped */
  final case class Books(root: String, segmentBytes: Long) derives Schema

  val Day: Long = 24L * 60 * 60 * 1000
  val TopicName = "sales"
  val codec = Codecs.cbor(summon[Schema[Sale]])

  private def policy(segmentBytes: Long) = Policy(segmentBytes = segmentBytes, compact = false)

  /** the stores this process holds open, by root — a `Ledger` puts
   * its own here so the job reads the SAME handle that writes */
  private val stores = scala.collection.concurrent.TrieMap.empty[String, FileStore]
  private[demo] def opened(root: Path): FileStore =
    stores.getOrElseUpdate(root.toString, FileStore.open(root))
  private[demo] def closed(root: Path): Unit =
    stores.remove(root.toString).foreach(_.close())

  /** the daily report as a job: partition 0 is the shop's one log,
   * read from its beginning; the sink tumbles by day, per item */
  object DailyJob extends Job[Books, Report] {
    type A = Sale
    def name: String = "demo.ledger.daily"
    def params: Schema[Books] = summon[Schema[Books]]
    def answer: Schema[Report] = summon[Schema[Report]]
    def flow(b: Books, parts: Int): Flow[Sale] =
      Flow.of(Vector.tabulate(parts)(p => () =>
        // the store is the caller's: found by name from the
        // parameters, ON the worker, like every source in okay-cluster
        // — the one this process holds open, or opened here if the
        // worker is another process
        val t = stores.getOrElseUpdate(b.root, FileStore.open(Path.of(b.root)))
          .topic(TopicName, parts, policy(b.segmentBytes))
        Chunks.map(Streams.chunks(t, p, t.begin(p)))(r =>
          codec.decode(r.value).fold(why => throw IllegalStateException(why), identity))))
    def sink(b: Books): Wire[Sale, Report] =
      Wire.tumbling(Day, 0L, (s: Sale) => s.item, (s: Sale) => s.ts,
        Aggregator.sum[Long].contramap[Sale](_.cents))(
        Aggregator[Pane[String, Long], Report, Report](Report(Vector.empty))((r, p) =>
          Report(r.lines :+ Line(p.start, p.key, p.value)))((a, b) =>
          Report(a.lines ++ b.lines))(_.sorted))
  }
  Jobs.register(DailyJob)

  /** the page: one table, the same tree every host draws */
  def page(r: Report): Ui =
    Ui.Table(Vector("day", "item", "total"),
      r.lines.map(l => Vector(Ui.Text(java.time.Instant.ofEpochMilli(l.day).toString.take(10)),
                              Ui.Text(l.item), Ui.Text(f"${l.cents / 100}%d.${l.cents % 100}%02d"))),
      "daily")

  def lines(r: Report): Vector[String] = Frame.render(page(r))

  /** the same report, computed the plain way — what the engine's
   * answer is checked against, and what a spreadsheet would do */
  def byHand(sales: Iterable[Sale]): Report =
    Report(sales.groupBy(s => (s.ts / Day * Day, s.item)).toVector
      .map((k, ss) => Line(k._1, k._2, ss.map(_.cents).sum))).sorted
}

/** the shop's books at `root`: record, report, back up, restore */
final class Ledger(root: Path, segmentBytes: Long = 64L * 1024) {
  import Ledger.*

  private val store = Ledger.opened(root)
  private val topic: Topic = store.topic(TopicName, 1, Policy(segmentBytes = segmentBytes, compact = false))
  private val books = Books(root.toString, segmentBytes)

  /** durable before it returns */
  def record(s: Sale): Long = topic.append(0, Array.emptyByteArray, codec.encode(s), Ack.Durable)

  def recorded: Long = topic.end(0)

  /** the report, by the engine, in this process */
  def report(): Run[Report] ! Async = Cluster.run(DailyJob, books, 1, Vector(Cluster.local))

  /** every segment the blob does not hold at that size to the blob,
   * the one being appended to included; answers what was copied THIS
   * time — a second call with nothing recorded between copies
   * nothing. `active = false` leaves the live segment home */
  def backup(blob: Blob, active: Boolean = true): Vector[String] ! Async =
    Backup.copy(root, blob, "books", active)

  def close(): Unit = Ledger.closed(root)
}

object Restored {
  /** the copy placed under a fresh root, the Doctor's verdict on it,
   * and a `Ledger` over it — which answers the report over whatever
   * the backup holds: the books as they stood, or the books up to the
   * last roll if the copy left the live segment home */
  def apply(blob: Blob, fresh: Path, segmentBytes: Long): (Vector[String], Doctor.Report, Ledger) ! Async =
    Backup.restore(blob, fresh, "books").map { placed =>
      (placed, Doctor.scan(fresh), Ledger(fresh, segmentBytes))
    }
}
