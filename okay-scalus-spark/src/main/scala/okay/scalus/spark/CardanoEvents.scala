package okay.scalus.spark

import _root_.okay.chain.Event
import _root_.okay.codec.{Cbor, Schema}
import _root_.okay.persist.{Ack, FileStore, Topic}
import _root_.okay.scalus.*
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.connector.read.*
import org.apache.spark.sql.connector.read.streaming.{MicroBatchStream, Offset, ReadLimit, SupportsAdmissionControl}
import org.apache.spark.sql.util.CaseInsensitiveStringMap

/** where a rollback lands: the last block still standing */
final case class RollbackPoint(blockNo: Long, hash: String) derives Schema

/**
 * `mode = events` (specs/scalus.md §6): a table's row, wrapped in what
 * happened — `applied` with `row` set, or `rolled_back` with
 * `rollbackTo` set and `row` null. A consumer deletes every row whose
 * `blockNo` is above `rollbackTo.blockNo` and reads on.
 */
final case class EventRow[A](seq: Long, event: String, rollbackTo: Option[RollbackPoint], row: Option[A])

/** what the journal holds, one record per event */
enum Journaled derives Schema:
  case Applied(block: Carried)
  case RolledBack(blockNo: Long, hash: String)

/**
 * The events journal: an okay-persist `FileStore` topic, one partition,
 * appended `Ack.Durable` (fsync'd) BEFORE an event is visible to Spark.
 * Its offsets ARE the stream's offsets, so a re-run batch reads the same
 * records whatever the chain did since — the reason this mode needs a
 * journal at all (a range holding an orphaned block cannot be fetched
 * again from a relay).
 */
object Journal:
  val topicName = "cardano-events"

  /** option `journal`, else the query's checkpoint location when local */
  def dir(o: CaseInsensitiveStringMap, checkpointLocation: String): java.nio.file.Path =
    Option(o.get("journal")).map(java.nio.file.Path.of(_)).getOrElse {
      val local = checkpointLocation match
        case s if s.startsWith("file:") => Some(java.nio.file.Path.of(java.net.URI(s)))
        case s if s.startsWith("/") => Some(java.nio.file.Path.of(s))
        case _ => None
      local.map(_.resolve("cardano-journal")).getOrElse(throw IllegalArgumentException(
        s"mode=events needs a local journal: set option 'journal' (the checkpoint location $checkpointLocation is not local)"))
    }

  def topic(dir: java.nio.file.Path): Topic =
    java.nio.file.Files.createDirectories(dir): Unit
    FileStore.open(dir).topic(topicName)

  def append(t: Topic, j: Journaled): Long = t.append(0, Array.emptyByteArray, Cbor.write(j), Ack.Durable)

  /** records `from`..`to` inclusive, decoded */
  def read(t: Topic, from: Long, to: Long): Vector[(Long, Journaled)] =
    val out = Vector.newBuilder[(Long, Journaled)]
    var at = from
    while at <= to do
      t.read(0, at, 256) match
        case Topic.Read.Records(rs) if rs.nonEmpty =>
          rs.takeWhile(_.offset <= to).foreach(r =>
            out += r.offset -> Cbor.read[Journaled](r.value).fold(e => throw IllegalStateException(s"journal record ${r.offset}: $e"), identity))
          at = rs.last.offset + 1
        case Topic.Read.Records(_) => throw IllegalStateException(s"journal ends before record $at")
        case Topic.Read.TooEarly(begin) => throw IllegalStateException(s"journal record $at was dropped (begins at $begin)")
    out.result()

  /**
   * Where a restarted stream resumes: the newest block still standing
   * after replaying the journal's events (an Applied adds, a RolledBack
   * removes everything above its point).
   */
  def resume(t: Topic): Option[Checkpoint] =
    val end = t.end(0)
    if end == 0 then None
    else
      val standing = scala.collection.mutable.TreeMap.empty[Long, Carried]
      read(t, t.begin(0), end - 1).foreach {
        case (_, Journaled.Applied(c)) =>
          val h = c.block.header
          standing(h.blockNo) = c
        case (_, Journaled.RolledBack(no, _)) => standing.keysIteratorFrom(no + 1).toList.foreach(standing.remove)
      }
      standing.lastOption.map((_, c) => { val h = c.block.header; Checkpoint(h.slot, h.hash, h.blockNo) })

final case class SeqOffset(seq: Long) extends Offset:
  def json(): String = s"""{"seq":$seq}"""

object SeqOffset:
  private val Json = """\{"seq":(-?\d+)\}""".r
  def parse(s: String): SeqOffset = s match
    case Json(n) => SeqOffset(n.toLong)
    case other => throw IllegalArgumentException(s"not an events offset: $other")

final case class EventsPartition(events: Vector[(Long, Journaled)]) extends InputPartition

final class EventReaders(table: String) extends PartitionReaderFactory:
  def createReader(p: InputPartition): PartitionReader[InternalRow] =
    val kind = CardanoSource.named(table)
    val toInternal = ExpressionEncoder(kind.eventsSchema).createSerializer()
    val rows = p match
      case EventsPartition(es) => es.iterator.flatMap((seq, j) => kind.eventRows(seq, j))
      case other => throw IllegalArgumentException(s"not an events partition: $other")
    new PartitionReader[InternalRow]:
      private var current: InternalRow = null
      def next(): Boolean =
        if rows.hasNext then { current = toInternal(rows.next()).copy(); true } else false
      def get(): InternalRow = current
      def close(): Unit = ()

/**
 * The events stream: the follower appends to the journal on a
 * background thread; offsets are journal sequences (-1 = nothing yet).
 */
final class EventsStream(options: CaseInsensitiveStringMap, kind: CardanoSource.Kind[?], dir: java.nio.file.Path)
    extends MicroBatchStream with SupportsAdmissionControl:
  private val journal = Journal.topic(dir)
  @volatile private var failure: Option[String] = None
  @volatile private var follower: Option[CardanoFollower] = None
  @volatile private var started = false

  def initialOffset(): Offset = SeqOffset(-1)

  private def start(): Unit = synchronized {
    if !started then
      started = true
      val from = Journal.resume(journal).orElse(CardanoSource.checkpoint(Option(options.get("start")).getOrElse("tip")))
      val _ = _root_.okay.Threads.spawnThread("okay-cardano-events") { () =>
        CardanoFollower.open(CardanoSource.wire(options), CardanoSource.network(options), from, depthOf(options)) match
          case Left(e) => failure = Some(e)
          case Right(f) =>
            follower = Some(f)
            while failure.isEmpty do
              f.step() match
                case Left(e) => failure = Some(e)
                case Right(es) => es.foreach {
                  case Event.Confirmed(b) => Journal.append(journal, Journaled.Applied(Carried.of(b))): Unit
                  case Event.RolledBack(to, _) => Journal.append(journal, Journaled.RolledBack(to.height, to.id.value)): Unit
                }
      }
  }

  private def depthOf(o: CaseInsensitiveStringMap) =
    _root_.okay.chain.Finality.Depth(Option(o.get("confirmations")).fold(15)(_.toInt))

  override def getDefaultReadLimit: ReadLimit = ReadLimit.allAvailable()
  override def latestOffset(): Offset =
    throw UnsupportedOperationException("latestOffset(start, limit) is the entry point (SupportsAdmissionControl)")
  override def latestOffset(startOffset: Offset, limit: ReadLimit): Offset =
    start()
    failure.foreach(e => throw IllegalStateException(e))
    SeqOffset(journal.end(0) - 1)

  def planInputPartitions(startOffset: Offset, end: Offset): Array[InputPartition] =
    val (a, b) = (SeqOffset.parse(startOffset.json()).seq, SeqOffset.parse(end.json()).seq)
    val events = if b <= a then Vector.empty else Journal.read(journal, a + 1, b)
    val per = Option(options.get("blocksPerPartition")).fold(10)(_.toInt)
    events.grouped(math.max(per, 1)).map(g => EventsPartition(g): InputPartition).toArray

  def createReaderFactory(): PartitionReaderFactory = EventReaders(kind.name)
  def deserializeOffset(json: String): Offset = SeqOffset.parse(json)
  def commit(end: Offset): Unit = ()     // the journal keeps history; retention is the topic's policy
  def stop(): Unit =
    failure = failure.orElse(Some("stopped"))
    follower.foreach(_.close())
