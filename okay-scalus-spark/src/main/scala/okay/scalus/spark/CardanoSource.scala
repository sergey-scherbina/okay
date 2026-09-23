package okay.scalus.spark

import _root_.okay.chain.{Event, Finality}
import _root_.okay.codec.Schema
import _root_.okay.scalus.*
import _root_.okay.scalus.CardanoTables.*
import _root_.okay.spark.SparkSchema
import org.apache.spark.sql.Row
import org.apache.spark.sql.catalyst.InternalRow
import org.apache.spark.sql.catalyst.encoders.ExpressionEncoder
import org.apache.spark.sql.connector.catalog.{SupportsRead, Table, TableCapability, TableProvider}
import org.apache.spark.sql.connector.expressions.Transform
import org.apache.spark.sql.connector.read.*
import org.apache.spark.sql.connector.read.streaming.{MicroBatchStream, Offset, ReadLimit, SupportsAdmissionControl}
import org.apache.spark.sql.sources.DataSourceRegister
import org.apache.spark.sql.types.StructType
import org.apache.spark.sql.util.CaseInsensitiveStringMap
import scala.jdk.CollectionConverters.*

/**
 * `spark.read.format("cardano")` and `spark.readStream.format("cardano")`
 * (specs/scalus.md §6): okay-scalus's follower and `CardanoTables` as a
 * Spark DataSource V2. It adds no table logic — the rows are
 * `CardanoTables`', their shape okay-codec's `Columns`', their Spark
 * types `SparkSchema`'s — so the DataFrame and an engine-free consumer
 * (okay-watch) read the same tables.
 *
 * Options:
 * - `table`: blocks | transactions | inputs | outputs | assets | mints |
 *   certificates | withdrawals | redeemers
 * - `network`: mainnet | preprod | preview
 * - `relay`: `host:port`, or `registered:<name>` for a wire registered in
 *   this JVM (`Relays.register` — tests and embedding)
 * - `confirmations`: blocks on top before a block is read (default 15)
 * - `start`: `tip`, or a checkpoint `slot:hash:blockNo` (the first block
 *   read extends it)
 * - batch only: `blocks` — how many confirmed blocks after `start`
 * - `fetch`: `driver` (default) | `executor` — who fetches the bodies
 * - `blocksPerPartition` (default 10 with `fetch = driver`, 100 with
 *   `executor`: each executor partition opens a session of its own)
 *
 * `fetch = driver`: the DRIVER follows the chain (headers and bodies, on
 * a background thread) and keeps the confirmed blocks' bytes; partitions
 * carry those bytes, and executors DECODE and explode them.
 * `fetch = executor` (specs/scalus.md stage 5): the driver follows
 * HEADERS only and keeps those; a partition carries its range's headers,
 * and the executor block-fetches the range from the relay itself — the
 * network and the decode both in parallel, for a backfill. The relay
 * must then be reachable from the executors (`registered:` wires only
 * in the same JVM, i.e. local mode).
 *
 * Either way an offset is a confirmed checkpoint, so a re-run batch
 * yields the same rows — a confirmed range is the same bytes whoever
 * fetches it. A rollback deeper than `confirmations` fails the query
 * rather than yielding rows that were never final.
 */
final class CardanoSource extends TableProvider with DataSourceRegister:
  def shortName(): String = "cardano"
  override def inferSchema(options: CaseInsensitiveStringMap): StructType =
    CardanoSource.table(options).schema
  override def getTable(schema: StructType, partitioning: Array[Transform], props: java.util.Map[String, String]): Table =
    CardanoTable(CaseInsensitiveStringMap(props))

object CardanoSource:
  /** one of CardanoTables' tables, as Spark reads it */
  final class Kind[A](val table: CardanoTables.Table[A]):
    import table.schema as s
    val name: String = table.name
    private def pick(t: Tables): Vector[A] = table.pick(t)
    lazy val schema: StructType = SparkSchema.structOf[A]
    def rows(t: Tables): Seq[Row] = SparkSchema.rows(pick(t))

    // `events` mode: the same row, wrapped in what happened to it
    private given Schema[EventRow[A]] = Schema.derived
    lazy val eventsSchema: StructType = SparkSchema.structOf[EventRow[A]]
    def eventRows(seq: Long, j: Journaled): Seq[Row] = j match
      case Journaled.Applied(c) =>
        SparkSchema.rows(pick(CardanoTables.of(c.block)).map(r => EventRow(seq, "applied", None, Some(r))))
      case Journaled.RolledBack(no, hash) =>
        SparkSchema.rows(Vector(EventRow[A](seq, "rolled_back", Some(RollbackPoint(no, hash)), None)))

  val kinds: Vector[Kind[?]] = CardanoTables.all.map(t => Kind(t))

  def table(o: CaseInsensitiveStringMap): Kind[?] = named(Option(o.get("table")).getOrElse("blocks"))

  def named(name: String): Kind[?] =
    val t = CardanoTables.named(name)
    kinds.find(_.table eq t).getOrElse(Kind(t))

  def network(o: CaseInsensitiveStringMap): CardanoNetwork = Option(o.get("network")).getOrElse("mainnet") match
    case "mainnet" => CardanoNetwork.mainnet
    case "preprod" => CardanoNetwork.preprod
    case "preview" => CardanoNetwork.preview
    case other => throw IllegalArgumentException(s"unknown network '$other'; one of: mainnet, preprod, preview")

  def wire(o: CaseInsensitiveStringMap): Wire =
    Relays.connect(Option(o.get("relay")).getOrElse(throw IllegalArgumentException("option 'relay' is required")))

  def eventsMode(o: CaseInsensitiveStringMap): Boolean = Option(o.get("mode")).getOrElse("confirmed") match
    case "confirmed" => false
    case "events" => true
    case other => throw IllegalArgumentException(s"unknown mode '$other'; one of: confirmed, events")

  /** who fetches the bodies: false = the driver, true = the executors */
  def executorFetch(o: CaseInsensitiveStringMap): Boolean = Option(o.get("fetch")).getOrElse("driver") match
    case "driver" => false
    case "executor" => true
    case other => throw IllegalArgumentException(s"unknown fetch '$other'; one of: driver, executor")

  def checkpoint(s: String): Option[Checkpoint] = s match
    case "tip" => None
    case other => other.split(':') match
      case Array(slot, hash, no) => Some(Checkpoint(slot.toLong, hash, no.toLong))
      case _ => throw IllegalArgumentException(s"start '$other' is neither 'tip' nor slot:hash:blockNo")

/** a confirmed block's bytes, as a partition carries it */
final case class Carried(era: Int, header: Array[Byte], body: Array[Byte], time: Option[Long]):
  def block: CardanoBlock =
    CardanoBlock(Header.parse(era, header).fold(e => throw IllegalStateException(e), identity), body, time)

object Carried:
  def of(b: CardanoBlock): Carried = Carried(b.header.era, b.header.bytes, b.bytes, b.time)
  /** what the events journal stores (CardanoEvents.scala) */
  given Schema[Carried] = Schema.derived

/** a confirmed block's HEADER, as an executor-fetch partition carries it */
final case class CarriedHeader(era: Int, bytes: Array[Byte]):
  def header: Header = Header.parse(era, bytes).fold(e => throw IllegalStateException(e), identity)

object CarriedHeader:
  def of(h: Header): CarriedHeader = CarriedHeader(h.era, h.bytes)

/** what the driver holds for a confirmed block until it is read */
enum Held:
  case Body(c: Carried)
  case Head(h: CarriedHeader)

/** where a stream stands: the last confirmed block read */
final case class CardanoOffset(slot: Long, hash: String, blockNo: Long) extends Offset:
  def json(): String = s"""{"slot":$slot,"hash":"$hash","blockNo":$blockNo}"""
  def checkpoint: Checkpoint = Checkpoint(slot, hash, blockNo)

object CardanoOffset:
  private val Json = """\{"slot":(\d+),"hash":"([0-9a-f]+)","blockNo":(\d+)\}""".r
  def parse(s: String): CardanoOffset = s match
    case Json(slot, hash, no) => CardanoOffset(slot.toLong, hash, no.toLong)
    case other => throw IllegalArgumentException(s"not a cardano offset: $other")
  def of(c: Checkpoint): CardanoOffset = CardanoOffset(c.slot, c.hash, c.blockNo)

final class CardanoTable(options: CaseInsensitiveStringMap) extends Table with SupportsRead:
  private val kind = CardanoSource.table(options)
  private val events = CardanoSource.eventsMode(options)
  def name(): String = s"cardano.${kind.name}"
  override def schema(): StructType = if events then kind.eventsSchema else kind.schema
  def capabilities(): java.util.Set[TableCapability] =
    Set(TableCapability.BATCH_READ, TableCapability.MICRO_BATCH_READ).asJava
  def newScanBuilder(o: CaseInsensitiveStringMap): ScanBuilder = () => CardanoScan(options, kind)

final class CardanoScan(options: CaseInsensitiveStringMap, kind: CardanoSource.Kind[?]) extends Scan:
  private val events = CardanoSource.eventsMode(options)
  def readSchema(): StructType = if events then kind.eventsSchema else kind.schema
  override def toBatch: Batch =
    if events then throw UnsupportedOperationException("mode=events is a stream; a batch read is always confirmed")
    else CardanoBatch(options, kind)
  override def toMicroBatchStream(checkpointLocation: String): MicroBatchStream =
    if events then EventsStream(options, kind, Journal.dir(options, checkpointLocation))
    else CardanoStream(options, kind)

/** the rows of carried blocks, decoded where the partition runs */
final case class BlockPartition(blocks: Vector[Carried]) extends InputPartition

/** a confirmed RANGE, fetched where the partition runs: its headers (in
 * order, consecutive), the relay, the network */
final case class RangePartition(relay: String, network: String, headers: Vector[CarriedHeader]) extends InputPartition

/** an executor's fetch of its range: its own session, one range request,
 * closed after — a failure names the range, and Spark's task retry is
 * the same fetch again */
object RangeFetch:
  def blocks(p: RangePartition): Vector[CardanoBlock] =
    val net = CardanoSource.network(CaseInsensitiveStringMap(Map("network" -> p.network).asJava))
    val hs = p.headers.map(_.header)
    val s = Session.open(Relays.connect(p.relay), net.magic).fold(e => throw IllegalStateException(s"relay ${p.relay}: $e"), identity)
    try BlockFetch.range(s, net, hs).fold(e =>
      throw IllegalStateException(s"blocks ${hs.head.blockNo}..${hs.last.blockNo} from ${p.relay}: $e"), identity)
    finally s.close()

/** shipped to executors: it carries the table's NAME, and finds the
 * table (a `Schema` is not serializable) where it runs */
final class Readers(table: String) extends PartitionReaderFactory:
  def createReader(p: InputPartition): PartitionReader[InternalRow] =
    val kind = CardanoSource.named(table)
    val toInternal = ExpressionEncoder(kind.schema).createSerializer()
    val rows = p match
      case BlockPartition(bs) => bs.iterator.flatMap(c => kind.rows(CardanoTables.of(c.block)))
      case r: RangePartition => RangeFetch.blocks(r).iterator.flatMap(b => kind.rows(CardanoTables.of(b)))
      case other => throw IllegalArgumentException(s"not a cardano partition: $other")
    new PartitionReader[InternalRow]:
      private var current: InternalRow = null
      def next(): Boolean =
        if rows.hasNext then { current = toInternal(rows.next()).copy(); true } else false
      def get(): InternalRow = current
      def close(): Unit = ()

/** held blocks cut into partitions: bytes as they are, headers as ranges
 * for the executors — one kind per read, since `fetch` is one option */
private def partitions(held: Vector[Held], o: CaseInsensitiveStringMap): Array[InputPartition] =
  val bodies = held.collect { case Held.Body(c) => c }
  val heads = held.collect { case Held.Head(h) => h }
  val size = math.max(per(o), 1)
  val relay = Option(o.get("relay")).getOrElse("")
  val network = Option(o.get("network")).getOrElse("mainnet")
  (bodies.grouped(size).map(g => BlockPartition(g): InputPartition) ++
    heads.grouped(size).map(g => RangePartition(relay, network, g): InputPartition)).toArray

private def per(o: CaseInsensitiveStringMap): Int =
  Option(o.get("blocksPerPartition")).fold(if CardanoSource.executorFetch(o) then 100 else 10)(_.toInt)

/** a confirmed block as what the driver holds; a rollback is the same */
private def held[A](f: A => (CardanoOffset, Held))(e: Event[A]): Event[(CardanoOffset, Held)] = e match
  case Event.Confirmed(b) => Event.Confirmed(f(b))
  case Event.RolledBack(to, from) => Event.RolledBack(to, from)

/** a follower of the kind `fetch` asks for, and what the driver holds of
 * each block it confirms */
private def follower(o: CaseInsensitiveStringMap, from: Option[Checkpoint])
    : Either[String, (() => Either[String, Vector[Event[(CardanoOffset, Held)]]], () => Unit)] =
  val (wire, net) = (CardanoSource.wire(o), CardanoSource.network(o))
  if CardanoSource.executorFetch(o) then
    CardanoFollower.headers(wire, net, from, depth(o)).map(f =>
      (() => f.step().map(_.map(held(h => (CardanoOffset(h.slot, h.hash, h.blockNo), Held.Head(CarriedHeader.of(h)))))), () => f.close()))
  else
    CardanoFollower.open(wire, net, from, depth(o)).map(f =>
      (() => f.step().map(_.map(held(b => (CardanoOffset(b.header.slot, b.header.hash, b.header.blockNo), Held.Body(Carried.of(b)))))), () => f.close()))
private def depth(o: CaseInsensitiveStringMap): Finality =
  Finality.Depth(Option(o.get("confirmations")).fold(15)(_.toInt))

/** a bounded read: `blocks` confirmed blocks after `start` */
final class CardanoBatch(options: CaseInsensitiveStringMap, kind: CardanoSource.Kind[?]) extends Batch:
  private lazy val held: Vector[Held] =
    val want = Option(options.get("blocks")).map(_.toInt)
      .getOrElse(throw IllegalArgumentException("a batch read needs option 'blocks'"))
    val from = CardanoSource.checkpoint(Option(options.get("start")).getOrElse("tip"))
    val (step, close) = follower(options, from).fold(e => throw IllegalStateException(e), identity)
    try
      var got = Vector.empty[Held]
      while got.size < want do
        step().fold(e => throw IllegalStateException(e), identity).foreach {
          case Event.Confirmed((_, h)) => if got.size < want then got :+= h
          case Event.RolledBack(to, _) =>
            throw IllegalStateException(s"a rollback past block ${to.height} reached confirmed blocks — raise 'confirmations'")
        }
      got
    finally close()
  def planInputPartitions(): Array[InputPartition] = partitions(held, options)
  def createReaderFactory(): PartitionReaderFactory = Readers(kind.name)

/**
 * The stream. The follower runs on a background thread from the offset
 * Spark resumes at (`latestOffset(start, limit)` names it), keeping the
 * confirmed blocks' bytes until `commit` says they are read.
 */
final class CardanoStream(options: CaseInsensitiveStringMap, kind: CardanoSource.Kind[?])
    extends MicroBatchStream with SupportsAdmissionControl:
  private val confirmed = java.util.concurrent.ConcurrentSkipListMap[Long, (CardanoOffset, Held)]()
  @volatile private var failure: Option[String] = None
  @volatile private var closeFollower: Option[() => Unit] = None
  @volatile private var started = false

  /** the offset a FRESH query starts from: `start` (tip resolved now) */
  def initialOffset(): Offset =
    CardanoSource.checkpoint(Option(options.get("start")).getOrElse("tip")) match
      case Some(c) => CardanoOffset.of(c)
      case None =>
        val s = Session.open(CardanoSource.wire(options), CardanoSource.network(options).magic)
          .fold(e => throw IllegalStateException(e), identity)
        try
          val src = ChainSyncSource(s, CardanoSource.network(options), None)
          val tip = src.open().fold(e => throw IllegalStateException(e), identity)
          val p = tip.point.getOrElse(throw IllegalStateException("the relay's chain is empty"))
          CardanoOffset(p.slot, p.hex, tip.blockNo)
        finally s.close()

  private def start(from: CardanoOffset): Unit = synchronized {
    if !started then
      started = true
      // okay's adaptive pick: a virtual thread where the JVM has them, a
      // daemon otherwise — the build's JDK floor is 17 (-java-output-version)
      val _ = _root_.okay.Threads.spawnThread("okay-cardano-follower") { () =>
        follower(options, Some(from.checkpoint)) match
          case Left(e) => failure = Some(e)
          case Right((step, close)) =>
            closeFollower = Some(close)
            while failure.isEmpty do
              step() match
                case Left(e) => failure = Some(e)
                case Right(es) => es.foreach {
                  case Event.Confirmed((o, h)) => confirmed.put(o.blockNo, (o, h)): Unit
                  case Event.RolledBack(to, _) =>
                    failure = Some(s"a rollback past block ${to.height} reached confirmed blocks — raise 'confirmations'")
                }
      }
  }

  override def getDefaultReadLimit: ReadLimit = ReadLimit.allAvailable()

  override def latestOffset(): Offset =
    throw UnsupportedOperationException("latestOffset(start, limit) is the entry point (SupportsAdmissionControl)")

  override def latestOffset(startOffset: Offset, limit: ReadLimit): Offset =
    val from = startOffset match
      case o: CardanoOffset => o
      case other => CardanoOffset.parse(other.json())
    start(from)
    failure.foreach(e => throw IllegalStateException(e))
    Option(confirmed.lastEntry()).map(_.getValue._1).getOrElse(from)

  def planInputPartitions(startOffset: Offset, end: Offset): Array[InputPartition] =
    val (a, b) = (CardanoOffset.parse(startOffset.json()), CardanoOffset.parse(end.json()))
    val range = confirmed.subMap(a.blockNo, false, b.blockNo, true).values.asScala.toVector.map(_._2)
    if range.size != (b.blockNo - a.blockNo) then
      throw IllegalStateException(s"blocks ${a.blockNo + 1}..${b.blockNo} are not all held (${range.size}); was the stream restarted past its buffer?")
    partitions(range, options)

  def createReaderFactory(): PartitionReaderFactory = Readers(kind.name)
  def deserializeOffset(json: String): Offset = CardanoOffset.parse(json)
  def commit(end: Offset): Unit =
    confirmed.headMap(CardanoOffset.parse(end.json()).blockNo, true).clear()
  def stop(): Unit =
    failure = failure.orElse(Some("stopped"))
    closeFollower.foreach(_())
