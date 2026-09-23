package okay.scalus.flink

import _root_.okay.chain.{Event, Finality}
import _root_.okay.codec.Columns
import _root_.okay.flink.FlinkSchema
import _root_.okay.scalus.*
import org.apache.flink.api.common.typeinfo.TypeInformation
import org.apache.flink.api.connector.source.*
import org.apache.flink.api.java.typeutils.ResultTypeQueryable
import org.apache.flink.core.io.{InputStatus, SimpleVersionedSerializer}
import org.apache.flink.types.Row
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue}
import scala.jdk.CollectionConverters.*

/**
 * The Cardano chain as a Flink FLIP-27 source (specs/scalus.md §7):
 * okay-scalus's follower and `CardanoTables`, rows typed by okay-flink's
 * `FlinkSchema` over okay-codec's `Columns` — so a Flink job, a Spark
 * DataFrame and an engine-free consumer read the same tables.
 *
 * The chain is ONE ordered sequence, so the source has ONE split: the
 * enumerator hands it to the first reader that asks, and that reader
 * runs the follower. The split's state is the last block emitted — a
 * checkpoint `(slot, hash, blockNo)` — so a job restored from a Flink
 * checkpoint resumes exactly after it (blocks are emitted only once
 * confirmed, so what was emitted cannot be taken back). A rollback past
 * `confirmations` fails the job rather than emitting rows never final.
 *
 * `maxBlocks` makes it BOUNDED (tests, backfills); without it the source
 * follows the chain for as long as the job runs.
 */
final case class CardanoConfig(relay: String, network: String, table: String,
                               confirmations: Int = 15, start: String = "tip",
                               maxBlocks: Option[Int] = None):
  def net: CardanoNetwork = network match
    case "mainnet" => CardanoNetwork.mainnet
    case "preprod" => CardanoNetwork.preprod
    case "preview" => CardanoNetwork.preview
    case other => throw IllegalArgumentException(s"unknown network '$other'; one of: mainnet, preprod, preview")

/** the one split: where the chain is read from, and how many blocks so far */
final case class ChainSplit(from: Option[Checkpoint], emitted: Long) extends SourceSplit:
  def splitId(): String = "cardano"

object ChainSplit:
  object Serializer extends SimpleVersionedSerializer[ChainSplit]:
    def getVersion: Int = 1
    def serialize(s: ChainSplit): Array[Byte] =
      (s.from.fold("tip")(c => s"${c.slot}:${c.hash}:${c.blockNo}") + "|" + s.emitted).getBytes(UTF_8)
    def deserialize(version: Int, bytes: Array[Byte]): ChainSplit =
      String(bytes, UTF_8).split('|') match
        case Array(at, n) =>
          val from = at.split(':') match
            case Array(slot, hash, no) => Some(Checkpoint(slot.toLong, hash, no.toLong))
            case _ => None
          ChainSplit(from, n.toLong)
        case _ => throw IllegalArgumentException(s"not a cardano split (version $version)")

/** the enumerator's state: the split, until a reader has it */
final case class Unassigned(split: Option[ChainSplit])

object Unassigned:
  object Serializer extends SimpleVersionedSerializer[Unassigned]:
    def getVersion: Int = 1
    def serialize(u: Unassigned): Array[Byte] = u.split.fold(Array.emptyByteArray)(ChainSplit.Serializer.serialize)
    def deserialize(version: Int, bytes: Array[Byte]): Unassigned =
      if bytes.isEmpty then Unassigned(None) else Unassigned(Some(ChainSplit.Serializer.deserialize(version, bytes)))

final class CardanoFlinkSource(config: CardanoConfig)
    extends Source[Row, ChainSplit, Unassigned] with ResultTypeQueryable[Row]:

  def getBoundedness: Boundedness =
    if config.maxBlocks.isDefined then Boundedness.BOUNDED else Boundedness.CONTINUOUS_UNBOUNDED

  def getProducedType: TypeInformation[Row] =
    val t = CardanoTables.named(config.table)
    FlinkSchema.rowType(Columns.fields(using t.schema))

  private def first: ChainSplit =
    val from = config.start match
      case "tip" => None
      case other => other.split(':') match
        case Array(slot, hash, no) => Some(Checkpoint(slot.toLong, hash, no.toLong))
        case _ => throw IllegalArgumentException(s"start '$other' is neither 'tip' nor slot:hash:blockNo")
    ChainSplit(from, 0)

  def createEnumerator(ctx: SplitEnumeratorContext[ChainSplit]): SplitEnumerator[ChainSplit, Unassigned] =
    Enumerator(ctx, Some(first))
  def restoreEnumerator(ctx: SplitEnumeratorContext[ChainSplit], state: Unassigned): SplitEnumerator[ChainSplit, Unassigned] =
    Enumerator(ctx, state.split)
  def getSplitSerializer: SimpleVersionedSerializer[ChainSplit] = ChainSplit.Serializer
  def getEnumeratorCheckpointSerializer: SimpleVersionedSerializer[Unassigned] = Unassigned.Serializer

  def createReader(ctx: SourceReaderContext): SourceReader[Row, ChainSplit] = Reader(config, ctx)

/** hands the one split to the first reader that asks; the rest get none */
final class Enumerator(ctx: SplitEnumeratorContext[ChainSplit], private var pending: Option[ChainSplit])
    extends SplitEnumerator[ChainSplit, Unassigned]:
  def start(): Unit = ()
  def handleSplitRequest(subtask: Int, host: String): Unit =
    pending match
      case Some(s) => ctx.assignSplit(s, subtask); pending = None
      case None => ctx.signalNoMoreSplits(subtask)
  def addSplitsBack(splits: java.util.List[ChainSplit], subtask: Int): Unit =
    pending = splits.asScala.headOption.orElse(pending)
  def addReader(subtask: Int): Unit = ()
  def snapshotState(checkpointId: Long): Unassigned = Unassigned(pending)
  def close(): Unit = ()

/** runs the follower for its split and emits the table's rows */
final class Reader(config: CardanoConfig, ctx: SourceReaderContext) extends SourceReader[Row, ChainSplit]:
  private val blocks = LinkedBlockingQueue[Either[String, CardanoBlock]]()
  @volatile private var available = CompletableFuture[Void]()
  @volatile private var follower: Option[CardanoFollower] = None
  @volatile private var closed = false
  private var split: Option[ChainSplit] = None
  private var done = false
  private lazy val table = CardanoTables.named(config.table)
  private lazy val shape = Columns.table(using table.schema)

  def start(): Unit = ctx.sendSplitRequest()

  def addSplits(splits: java.util.List[ChainSplit]): Unit =
    splits.asScala.headOption.foreach { s =>
      split = Some(s)
      val _ = _root_.okay.Threads.spawnThread("okay-cardano-flink") { () =>
        CardanoFollower.open(Relays.connect(config.relay), config.net, s.from, Finality.Depth(config.confirmations)) match
          case Left(e) => offer(Left(e))
          case Right(f) =>
            follower = Some(f)
            while !closed do
              f.step() match
                case Left(e) => if !closed then offer(Left(e)); closed = true
                case Right(es) => es.foreach {
                  case Event.Confirmed(b) => offer(Right(b))
                  case Event.RolledBack(to, _) =>
                    offer(Left(s"a rollback past block ${to.height} reached emitted blocks — raise 'confirmations'"))
                }
      }
    }

  private def offer(x: Either[String, CardanoBlock]): Unit =
    blocks.put(x)
    available.complete(null): Unit

  def pollNext(out: ReaderOutput[Row]): InputStatus =
    if done then InputStatus.END_OF_INPUT
    else Option(blocks.poll()) match
      case Some(Left(e)) => throw IllegalStateException(e)
      case Some(Right(b)) =>
        val (fields, toRow) = shape
        table.pick(CardanoTables.of(b)).foreach(r => out.collect(FlinkSchema.rowOf(fields, toRow(r))))
        split = split.map(s => ChainSplit(Some(Checkpoint(b.header.slot, b.header.hash, b.header.blockNo)), s.emitted + 1))
        if config.maxBlocks.exists(m => split.exists(_.emitted >= m)) then
          done = true; InputStatus.END_OF_INPUT
        else InputStatus.MORE_AVAILABLE
      case None =>
        if split.isEmpty && done then InputStatus.END_OF_INPUT
        else
          // a fresh future, then look again: a block offered between the
          // poll and here must not wait for the next one
          available = CompletableFuture[Void]()
          if !blocks.isEmpty then available.complete(null): Unit
          InputStatus.NOTHING_AVAILABLE

  def snapshotState(checkpointId: Long): java.util.List[ChainSplit] = split.toList.asJava
  def isAvailable: CompletableFuture[Void] = available
  def notifyNoMoreSplits(): Unit = if split.isEmpty then { done = true; available.complete(null): Unit }
  def close(): Unit =
    closed = true
    follower.foreach(_.close())
