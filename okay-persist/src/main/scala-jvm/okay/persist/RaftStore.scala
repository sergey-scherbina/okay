package okay.persist

import okay.codec.{Cbor, Schema}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}
import scala.util.boundary, boundary.break

/**
 * The `Store` over a Raft-replicated log (specs/consensus.md, stage
 * 1b): the reduction's claim made concrete — `Election` constructs a
 * `Topic` over this without changing, because this IS a `Store`.
 *
 * The shape is the replicated state machine of the paper: an append
 * is PROPOSED to the leader as one log entry (`Op.Append`: topic,
 * partition, key, value, and the proposer's own id), and every node
 * — leader and followers alike — APPLIES each entry to its LOCAL
 * store when the wire node reports it committed, in log order. So
 * the local stores agree by construction, and a read served from the
 * local store never shows a record a failover could unwrite: only
 * committed entries reach it, which is the same guarantee
 * `Replicated` states with its high-water mark.
 *
 * `append` on a follower is CARRIED to the leader over the node wire
 * (persist-raft-forward: `RaftMsg.Propose`/`Proposed`) — the leader
 * appends it as its own entry, and the answer is still this node
 * applying the committed entry, so the offset returned is the local
 * one. `NotLeader(leader)` is thrown only when no leader is known (an
 * election in progress) or when the node it was carried to refused
 * it and named another. Every append waits for ITS entry to commit —
 * an offset that is not yet an offset is not an answer — and a
 * proposal that does not commit within `commitWaitMs` (a lost
 * majority) throws `NotCommitted` rather than pretending.
 *
 * Topics are declared per node, not replicated: partition counts are
 * configuration, agreed the way a cluster's addresses are.
 *
 * Stage 2c — the store's own snapshot: `snapshot()` writes the local
 * store's FULL history (every topic, every partition, every record
 * with its offset) as one image at the last applied index and hands
 * it to the node's `compact`; a node that falls behind a compacted
 * stretch is restored from it — the image's records past its own
 * local `end` appended in offset order, since every node applies the
 * same log and so holds a prefix of the same history. Refused when
 * any partition's `begin` is past 0 (retention or a local `compact`
 * has dropped history the image would need, and offsets must survive
 * a restore); a gap between a local `end` and the image is named in
 * `damaged` and refused, never papered over. `snapshotEvery` > 0
 * takes one every that many applied entries.
 */
final class RaftStore private (val id: String, local: Store, commitWaitMs: Long, snapshotEvery: Int) extends Store:

  import RaftStore.*

  private val pending = ConcurrentHashMap[String, Pending]()
  /** the last Raft index applied to the local store: what a snapshot
   * is taken at, and up to which a re-application from a snapshot's
   * edge is skipped (already in the store). Written under the node's
   * lock only (the callbacks run there) */
  @volatile private var appliedIndex = 0L
  private var sinceSnapshot = 0
  /** a restore this store could not honour — the local store no
   * longer matches the log and serves what it had; an operator's
   * matter, said once and kept */
  @volatile var damaged: Option[String] = None
  // set once by `start`, after this store exists: the node's commit
  // seam is a method of the store, and the store cannot see the node
  // before it is built
  private var wired: RaftWire.Node | Null = null
  private def node: RaftWire.Node = wired.nn
  private var seq = 0L

  /** a forwarded proposal the leader refused: fail the proposer's wait
   * with the leader it named, rather than letting it time out */
  private def refused(n: Long, leader: Option[String]): Unit =
    val p = pending.remove(s"$id/$n")
    if p != null then { p.nn.refused = Some(leader); p.nn.done.countDown() }

  /** the wire node's commit seam (under its lock, in log order):
   * apply, then wake the proposer */
  private def applied(index: Long, entry: RaftEntry): Unit =
    // a re-application from a snapshot's edge: already in the store
    if index <= appliedIndex then ()
    else
      // a configuration entry (stage 2a) is the cluster's business, and
      // a leader's blank no-op (paper §8) is the log's own: nothing to apply
      if entry.members.nonEmpty || entry.data.isEmpty then ()
      else okay.codec.Codecs.readCbor[Op](entry.data) match
        case Right(Op.Append(topic, partition, key, value, proposer, n)) =>
          val off = local.topic(topic).append(partition, key, value, Ack.Durable)
          val p = pending.remove(s"$proposer/$n")
          if p != null then { p.nn.offset = off; p.nn.done.countDown() }
        case Left(err) =>
          // an entry this node cannot read is a bug in the writer, not a
          // reason to diverge silently: name it and keep applying
          System.err.println(s"raft-store $id: unreadable entry at $index: $err")
      appliedIndex = index
      sinceSnapshot += 1
      if snapshotEvery > 0 && sinceSnapshot >= snapshotEvery then
        sinceSnapshot = 0
        snapshot().left.foreach(why => System.err.println(s"raft-store $id: snapshot at $index refused: $why"))

  /** the local store's full history as one image at the last applied
   * index, handed to the node as its snapshot — the log up to that
   * index is then dropped. `Left` names why not: nothing applied yet,
   * or a partition whose `begin` is past 0 (its history is gone, and
   * a restore could not reproduce its offsets). Taken with the node
   * quiesced, so the index and the records agree. */
  def snapshot(): Either[String, Long] = node.quiesced {
    val at = appliedIndex
    if at == 0 then Left("nothing applied yet")
    else boundary[Either[String, Long]] {
      val declared = synchronized(byName.map(t => (t.name, t.partitions)))
      val topics = declared.map { (name, partitions) =>
        val mine = local.topic(name, partitions)
        val parts = (0 until partitions).toVector.map { p =>
          if mine.begin(p) != 0 then
            break(Left(s"topic $name partition $p begins at ${mine.begin(p)}: history before it is gone"))
          val out = Vector.newBuilder[Rec]
          var from = 0L
          var going = true
          while going do
            mine.read(p, from, 512) match
              case Topic.Read.TooEarly(b) => break(Left(s"topic $name partition $p: history before $b is gone"))
              case Topic.Read.Records(rs) =>
                if rs.isEmpty then going = false
                else
                  rs.foreach(r => out += Rec(r.offset, r.timestamp, r.key, r.value))
                  from = rs.last.offset + 1
          out.result()
        }
        TopicImage(name, partitions, parts)
      }
      if node.compact(at, Cbor.write(Image(at, topics))) then Right(at)
      else Left(s"the node refused to compact at $at (snapshot ${node.snapshotIndex}, commit ${node.commitIndex})")
    }
  }

  /** the wire node's restore seam (under its lock): a snapshot from
   * the leader — the image's records past this store's own `end`
   * are appended in offset order; what it already holds is a prefix
   * of the same history, by construction. A gap is `damaged`. */
  private def restore(index: Long, bytes: Array[Byte]): Unit =
    if index <= appliedIndex then ()   // already past it: nothing the image knows that we do not
    else Cbor.read[Image](bytes) match
      case Left(err) =>
        damaged = Some(s"unreadable snapshot at $index: $err")
        System.err.println(s"raft-store $id: ${damaged.get}")
      case Right(img) =>
        img.topics.foreach { t =>
          val mine = topic(t.name, t.partitions)   // declared here if not yet, with the default policy
          for p <- 0 until t.partitions do
            val end = mine.end(p)
            val fresh = t.parts(p).filter(_.offset >= end)
            fresh.headOption.filter(_.offset != end).foreach { r =>
              damaged = Some(s"topic ${t.name} partition $p: local end $end, the snapshot resumes at ${r.offset}")
              System.err.println(s"raft-store $id: ${damaged.get}")
            }
            if damaged.isEmpty then fresh.foreach { r =>
              val off = local.topic(t.name, t.partitions).append(p, r.key, r.value, Ack.Durable)
              if off != r.offset then
                damaged = Some(s"topic ${t.name} partition $p: appended at $off, the snapshot said ${r.offset}")
                System.err.println(s"raft-store $id: ${damaged.get}")
            }
        }
        if damaged.isEmpty then
          appliedIndex = index
          sinceSnapshot = 0

  private final class RaftTopic(val name: String, val partitions: Int) extends Topic:
    private def mine = local.topic(name, partitions)
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      val n = RaftStore.this.synchronized { seq += 1; seq }
      val p = Pending()
      pending.put(s"$id/$n", p)
      // on the leader this appends here; on a follower that knows its
      // leader it is carried there (persist-raft-forward), and either
      // way the answer is this node applying the committed entry
      val proposed = node.propose(n, okay.codec.Codecs.writeCbor(Op.Append(name, partition, key, value, id, n)))
      if !proposed then
        pending.remove(s"$id/$n")
        throw NotLeader(node.leaderId)
      if !p.done.await(commitWaitMs, TimeUnit.MILLISECONDS) then
        pending.remove(s"$id/$n")
        throw NotCommitted(name, partition)
      p.refused match
        case Some(leader) => throw NotLeader(leader)
        case None => p.offset
    def read(partition: Int, from: Long, max: Int): Topic.Read = mine.read(partition, from, max)
    def begin(partition: Int): Long = mine.begin(partition)
    def end(partition: Int): Long = mine.end(partition)
    def compact(partition: Int): Unit = mine.compact(partition)

  private var byName = Vector.empty[RaftTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized:
    byName.find(_.name == name) match
      case Some(t) =>
        if t.partitions != partitions then
          throw IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        local.topic(name, partitions, policy): Unit   // declared locally, same count on every node
        val t = new RaftTopic(name, partitions)
        byName :+= t
        t

  def topics: Vector[String] = synchronized(byName.map(_.name))
  def stats: Store.Stats = local.stats

  def isLeader: Boolean = node.isLeader
  def leaderId: Option[String] = node.leaderId
  def currentTerm: Long = node.currentTerm
  /** the cluster as this node currently counts it */
  def members: Set[String] = node.members
  /** the index the node's log is compacted to (0: never) */
  def snapshotIndex: Long = node.snapshotIndex
  /** the last Raft index applied to the local store */
  def applied: Long = appliedIndex
  /** a membership change: the whole new cluster with addresses;
   * accepted only on the leader with no earlier change pending —
   * see `RaftWire.Node.reconfigure` */
  def reconfigure(cluster: Map[String, (String, Int)]): Boolean = node.reconfigure(cluster)
  def close(): Unit = node.close()

object RaftStore:

  /** the one replicated operation; the proposer's id and sequence let
   * the proposing node recognise its own entry when it commits */
  enum Op derives Schema:
    case Append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte],
                proposer: String, n: Long)

  /** the store's snapshot (stage 2c): the local store's full history
   * at a Raft index — every declared topic, every partition, every
   * record with its offset (timestamps ride along; a restore's own
   * appends stamp anew) */
  final case class Rec(offset: Long, timestamp: Long, key: Array[Byte], value: Array[Byte]) derives Schema
  final case class TopicImage(name: String, partitions: Int, parts: Vector[Vector[Rec]]) derives Schema
  final case class Image(index: Long, topics: Vector[TopicImage]) derives Schema

  private final class Pending:
    val done = CountDownLatch(1)
    @volatile var offset = -1L
    /** set when the leader this was forwarded to refused it */
    @volatile var refused: Option[Option[String]] = None

  /** this node is not the leader; the leader, when known, is named */
  final case class NotLeader(leader: Option[String])
    extends RuntimeException(s"not the leader${leader.map(l => s" (leader: $l)").getOrElse("")}")

  /** the proposal did not commit in time: no majority reached */
  final case class NotCommitted(topic: String, partition: Int)
    extends RuntimeException(s"$topic/$partition: the proposal did not commit — no majority")

  /**
   * Start a node and the store over it. `local` holds the applied
   * state (a `MemoryStore` for a test, a file store for a service);
   * `stable` holds the term and the vote across a crash.
   */
  def start(id: String, port: Int, peers: Map[String, (String, Int)], local: Store,
            stable: RaftWire.Stable = RaftWire.Stable.memory(),
            tickMs: Long = 50, electionTimeoutMs: Long = 300, heartbeatMs: Long = 100,
            commitWaitMs: Long = 5000, snapshotEvery: Int = 0): RaftStore =
    val store = new RaftStore(id, local, commitWaitMs, snapshotEvery)
    store.wired = RaftWire.Node(id, port, peers, tickMs, electionTimeoutMs, heartbeatMs,
      onCommit = store.applied, stable = stable, onRefused = store.refused, onRestore = store.restore)
    store
