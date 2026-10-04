package okay2.persist

import okay2.codec.{Cbor, Codecs, Schema}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

/**
 * The `Store` over a Raft-replicated log (okay-persist's RaftStore.scala;
 * specs/consensus.md, stage 1b): `Election` constructs a `Topic` over
 * this without changing, because this IS a `Store`.
 *
 * The replicated state machine of the paper: an append is PROPOSED as
 * one log entry (`Op.Append`, with the proposer's id and sequence), and
 * every node APPLIES each entry to its LOCAL store when the wire node
 * reports it committed, in log order — so the local stores agree by
 * construction, and a local read never shows a record a failover could
 * unwrite. An append on a follower is CARRIED to the leader; every append
 * waits for ITS entry to commit, and one that does not commit within
 * `commitWaitMs` throws `NotCommitted`.
 *
 * Topics are declared per node, not replicated: partition counts are
 * configuration.
 *
 * Stage 2c — the store's own snapshot: `snapshot()` writes the local
 * store's FULL history as one image at the last applied index and hands
 * it to the node's `compact`; a node behind a compacted stretch is
 * restored from it, the image's records past its own `end` appended in
 * offset order. Refused when a partition's `begin` is past 0; a gap
 * between a local `end` and the image is named in `damaged`, never
 * papered over. `snapshotEvery` > 0 takes one every that many entries.
 */
final class RaftStore private (val id: String, local: Store, commitWaitMs: Long, snapshotEvery: Int) extends Store {

  import RaftStore._

  private val pending = new ConcurrentHashMap[String, Pending]()
  /** the last Raft index applied to the local store. Written under the
   * node's lock only (the callbacks run there) */
  @volatile private var appliedIndex = 0L
  private var sinceSnapshot = 0
  /** a restore this store could not honour — an operator's matter, said
   * once and kept */
  @volatile var damaged: Option[String] = None
  // set once by `start`, after this store exists: the node's commit seam
  // is a method of the store, and the store cannot see the node before
  // it is built
  private var wired: Option[RaftWire.Node] = None
  private def node: RaftWire.Node = wired.getOrElse(throw new IllegalStateException(s"raft-store $id: not started"))
  private var seq = 0L

  /** a forwarded proposal the leader refused: fail the proposer's wait
   * with the leader it named */
  private def refused(n: Long, leader: Option[String]): Unit = {
    val p = pending.remove(s"$id/$n")
    if (p != null) { p.refused = Some(leader); p.done.countDown() }
  }

  private def complain(what: String): Unit = System.err.println(s"raft-store $id: $what")

  /** the wire node's commit seam (under its lock, in log order): apply,
   * then wake the proposer */
  private def applyCommitted(index: Long, entry: RaftEntry): Unit =
    // a re-application from a snapshot's edge: already in the store
    if (index > appliedIndex) {
      // a configuration entry is the cluster's business, and a leader's
      // blank no-op the log's own: nothing to apply
      if (entry.members.isEmpty && entry.data.nonEmpty)
        Codecs.readCbor[Op](entry.data) match {
          case Right(Op.Append(topic, partition, key, value, proposer, n)) =>
            val off = local.topic(topic).append(partition, key, value, Ack.Durable)
            val p = pending.remove(s"$proposer/$n")
            if (p != null) { p.offset = off; p.done.countDown() }
          case Left(err) =>
            // an entry this node cannot read is a bug in the writer: name
            // it and keep applying
            complain(s"unreadable entry at $index: $err")
        }
      appliedIndex = index
      sinceSnapshot += 1
      if (snapshotEvery > 0 && sinceSnapshot >= snapshotEvery) {
        sinceSnapshot = 0
        snapshot().left.foreach(why => complain(s"snapshot at $index refused: $why"))
      }
    }

  /** one partition's full history, or why it cannot be had */
  private def history(name: String, mine: Topic, p: Int): Either[String, Vector[Rec]] =
    if (mine.begin(p) != 0) Left(s"topic $name partition $p begins at ${mine.begin(p)}: history before it is gone")
    else {
      val out = Vector.newBuilder[Rec]
      var from = 0L
      var gone: Option[Long] = None
      var going = true
      while (going) {
        mine.read(p, from, 512) match {
          case Topic.Read.TooEarly(b) => gone = Some(b); going = false
          case Topic.Read.Records(rs) =>
            if (rs.isEmpty) going = false
            else {
              rs.foreach(r => out += Rec(r.offset, r.timestamp, r.key, r.value))
              from = rs.last.offset + 1
            }
        }
      }
      gone match {
        case Some(b) => Left(s"topic $name partition $p: history before $b is gone")
        case None => Right(out.result())
      }
    }

  /** the local store's full history as one image at the last applied
   * index, handed to the node as its snapshot. `Left` names why not.
   * Taken with the node quiesced, so the index and the records agree */
  def snapshot(): Either[String, Long] = node.quiesced {
    val at = appliedIndex
    if (at == 0) Left("nothing applied yet")
    else {
      val declared = synchronized(byName.map(t => (t.name, t.partitions)))
      val images = declared.foldLeft[Either[String, Vector[TopicImage]]](Right(Vector.empty)) {
        case (acc, (name, partitions)) =>
          acc.flatMap { done =>
            val mine = local.topic(name, partitions)
            val parts = (0 until partitions).foldLeft[Either[String, Vector[Vector[Rec]]]](Right(Vector.empty)) {
              (pa, p) => pa.flatMap(ps => history(name, mine, p).map(ps :+ _))
            }
            parts.map(ps => done :+ TopicImage(name, partitions, ps))
          }
      }
      images.flatMap { topics =>
        if (node.compact(at, Cbor.write(Image(at, topics)))) Right(at)
        else Left(s"the node refused to compact at $at (snapshot ${node.snapshotIndex}, commit ${node.commitIndex})")
      }
    }
  }

  /** the wire node's restore seam (under its lock): the image's records
   * past this store's own `end` are appended in offset order. A gap is
   * `damaged` */
  private def restore(index: Long, bytes: Array[Byte]): Unit =
    if (index > appliedIndex) Cbor.read[Image](bytes) match {
      case Left(err) =>
        damaged = Some(s"unreadable snapshot at $index: $err")
        complain(damaged.get)
      case Right(img) =>
        img.topics.foreach { t =>
          val mine = topic(t.name, t.partitions)   // declared here if not yet, with the default policy
          for (p <- 0 until t.partitions) {
            val end = mine.end(p)
            val fresh = t.parts(p).filter(_.offset >= end)
            fresh.headOption.filter(_.offset != end).foreach { r =>
              damaged = Some(s"topic ${t.name} partition $p: local end $end, the snapshot resumes at ${r.offset}")
              complain(damaged.get)
            }
            if (damaged.isEmpty) fresh.foreach { r =>
              val off = local.topic(t.name, t.partitions).append(p, r.key, r.value, Ack.Durable)
              if (off != r.offset) {
                damaged = Some(s"topic ${t.name} partition $p: appended at $off, the snapshot said ${r.offset}")
                complain(damaged.get)
              }
            }
          }
        }
        if (damaged.isEmpty) {
          appliedIndex = index
          sinceSnapshot = 0
        }
    }

  private final class RaftTopic(val name: String, val partitions: Int) extends Topic {
    private def mine = local.topic(name, partitions)
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long = {
      val n = RaftStore.this.synchronized { seq += 1; seq }
      val p = new Pending
      pending.put(s"$id/$n", p)
      // on the leader this appends here; on a follower that knows its
      // leader it is carried there, and either way the answer is this
      // node applying the committed entry
      val proposed = node.propose(n, Codecs.writeCbor[Op](Op.Append(name, partition, key, value, id, n)))
      if (!proposed) {
        pending.remove(s"$id/$n")
        throw NotLeader(node.leaderId)
      }
      if (!p.done.await(commitWaitMs, TimeUnit.MILLISECONDS)) {
        pending.remove(s"$id/$n")
        throw NotCommitted(name, partition)
      }
      p.refused match {
        case Some(leader) => throw NotLeader(leader)
        case None => p.offset
      }
    }
    def read(partition: Int, from: Long, max: Int): Topic.Read = mine.read(partition, from, max)
    def begin(partition: Int): Long = mine.begin(partition)
    def end(partition: Int): Long = mine.end(partition)
    def compact(partition: Int): Unit = mine.compact(partition)
  }

  private var byName = Vector.empty[RaftTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized {
    byName.find(_.name == name) match {
      case Some(t) =>
        if (t.partitions != partitions)
          throw new IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        local.topic(name, partitions, policy)   // declared locally, same count on every node
        val t = new RaftTopic(name, partitions)
        byName :+= t
        t
    }
  }

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
  /** a membership change: see `RaftWire.Node.reconfigure` */
  def reconfigure(cluster: Map[String, (String, Int)]): Boolean = node.reconfigure(cluster)
  def close(): Unit = node.close()
}

object RaftStore {

  /** the one replicated operation; the proposer's id and sequence let the
   * proposing node recognise its own entry when it commits */
  sealed trait Op
  object Op {
    final case class Append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte],
                            proposer: String, n: Long) extends Op
    implicit lazy val schema: Schema[Op] = Schema.derived
  }

  /** the store's snapshot (stage 2c): the local store's full history at
   * a Raft index */
  final case class Rec(offset: Long, timestamp: Long, key: Array[Byte], value: Array[Byte])
  final case class TopicImage(name: String, partitions: Int, parts: Vector[Vector[Rec]])
  final case class Image(index: Long, topics: Vector[TopicImage])
  implicit lazy val recSchema: Schema[Rec] = Schema.derived
  implicit lazy val topicImageSchema: Schema[TopicImage] = Schema.derived
  implicit lazy val imageSchema: Schema[Image] = Schema.derived

  private final class Pending {
    val done = new CountDownLatch(1)
    @volatile var offset = -1L
    /** set when the leader this was forwarded to refused it */
    @volatile var refused: Option[Option[String]] = None
  }

  /** this node is not the leader; the leader, when known, is named */
  final case class NotLeader(leader: Option[String])
    extends RuntimeException(s"not the leader${leader.map(l => s" (leader: $l)").getOrElse("")}")

  /** the proposal did not commit in time: no majority reached */
  final case class NotCommitted(topic: String, partition: Int)
    extends RuntimeException(s"$topic/$partition: the proposal did not commit — no majority")

  /** start a node and the store over it. `local` holds the applied state;
   * `stable` holds the term and the vote across a crash */
  def start(id: String, port: Int, peers: Map[String, (String, Int)], local: Store,
            stable: RaftWire.Stable = RaftWire.Stable.memory(),
            tickMs: Long = 50, electionTimeoutMs: Long = 300, heartbeatMs: Long = 100,
            commitWaitMs: Long = 5000, snapshotEvery: Int = 0): RaftStore = {
    val store = new RaftStore(id, local, commitWaitMs, snapshotEvery)
    store.wired = Some(new RaftWire.Node(id, port, peers, tickMs, electionTimeoutMs, heartbeatMs,
      onCommit = store.applyCommitted, stable = stable, onRefused = store.refused, onRestore = store.restore))
    store
  }
}
