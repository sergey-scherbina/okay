package okay.persist

import okay.codec.{Cbor, Schema}
import java.util.concurrent.{ConcurrentHashMap, CountDownLatch, TimeUnit}

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
 * `append` on a follower answers `NotLeader(leaderId)` rather than
 * forwarding (stage 1a's stated limit, unchanged here): the caller
 * retries at the leader the way the two-node demo's 503 names it.
 * On the leader it waits for ITS entry to commit and returns the
 * local offset that entry was applied at — `Ack.None` waits too,
 * because an offset that is not yet an offset is not an answer; a
 * proposal that does not commit within `commitWaitMs` (a lost
 * majority) throws `NotCommitted` rather than pretending.
 *
 * Topics are declared per node, not replicated: partition counts are
 * configuration, agreed the way a cluster's addresses are.
 */
final class RaftStore private (val id: String, local: Store, commitWaitMs: Long) extends Store:

  import RaftStore.*

  private val pending = ConcurrentHashMap[String, Pending]()
  // set once by `start`, after this store exists: the node's commit
  // seam is a method of the store, and the store cannot see the node
  // before it is built
  private var wired: RaftWire.Node | Null = null
  private def node: RaftWire.Node = wired.nn
  private var seq = 0L

  /** the wire node's commit seam: apply, then wake the proposer */
  private def applied(index: Long, entry: RaftEntry): Unit =
    Cbor.read[Op](entry.data) match
      case Right(Op.Append(topic, partition, key, value, proposer, n)) =>
        val off = local.topic(topic).append(partition, key, value, Ack.Durable)
        val p = pending.remove(s"$proposer/$n")
        if p != null then { p.nn.offset = off; p.nn.done.countDown() }
      case Left(err) =>
        // an entry this node cannot read is a bug in the writer, not a
        // reason to diverge silently: name it and keep applying
        System.err.println(s"raft-store $id: unreadable entry at $index: $err")

  private final class RaftTopic(val name: String, val partitions: Int) extends Topic:
    private def mine = local.topic(name, partitions)
    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      val n = RaftStore.this.synchronized { seq += 1; seq }
      val p = Pending()
      pending.put(s"$id/$n", p)
      val proposed = node.propose(Cbor.write(Op.Append(name, partition, key, value, id, n)))
      if !proposed then
        pending.remove(s"$id/$n")
        throw NotLeader(node.leaderId)
      if !p.done.await(commitWaitMs, TimeUnit.MILLISECONDS) then
        pending.remove(s"$id/$n")
        throw NotCommitted(name, partition)
      p.offset
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
  def close(): Unit = node.close()

object RaftStore:

  /** the one replicated operation; the proposer's id and sequence let
   * the proposing node recognise its own entry when it commits */
  enum Op derives Schema:
    case Append(topic: String, partition: Int, key: Array[Byte], value: Array[Byte],
                proposer: String, n: Long)

  private final class Pending:
    val done = CountDownLatch(1)
    @volatile var offset = -1L

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
            commitWaitMs: Long = 5000): RaftStore =
    val store = new RaftStore(id, local, commitWaitMs)
    store.wired = RaftWire.Node(id, port, peers, tickMs, electionTimeoutMs, heartbeatMs,
      onCommit = store.applied, stable = stable)
    store
