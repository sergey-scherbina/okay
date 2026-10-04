package okay2.persist

import okay2.codec.Schema

/**
 * Stage 2's core, transport-agnostic (okay-persist's Replicated.scala;
 * specs/persist.md): a coordinator over N replica `Store`s behind the
 * same `Topic` trait. Replication is a consumer that writes what it
 * reads, and none of the machinery (epochs, the high-water mark, quorum
 * acks, producer dedup) cares whether a replica lives in this process or
 * across a wire.
 *
 *  - reads NEVER serve past the high-water mark (the quorum-th largest
 *    replica end); `end` reports the hwm.
 *  - `Ack.Replicated` returns only when a QUORUM holds the record
 *    durably; short of quorum it throws `NoQuorum`.
 *  - appends go through a `Leader` handle bound to its epoch: `promote`
 *    advances the epoch and a deposed handle's append is FENCED, and the
 *    rejection is an ops event.
 *  - a follower that fails a push is counted out (its lag shows in
 *    stats) and caught up by `replicate`.
 */
final class Replicated private (val name: String, val partitions: Int,
                                policy: Policy, replicas: Vector[Store],
                                ops: Topic) extends Topic {
  import Replicated._

  private val topics: Vector[Topic] = replicas.map(_.topic(name, partitions, policy))
  private val opsTyped = Typed[Op](ops, version = 1, upcasts = Map.empty)

  private final class Part {
    var epoch = 1L
    var leader = 0
    var hwm = 0L
    /** last (seq, offset) per producer — the dedup window */
    var producers = Map.empty[String, (Long, Long)]
  }

  private val parts = Array.fill(partitions)(new Part)

  private def quorum: Int = replicas.length / 2 + 1

  // ── the Topic seam ─────────────────────────────────────────────

  def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
    append(leader(partition), key, value, ack)

  /** reads come from the current leader, truncated to the hwm */
  def read(partition: Int, from: Long, max: Int): Topic.Read = {
    val part = parts(partition)
    part.synchronized {
      topics(part.leader).read(partition, from, max) match {
        case Topic.Read.TooEarly(b) => Topic.Read.TooEarly(b)
        case Topic.Read.Records(rs) => Topic.Read.Records(rs.takeWhile(_.offset < part.hwm))
      }
    }
  }

  def begin(partition: Int): Long = {
    val part = parts(partition)
    part.synchronized(topics(part.leader).begin(partition))
  }

  /** the end a consumer can REACH: the high-water mark */
  def end(partition: Int): Long = {
    val part = parts(partition)
    part.synchronized(part.hwm)
  }

  /** compaction preserves latest-per-key on identical content, so every
   * replica compacts to the same view */
  def compact(partition: Int): Unit =
    parts(partition).synchronized(topics.foreach(_.compact(partition)))

  // ── the stage-2 surface ────────────────────────────────────────

  /** the current leadership handle; hold it across a promotion and your
   * next append is fenced */
  def leader(partition: Int): Leader = {
    val part = parts(partition)
    part.synchronized(Leader(partition, part.epoch))
  }

  /** the epoch-checked append: the only road records enter by */
  def append(l: Leader, key: Array[Byte], value: Array[Byte], ack: Ack): Long = {
    val part = parts(l.partition)
    part.synchronized {
      if (l.epoch != part.epoch) {
        opsTyped.append(0, opsKey, Op.FencedAppend(l.partition, l.epoch, part.epoch): Op, Ack.Durable)
        throw Fenced(l.partition, l.epoch, part.epoch)
      }

      val leaderAck = if (ack == Ack.Replicated) Ack.Durable else ack
      val off = topics(part.leader).append(l.partition, key, value, leaderAck)

      // the eager push: each follower is a consumer we run inline; one
      // that fails is counted out and caught up by replicate
      var durableCopies = 1
      for (i <- replicas.indices if i != part.leader) {
        try {
          val fo = topics(i).append(l.partition, key, value, leaderAck)
          if (fo != off) throw Diverged(l.partition, i, fo, off)
          durableCopies += 1
        } catch {
          case d: Diverged => throw d
          case _: Throwable => () // the follower's lag is visible in stats
        }
      }

      advanceHwm(part, l.partition)
      if (ack == Ack.Replicated && durableCopies < quorum) throw NoQuorum(l.partition, durableCopies, quorum)
      off
    }
  }

  /**
   * The idempotent producer window: a retry after a lost ack carries the
   * SAME (producerId, seq) and lands once — the answer is the ORIGINAL
   * offset. A seq below the remembered one is a replay from beyond the
   * window and refuses loudly.
   */
  def produce(partition: Int, producerId: String, seq: Long,
              key: Array[Byte], value: Array[Byte], ack: Ack): Long = {
    val part = parts(partition)
    part.synchronized {
      part.producers.get(producerId) match {
        case Some((last, off)) if seq == last => off        // the retry: dropped
        case Some((last, _)) if seq < last => throw ReplayBeyondWindow(producerId, seq, last)
        case _ =>
          val off = append(leader(partition), key, value, ack)
          part.producers = part.producers.updated(producerId, (seq, off))
          off
      }
    }
  }

  /** the operator's failover: promote a follower at the next epoch; the
   * promotion itself is an ops event */
  def promote(partition: Int, replica: Int): Unit = {
    require(replica >= 0 && replica < replicas.length, s"no replica $replica")
    val part = parts(partition)
    part.synchronized {
      replicate(partition)                    // catch the successor up first
      val from = part.leader
      part.epoch += 1
      part.leader = replica
      val _ = opsTyped.append(0, opsKey, Op.Promoted(partition, part.epoch, from, replica): Op, Ack.Durable)
    }
  }

  /** the pull: every follower reads what it lacks from the leader and
   * writes it; then the hwm advances to the quorum-th largest end */
  def replicate(partition: Int): Unit = {
    val part = parts(partition)
    part.synchronized {
      val lead = topics(part.leader)
      val leadEnd = lead.end(partition)
      for (i <- replicas.indices if i != part.leader) {
        var from = topics(i).end(partition)
        while (from < leadEnd) {
          lead.read(partition, from, 256) match {
            case Topic.Read.TooEarly(b) => from = b
            case Topic.Read.Records(rs) =>
              if (rs.isEmpty) from = leadEnd
              else {
                for (r <- rs) {
                  val fo = topics(i).append(partition, r.key, r.value, Ack.Durable)
                  if (fo != r.offset) throw Diverged(partition, i, fo, r.offset)
                }
                from = rs.last.offset + 1
              }
          }
        }
      }
      advanceHwm(part, partition)
    }
  }

  private def advanceHwm(part: Part, partition: Int): Unit = {
    val ends = topics.map(_.end(partition)).sorted(Ordering[Long].reverse)
    part.hwm = math.max(part.hwm, ends(quorum - 1))
  }

  /** per partition: epoch, leader, hwm, every replica's end and lag */
  def replicaStats: Stats =
    Stats(Vector.tabulate(partitions) { p =>
      val part = parts(p)
      part.synchronized {
        val ends = topics.map(_.end(p))
        PartitionState(p, part.epoch, part.leader, part.hwm,
          ends.zipWithIndex.map { case (e, i) => ReplicaState(i, e, ends(part.leader) - e) })
      }
    })
}

object Replicated {

  /** replicas are engine Stores; the ops topic records promotions and
   * fencings on the FIRST replica by default */
  def apply(name: String, partitions: Int, policy: Policy,
            replicas: Vector[Store], ops: Option[Topic] = None): Replicated = {
    require(replicas.nonEmpty, "replication needs at least one replica")
    new Replicated(name, partitions, policy, replicas, ops.getOrElse(replicas.head.topic("__ops", 1, Policy())))
  }

  /** the leadership handle: an append credential bound to its epoch */
  final case class Leader private[persist] (partition: Int, epoch: Long)

  /** the ops events — the log is its own audit trail */
  sealed trait Op
  object Op {
    final case class Promoted(partition: Int, epoch: Long, from: Int, to: Int) extends Op
    final case class FencedAppend(partition: Int, attempted: Long, current: Long) extends Op
    implicit lazy val schema: Schema[Op] = Schema.derived
  }

  private val opsKey = "replication".getBytes("UTF-8")

  final case class Fenced(partition: Int, attempted: Long, current: Long)
    extends RuntimeException(
      s"append fenced: partition $partition is at epoch $current, the handle is from $attempted — " +
        "a deposed leader may not write")

  final case class NoQuorum(partition: Int, copies: Int, needed: Int)
    extends RuntimeException(
      s"Ack.Replicated unreachable: $copies durable copies of a needed $needed on partition $partition")

  final case class ReplayBeyondWindow(producerId: String, seq: Long, last: Long)
    extends RuntimeException(
      s"producer $producerId replayed seq $seq behind the remembered $last — beyond the dedup window")

  final case class Diverged(partition: Int, replica: Int, at: Long, expected: Long)
    extends RuntimeException(
      s"replica $replica diverged on partition $partition: wrote offset $at where $expected was expected")

  final case class ReplicaState(replica: Int, end: Long, lag: Long)
  final case class PartitionState(partition: Int, epoch: Long, leader: Int, hwm: Long, replicas: Vector[ReplicaState])
  final case class Stats(partitions: Vector[PartitionState])
}
