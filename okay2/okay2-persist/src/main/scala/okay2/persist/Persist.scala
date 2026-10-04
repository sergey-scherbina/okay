package okay2.persist

/**
 * The durable log (okay-persist's Persist.scala, specs/persist.md): a
 * named, partitioned, append-only log of records, with offsets as resume
 * tokens. BYTES in the engine, Schema/CBOR at the edge (`Typed`). Traits,
 * not effect rows: the log is infrastructure a HANDLER owns.
 *
 * okay2-persist holds the log's primitives — Record, Ack, Policy, Topic,
 * Store, the typed view, Offsets, Snapshots and the memory engine, what
 * okay2-jdbc's Writes, Poll and SqlStore stand on — and the rest of
 * okay-persist on top (specs/okay2.md stage 56): Configs, the streaming
 * reads, the file engine, replication, election and Raft, the wire, and
 * the durable workflow over the log.
 */

/** what the log stores; `key` may be empty (unkeyed append). The offset
 * is assigned by the log, dense within a partition — THE resume token */
final case class Record(offset: Long, timestamp: Long, key: Array[Byte], value: Array[Byte])

/** the durability DECISION, per append: `Received` (memory), `Durable`
 * (local disk before returning), `Replicated` (a quorum's disks) */
sealed trait Ack

object Ack {
  case object Received extends Ack
  case object Durable extends Ack
  case object Replicated extends Ack
}

/** per-topic policy. `compact` and retention are exclusive: a compacted
 * topic never drops from the front, so `compact = true` switches
 * `retainBytes` off and `Topic.compact` is the space reclaimer */
final case class Policy(segmentBytes: Long = 64L * 1024 * 1024,
                        retainBytes: Long = Long.MaxValue,
                        compact: Boolean = false,
                        replicas: Int = 1)

object Policy {
  val default: Policy = Policy()
}

/** one topic, already resolved; the partition-addressed methods are the
 * core, the keyed `append` is the sharding convenience */
trait Topic {
  def name: String
  def partitions: Int

  /** appends to one partition; returns the record's offset, which is
   * dense: this call's offset plus one is the next one's */
  def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long

  /** total: damage and dropped history are answers, never throws */
  def read(partition: Int, from: Long, max: Int): Topic.Read

  /** the first retained offset (retention moves it forward) */
  def begin(partition: Int): Long

  /** the next offset to be assigned (offsets between `begin` and `end`
   * may have gaps once compaction has run) */
  def end(partition: Int): Long

  /** keep the latest record per key: offsets and `begin` preserved,
   * `end` unmoved — the sequence just grows holes */
  def compact(partition: Int): Unit

  /** keyed convenience: route by key hash — same key, same partition */
  def append(key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Durable): Long =
    append(Topic.route(key, partitions), key, value, ack)
}

object Topic {

  /** a read names its failure instead of returning silence */
  sealed trait Read

  object Read {
    final case class Records(records: Vector[Record]) extends Read
    final case class TooEarly(begin: Long) extends Read
  }

  /** FNV-1a over the key bytes: stable across platforms, processes and
   * runs, so the same key lands in the same partition on every node */
  def route(key: Array[Byte], partitions: Int): Int = {
    var h = 0x811c9dc5
    var i = 0
    while (i < key.length) {
      h = (h ^ (key(i) & 0xff)) * 0x01000193
      i += 1
    }
    math.floorMod(h, partitions)
  }
}

/** a named collection of topics behind one engine, chosen at
 * construction and invisible above */
trait Store {
  /** resolves or creates; an existing topic keeps its partition count —
   * asking for a different one is refused loudly */
  def topic(name: String, partitions: Int = 1, policy: Policy = Policy.default): Topic
  def topics: Vector[String]

  /** observability as plain values */
  def stats: Store.Stats
}

object Store {
  final case class PartitionStats(partition: Int, begin: Long, end: Long, bytes: Long, segments: Int)
  final case class TopicStats(name: String, partitions: Vector[PartitionStats])
  final case class Stats(topics: Vector[TopicStats])
}
