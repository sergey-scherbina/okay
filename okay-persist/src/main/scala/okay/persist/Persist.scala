package okay.persist

/**
 * The durable log (specs/persist.md): a named, partitioned,
 * append-only log of records, with offsets as resume tokens.
 *
 * The layering rule: BYTES in the engine, Schema/CBOR at the edge —
 * the engine must not care what it stores, or it can never be
 * generic over its consumers (the okay-rag `Persist` layering, one
 * level down). Traits, not effect rows: the log is infrastructure a
 * HANDLER owns, not something programs request — `Durable.tools`
 * takes its journal as a plain argument, and that precedent holds.
 *
 * The partition is the unit of ORDER, of durability, and of
 * replication. Offsets are dense within a partition and mean nothing
 * across partitions; the same key always routes to the same
 * partition, so per-key order is free.
 */

/** what the log stores; `key` may be empty (unkeyed append). The
 * offset is assigned by the log, dense within a partition — it is
 * THE resume token (the Durable seq, the SSE Last-Event-ID, the UI
 * refold position are all this number). */
final case class Record(offset: Long, timestamp: Long,
                        key: Array[Byte], value: Array[Byte])

/**
 * The durability DECISION, per append. A guarantee is not on offer —
 * a named choice is (the `Durable.OnRepeat` pattern):
 *
 *   - `Received`   in the engine's memory; fastest, lost on crash
 *   - `Durable`    on the local disk (fsync'd) before returning
 *   - `Replicated` on a quorum of replicas' disks (stage 2; with a
 *                  replication factor of one it means `Durable`)
 */
enum Ack:
  case Received, Durable, Replicated

/** per-topic policy: an agent journal (retain forever, never
 * compact) and a metrics stream (a day, who cares) are both honest
 * topics. `compact` and retention are exclusive: a compacted topic
 * never drops segments from the front — that would delete the
 * latest value of every quiet key, the exact records compaction
 * keeps — so `compact = true` switches `retainBytes` off and
 * `Topic.compact` is the space reclaimer. `replicas` is declared
 * now, honored from stage 2 (specs/persist.md, Staging). */
final case class Policy(segmentBytes: Long = 64L * 1024 * 1024,
                        retainBytes: Long = Long.MaxValue,
                        compact: Boolean = false,
                        replicas: Int = 1)

object Policy:
  val default: Policy = Policy()

/** one topic, already resolved; the partition-addressed methods are
 * the core, the keyed `append` is the sharding convenience */
trait Topic:
  def name: String
  def partitions: Int

  /** appends to one partition; returns the record's offset, which is
   * dense: this call's offset plus one is the next one's */
  def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long

  /** total: damage and dropped history are answers, never throws */
  def read(partition: Int, from: Long, max: Int): Topic.Read

  /** the first retained offset (retention moves it forward) */
  def begin(partition: Int): Long

  /** the next offset to be assigned; a reader that has consumed to
   * `end` is caught up (offsets between `begin` and `end` may have
   * gaps once compaction has run) */
  def end(partition: Int): Long

  /** keep the latest record per key, dropping the superseded ones
   * (the force-compact admin call): offsets and `begin` are
   * preserved, no surviving record changes, `end` does not move —
   * the sequence just grows holes. A fold from `begin` of the
   * compacted partition equals the fold of the full history for any
   * last-write-wins-per-key fold, which is the snapshot story.
   * Meaningful for keyed topics; unkeyed records share the empty
   * key and collapse to the last one. */
  def compact(partition: Int): Unit

  /** keyed convenience: route by key hash — same key, same
   * partition, per-key order for free */
  def append(key: Array[Byte], value: Array[Byte], ack: Ack = Ack.Durable): Long =
    append(Topic.route(key, partitions), key, value, ack)

object Topic:

  /** a read names its failure instead of returning silence: asking
   * for history that retention dropped is an answer, not an empty
   * vector pretending nothing was ever there */
  enum Read:
    case Records(records: Vector[Record])
    case TooEarly(begin: Long)

  /** FNV-1a over the key bytes: stable across platforms, processes
   * and runs (no identity hashes, no JVM-specific `hashCode`), so
   * the same key lands in the same partition on every node — the
   * property sharding stands on */
  def route(key: Array[Byte], partitions: Int): Int =
    var h = 0x811c9dc5
    var i = 0
    while i < key.length do
      h = (h ^ (key(i) & 0xff)) * 0x01000193
      i += 1
    math.floorMod(h, partitions)

/** a named collection of topics behind one engine (memory, file,
 * replicated, interop) chosen at construction and invisible above */
trait Store:
  /** resolves or creates; an existing topic keeps its partition
   * count — asking for a different one is refused loudly, because
   * silently rerouting keys would break per-key order */
  def topic(name: String, partitions: Int = 1, policy: Policy = Policy.default): Topic
  def topics: Vector[String]

  /** observability as plain values (specs/persist.md, Operations):
   * serves equally as a JSON endpoint, a log line, or a test
   * assertion — no metrics framework, an exporter is a consumer */
  def stats: Store.Stats

object Store:
  final case class PartitionStats(partition: Int, begin: Long, end: Long,
                                  bytes: Long, segments: Int)
  final case class TopicStats(name: String, partitions: Vector[PartitionStats])
  final case class Stats(topics: Vector[TopicStats])
