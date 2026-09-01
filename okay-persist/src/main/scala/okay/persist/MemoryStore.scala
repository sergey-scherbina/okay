package okay.persist

/**
 * The same trait in memory: tests, and short interactive runs where
 * a durable journal is overhead (specs/llm-agentic.md: durability is
 * switched on for a run, not paid always).
 *
 * Semantics mirror the file engine — dense offsets, `begin` moving
 * under retention, `TooEarly` on reads before it — so a consumer
 * developed against memory meets no surprises on disk. Retention
 * granularity is the one honest difference: here a record is its own
 * "segment", so `begin` advances a record at a time where the file
 * engine drops whole segment files.
 */
final class MemoryStore extends Store:

  private final class Part:
    var records = Vector.empty[Record]
    var base = 0L
    var next = 0L
    var bytes = 0L

  private final class MemTopic(val name: String, val partitions: Int,
                               policy: Policy) extends Topic:
    val parts = Array.fill(partitions)(new Part)

    def append(partition: Int, key: Array[Byte], value: Array[Byte], ack: Ack): Long =
      val part = parts(partition)
      part.synchronized:
        val off = part.next
        part.records :+= Record(off, System.currentTimeMillis(), key, value)
        part.next += 1
        part.bytes += key.length.toLong + value.length + frameOverhead
        // retention and compaction are exclusive (Policy): dropping
        // from the front of a compacted topic would delete keys
        while !policy.compact && part.bytes > policy.retainBytes && part.records.length > 1 do
          val dropped = part.records.head
          part.records = part.records.tail
          part.base = part.records.head.offset
          part.bytes -= dropped.key.length.toLong + dropped.value.length + frameOverhead
        off

    def read(partition: Int, from: Long, max: Int): Topic.Read =
      val part = parts(partition)
      part.synchronized:
        if from < part.base then Topic.Read.TooEarly(part.base)
        else
          // by offset, not by position: compaction leaves gaps
          Topic.Read.Records(part.records.dropWhile(_.offset < from).take(max))

    def compact(partition: Int): Unit =
      val part = parts(partition)
      part.synchronized:
        val latest = part.records.map(r => keyOf(r.key) -> r.offset).toMap
        part.records = part.records.filter(r => latest(keyOf(r.key)) == r.offset)
        part.bytes = part.records
          .map(r => r.key.length.toLong + r.value.length + frameOverhead).sum

    def begin(partition: Int): Long = parts(partition).synchronized(parts(partition).base)
    def end(partition: Int): Long = parts(partition).synchronized(parts(partition).next)

  /** what a record costs beyond its bytes — kept equal to the file
   * frame overhead so retention arithmetic agrees across engines */
  private val frameOverhead = 28L

  /** an equality-honest view of key bytes, for the per-key latest map */
  private def keyOf(key: Array[Byte]): scala.collection.immutable.ArraySeq[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(key)

  private var byName = Vector.empty[MemTopic]

  def topic(name: String, partitions: Int, policy: Policy): Topic = synchronized:
    byName.find(_.name == name) match
      case Some(t) =>
        if t.partitions != partitions then
          throw IllegalArgumentException(
            s"topic $name has ${t.partitions} partitions; asked for $partitions — " +
              "rerouting keys would break per-key order")
        t
      case None =>
        val t = new MemTopic(name, partitions, policy)
        byName :+= t
        t

  def topics: Vector[String] = synchronized(byName.map(_.name))

  def stats: Store.Stats = synchronized:
    Store.Stats(byName.map { t =>
      Store.TopicStats(t.name, Vector.tabulate(t.partitions) { p =>
        val part = t.parts(p)
        part.synchronized:
          Store.PartitionStats(p, part.base, part.next, part.bytes, part.records.length)
      })
    })
