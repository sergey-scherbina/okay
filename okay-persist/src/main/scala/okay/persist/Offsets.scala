package okay.persist

import scala.collection.immutable.ArraySeq

/**
 * Consumer offsets as records (specs/persist.md, Delivery
 * semantics): a consumer commits its position AS A RECORD to an
 * offsets topic — the log's own machinery, no second store to make
 * durable. Commit after processing is at-least-once, before is
 * at-most-once; the consumer picks per its own idempotency.
 *
 * The topic is keyed (group, topic and partition
 * joined by NUL, a byte no honest name contains) and
 * compacted: the latest commit per key is the only one that
 * matters, and `Topic.compact` reclaims the rest. On construction
 * the topic is folded once into memory; commits update both. A
 * restart constructs anew and refolds — which is exactly the
 * resume-from-commit contract.
 */
final class Offsets(val topic: Topic):

  private var committedByKey: Map[ArraySeq[Byte], Long] = fold()

  private def fold(): Map[ArraySeq[Byte], Long] =
    var acc = Map.empty[ArraySeq[Byte], Long]
    for p <- 0 until topic.partitions do
      var from = topic.begin(p)
      var going = true
      while going do
        topic.read(p, from, 512) match
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              for r <- rs if r.value.length == 8 do
                acc = acc.updated(ArraySeq.unsafeWrapArray(r.key), longOf(r.value))
              from = rs.last.offset + 1
    acc

  def commit(group: String, topicName: String, partition: Int, offset: Long,
             ack: Ack = Ack.Durable): Unit = synchronized:
    val key = keyOf(group, topicName, partition)
    topic.append(key, bytesOf(offset), ack): Unit
    committedByKey = committedByKey.updated(ArraySeq.unsafeWrapArray(key), offset)

  /** the last committed NEXT-offset for this group and partition —
   * the Last-Event-ID shape: resume reading from exactly here */
  def committed(group: String, topicName: String, partition: Int): Option[Long] =
    synchronized(committedByKey.get(ArraySeq.unsafeWrapArray(keyOf(group, topicName, partition))))

  /** end minus committed, summed over partitions — THE number that
   * says a consumer is drowning; an uncommitted partition counts
   * from `begin` (everything retained is unread) */
  def lag(group: String, of: Topic): Long =
    (0 until of.partitions).map { p =>
      val at = committed(group, of.name, p).getOrElse(of.begin(p))
      math.max(0L, of.end(p) - at)
    }.sum

  private def keyOf(group: String, topicName: String, partition: Int): Array[Byte] =
    s"$group\u0000$topicName\u0000$partition".getBytes("UTF-8")

  private def bytesOf(offset: Long): Array[Byte] =
    val out = new Array[Byte](8)
    var i = 0
    while i < 8 do
      out(i) = (offset >> (56 - i * 8)).toByte
      i += 1
    out

  private def longOf(bs: Array[Byte]): Long =
    var v = 0L
    var i = 0
    while i < 8 do
      v = (v << 8) | (bs(i) & 0xffL)
      i += 1
    v

object Offsets:
  /** the conventional topic; one partition is plenty for commits */
  def apply(store: Store, name: String = "__offsets"): Offsets =
    new Offsets(store.topic(name, 1, Policy(compact = true)))
