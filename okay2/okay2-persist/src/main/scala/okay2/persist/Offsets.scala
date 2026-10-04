package okay2.persist

import scala.collection.immutable.ArraySeq

/**
 * Consumer offsets as records (okay-persist's Offsets.scala): a consumer
 * commits its position AS A RECORD to an offsets topic — the log's own
 * machinery, no second store to make durable. The topic is keyed (group,
 * topic and partition joined by NUL) and compacted; on construction it
 * is folded once into memory, and a restart refolds — the
 * resume-from-commit contract.
 */
final class Offsets(val topic: Topic) {

  private var committedByKey: Map[ArraySeq[Byte], Long] = fold()

  private def fold(): Map[ArraySeq[Byte], Long] = {
    var acc = Map.empty[ArraySeq[Byte], Long]
    for (p <- 0 until topic.partitions) {
      var from = topic.begin(p)
      var going = true
      while (going) {
        topic.read(p, from, 512) match {
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if (rs.isEmpty) going = false
            else {
              for (r <- rs if r.value.length == 8)
                acc = acc.updated(ArraySeq.unsafeWrapArray(r.key), longOf(r.value))
              from = rs.last.offset + 1
            }
        }
      }
    }
    acc
  }

  def commit(group: String, topicName: String, partition: Int, offset: Long, ack: Ack = Ack.Durable): Unit =
    synchronized {
      val key = keyOf(group, topicName, partition)
      topic.append(key, bytesOf(offset), ack): Unit
      committedByKey = committedByKey.updated(ArraySeq.unsafeWrapArray(key), offset)
    }

  /** the last committed NEXT-offset for this group and partition */
  def committed(group: String, topicName: String, partition: Int): Option[Long] =
    synchronized(committedByKey.get(ArraySeq.unsafeWrapArray(keyOf(group, topicName, partition))))

  /** end minus committed, summed over partitions; an uncommitted
   * partition counts from `begin` */
  def lag(group: String, of: Topic): Long =
    (0 until of.partitions).map { p =>
      val at = committed(group, of.name, p).getOrElse(of.begin(p))
      math.max(0L, of.end(p) - at)
    }.sum

  private def keyOf(group: String, topicName: String, partition: Int): Array[Byte] =
    s"$group\u0000$topicName\u0000$partition".getBytes("UTF-8")

  private def bytesOf(offset: Long): Array[Byte] = {
    val out = new Array[Byte](8)
    var i = 0
    while (i < 8) {
      out(i) = (offset >> (56 - i * 8)).toByte
      i += 1
    }
    out
  }

  private def longOf(bs: Array[Byte]): Long = {
    var v = 0L
    var i = 0
    while (i < 8) {
      v = (v << 8) | (bs(i) & 0xffL)
      i += 1
    }
    v
  }
}

object Offsets {
  /** the conventional topic; one partition is plenty for commits */
  def apply(store: Store, name: String = "__offsets"): Offsets =
    new Offsets(store.topic(name, 1, Policy(compact = true)))
}
