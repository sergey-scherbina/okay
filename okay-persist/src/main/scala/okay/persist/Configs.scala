package okay.persist

import okay.codec.{Json, Schema}

/**
 * Managed configuration as one more consumer of the one primitive
 * (specs/conf.md, stage 2): a compacted keyed topic where key = the
 * config's name and value = its Schema's JSON — a config is for
 * looking at, and the log is one more place it gets looked at. With
 * nothing built: history of every change IS the log, "who changed
 * what when" is a read, rollback is reading an older offset, and
 * `latest` is the same compacted-topic story Snapshots already
 * tells. Reference-only safety is specs/conf.md invariant 3 by
 * construction: Schema[Secret] encodes the reference.
 *
 * History is honest about compaction: the audit lives until
 * `Topic.compact` reclaims superseded writes; `latest` still answers
 * after it — the latest per key is exactly what compaction keeps.
 */
final class Configs(val topic: Topic):

  private def keyOf(name: String): Array[Byte] = name.getBytes("UTF-8")

  /** one write is one config version; the offset is its identity */
  def put[C](name: String, value: C, ack: Ack = Ack.Durable)(using Schema[C]): Long =
    topic.append(keyOf(name), Json.write(value).getBytes("UTF-8"), ack)

  /** every surviving write under this name, oldest first, each with
   * its offset; a damaged value is a Left in place, the rest intact */
  def history[C](name: String)(using Schema[C]): Vector[(Long, Either[String, C])] =
    val key = keyOf(name)
    val p = Topic.route(key, topic.partitions)
    val out = Vector.newBuilder[(Long, Either[String, C])]
    var from = topic.begin(p)
    var going = true
    while going do
      topic.read(p, from, 512) match
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            rs.iterator.filter(_.key.sameElements(key)).foreach { r =>
              out += ((r.offset, Json.read[C](String(r.value, "UTF-8"))))
            }
            from = rs.last.offset + 1
    out.result()

  /** the current config — the newest write under the name */
  def latest[C](name: String)(using Schema[C]): Option[(Long, Either[String, C])] =
    history[C](name).lastOption

  /** rollback IS a read: the newest write at or before `offset` */
  def at[C](name: String, offset: Long)(using Schema[C]): Option[(Long, Either[String, C])] =
    history[C](name).takeWhile(_._1 <= offset).lastOption

object Configs:
  /** the conventional topic: keyed, compacted */
  def apply(store: Store, name: String = "__configs", partitions: Int = 1): Configs =
    new Configs(store.topic(name, partitions, Policy(compact = true)))

  /** the ambient-Store door (ctx-everywhere) */
  def ambient(name: String = "__configs", partitions: Int = 1)(using store: Store): Configs =
    apply(store, name, partitions)
