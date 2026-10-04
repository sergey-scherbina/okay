package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * Managed configuration as one more consumer of the one primitive
 * (okay-persist's Configs.scala; specs/conf.md, stage 2): a compacted
 * keyed topic where key = the config's name and value = its Schema's
 * JSON. History of every change IS the log, "who changed what when" is a
 * read, rollback is reading an older offset, and `latest` is the
 * compacted-topic story Snapshots already tells. The audit lives until
 * `Topic.compact` reclaims superseded writes; `latest` still answers.
 */
final class Configs(val topic: Topic) {

  private def keyOf(name: String): Array[Byte] = name.getBytes("UTF-8")

  /** one write is one config version; the offset is its identity */
  def put[C](name: String, value: C, ack: Ack = Ack.Durable)(implicit s: Schema[C]): Long =
    topic.append(keyOf(name), Codecs.writeJson(value).getBytes("UTF-8"), ack)

  /** every surviving write under this name, oldest first, each with its
   * offset; a damaged value is a Left in place, the rest intact */
  def history[C](name: String)(implicit s: Schema[C]): Vector[(Long, Either[String, C])] = {
    val key = keyOf(name)
    val p = Topic.route(key, topic.partitions)
    val out = Vector.newBuilder[(Long, Either[String, C])]
    var from = topic.begin(p)
    var going = true
    while (going) {
      topic.read(p, from, 512) match {
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else {
            rs.iterator.filter(_.key.sameElements(key)).foreach { r =>
              out += ((r.offset, Codecs.readJson[C](new String(r.value, "UTF-8"))))
            }
            from = rs.last.offset + 1
          }
      }
    }
    out.result()
  }

  /** the current config — the newest write under the name */
  def latest[C](name: String)(implicit s: Schema[C]): Option[(Long, Either[String, C])] =
    history[C](name).lastOption

  /** rollback IS a read: the newest write at or before `offset` */
  def at[C](name: String, offset: Long)(implicit s: Schema[C]): Option[(Long, Either[String, C])] =
    history[C](name).takeWhile(_._1 <= offset).lastOption
}

object Configs {
  /** the conventional topic: keyed, compacted */
  def apply(store: Store, name: String = "__configs", partitions: Int = 1): Configs =
    new Configs(store.topic(name, partitions, Policy(compact = true)))

  /** the ambient-Store door: the store as an implicit */
  def ambient(name: String = "__configs", partitions: Int = 1)(implicit store: Store): Configs =
    apply(store, name, partitions)
}
