package okay2.persist

import okay2.codec.{Cbor, Schema}

/**
 * The put/latest convenience (okay-persist's Snapshots.scala): a snapshot
 * store IS a compacted keyed topic — refold starts from the snapshot's
 * offset, and `Topic.compact` reclaims the superseded states. `latest`
 * scans the key's partition, bounded by what compaction left.
 */
final class Snapshots(val topic: Topic) {

  def put(key: Array[Byte], state: Array[Byte], ack: Ack = Ack.Durable): Long =
    topic.append(key, state, ack)

  /** the newest record under this key, with its offset */
  def latest(key: Array[Byte]): Option[Record] = {
    val p = Topic.route(key, topic.partitions)
    var found: Option[Record] = None
    var from = topic.begin(p)
    var going = true
    while (going) {
      topic.read(p, from, 512) match {
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else {
            rs.reverseIterator.find(_.key.sameElements(key)).foreach(r => found = Some(r))
            from = rs.last.offset + 1
          }
      }
    }
    found
  }

  /** the Schema'd pair; decode damage is data, never a throw */
  def putValue[S](key: Array[Byte], state: S, ack: Ack = Ack.Durable)(implicit s: Schema[S]): Long =
    put(key, Cbor.write(state), ack)

  def latestValue[S](key: Array[Byte])(implicit s: Schema[S]): Option[(Long, Either[String, S])] =
    latest(key).map(r => (r.offset, Cbor.read[S](r.value)))
}

object Snapshots {
  /** the conventional topic: keyed, compacted */
  def apply(store: Store, name: String = "__snapshots", partitions: Int = 1): Snapshots =
    new Snapshots(store.topic(name, partitions, Policy(compact = true)))
}
