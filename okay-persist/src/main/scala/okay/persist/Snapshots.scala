package okay.persist

import okay.codec.{Cbor, Schema}

/**
 * The thin put/latest convenience the ui lane asked for
 * (specs/persist.md, Results): a snapshot store IS a compacted
 * keyed topic — a fold that writes its state S under its key makes
 * the topic BE the snapshot, refold starts from the snapshot's
 * offset instead of zero, and `Topic.compact` reclaims the
 * superseded states. Snapshotting stays an optimization a consumer
 * opts into by writing a value (specs/llm-agentic.md, the settled
 * WAL+checkpoint decision).
 *
 * `latest` scans the key's partition — bounded by what compaction
 * left, which is the point of pairing the two.
 */
final class Snapshots(val topic: Topic):

  def put(key: Array[Byte], state: Array[Byte], ack: Ack = Ack.Durable): Long =
    topic.append(key, state, ack)

  /** the newest record under this key, with its offset — the refold
   * resumes from `offset + 1` */
  def latest(key: Array[Byte]): Option[Record] =
    val p = Topic.route(key, topic.partitions)
    var found: Option[Record] = None
    var from = topic.begin(p)
    var going = true
    while going do
      topic.read(p, from, 512) match
        case Topic.Read.TooEarly(b) => from = b
        case Topic.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            rs.reverseIterator.find(_.key.sameElements(key)).foreach(r => found = Some(r))
            from = rs.last.offset + 1
    found

  /** the Schema'd pair, for consumers whose state has one; decode
   * damage is data, never a throw */
  def putValue[S](key: Array[Byte], state: S, ack: Ack = Ack.Durable)
                 (using Schema[S]): Long =
    put(key, Cbor.write(state), ack)

  def latestValue[S](key: Array[Byte])(using Schema[S]): Option[(Long, Either[String, S])] =
    latest(key).map(r => (r.offset, Cbor.read[S](r.value)))

object Snapshots:
  /** the conventional topic: keyed, compacted */
  def apply(store: Store, name: String = "__snapshots", partitions: Int = 1): Snapshots =
    new Snapshots(store.topic(name, partitions, Policy(compact = true)))
