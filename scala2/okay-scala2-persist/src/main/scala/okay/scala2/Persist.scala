package okay.scala2

import okay.codec.Schema
import okay.given
import okay.persist.{Policy, Record, Store, Streams, Topic, Typed}

/**
 * okay-persist for Scala 2.13 (specs/scala2-facade.md, stage 15.2).
 *
 * Probed first, and most of okay-persist needs nothing: its engine API
 * is synchronous and plain, and a Scala 2 caller uses it directly —
 * `new MemoryStore`, `FileStore.open(path)`, `topic.append(key, value,
 * Ack.Durable)`, `topic.read(partition, from, max)`, `Offsets`,
 * `Snapshots`, `new Typed[A](topic, version, upcasts)`. Three things do
 * not carry over, and this object supplies them:
 *
 *   - DEFAULTS of a trait's abstract method are invisible from Scala 2:
 *     `store.topic("t")` asks for `partitions` and `policy` there.
 *     `Persist.topic` supplies them. (Defaults of a class's or an
 *     object's methods do carry over.)
 *   - an EXTENSION is invisible: `topic.of[A]` is `Persist.typed`.
 *   - the streaming reads answer a `Source` of chunks: `stream` and
 *     `tail` here answer a `Source[Record]`.
 */
object Persist {

  /** a topic with okay-persist's own defaults: one partition, the
   * default retention policy */
  def topic(store: Store, name: String, partitions: Int = 1): Topic =
    store.topic(name, partitions, Policy.default)

  /** the typed view: values of `A` as CBOR under a version envelope */
  def typed[A](topic: Topic, version: Int = 1)(using s: Schema[A]): Typed[A] =
    new Typed[A](topic, version, Map.empty)

  /** every record of a partition from `from` until the reader catches up;
   * history dropped by retention fails the stream */
  def stream(topic: Topic, partition: Int, from: Long, chunk: Int = 256): Source[Record] =
    Source.of(unchunk(Streams.stream(topic, partition, from, chunk)))

  /** the tailing read: it never ends, a caught-up reader waits
   * `pollMillis` and reads again, and the consumer stops pulling
   * (`take`, `takeWhile`) when it has enough */
  def tail(topic: Topic, partition: Int, from: Long, chunk: Int = 256, pollMillis: Long = 25): Source[Record] =
    Source.of(unchunk(Streams.tail(topic, partition, from, chunk, pollMillis)))

  private def unchunk(s: okay.Source[okay.Chunk[Record]]): okay.Source[Record] =
    okay.Writer.expand[okay.Chunk[Record], Record, Unit, okay.Async](s)(c => c)
}
