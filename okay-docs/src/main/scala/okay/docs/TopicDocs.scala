package okay.docs

import okay.{!, +, Async, Chunk, ChunkBuf, Produce, async, effect}
import okay.codec.Schema
import okay.persist.{Ack, Topic, Typed}
import scala.collection.mutable

/**
 * The OWN posture (specs/data.md): a compacted keyed topic ALREADY
 * IS a document store — this engine is that sentence made code. A
 * document's key is the record key, its VERSION is the record
 * OFFSET (monotone per partition by construction — the log hands
 * out CAS tokens for free), a delete is the tombstone (an empty
 * record value, the View convention), and the store's state is a
 * fold any node can rebuild from `begin`. Values travel through
 * the persist Typed envelope, so the document format evolves by
 * upcast like every journal-grade topic.
 *
 * Single-process semantics: every operation folds the log forward
 * first (read-your-writes), CAS is checked under the lock, and
 * `grants` answers Strong for everything — a single fold has no
 * weaker truth to offer.
 */
final class TopicDocs[A](topic: Topic,
                         indexes: Map[String, A => String] = Map.empty[String, A => String])
                        (using Schema[A]) extends Docs[A]:

  private val typed = Typed[A](topic, version = 1, upcasts = Map.empty)
  private val state = mutable.HashMap.empty[String, Docs.Versioned[A]]
  private val consumed = Array.tabulate(topic.partitions)(topic.begin)

  private def advance(): Unit =
    var p = 0
    while p < topic.partitions do
      var going = true
      while going do
        topic.read(p, consumed(p), 256) match
          case Topic.Read.TooEarly(b) => consumed(p) = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              for r <- rs do
                val id = new String(r.key, "UTF-8")
                if r.value.isEmpty then state.remove(id): Unit // the tombstone
                else typed.decode(r) match
                  case Typed.Decoded.Ok(off, _, _, a) =>
                    state.put(id, Docs.Versioned(off, a)): Unit
                  case Typed.Decoded.Bad(_, _) => ()             // damage is data; skip
                consumed(p) = r.offset + 1
      p += 1

  private def keyOf(id: String): Array[Byte] = id.getBytes("UTF-8")

  def get(id: String): Option[Docs.Versioned[A]] ! Async =
    async(synchronized { advance(); state.get(id).map(v => v.copy()) })

  def put(id: String, a: A, cond: Cond): PutResult ! Async = async {
    synchronized {
      advance()
      val current = state.get(id)
      val allowed = cond match
        case Cond.Always => true
        case Cond.IfAbsent => current.isEmpty
        case Cond.IfVersion(v) => current.exists(_.version == v)
      if !allowed then PutResult.Stale(current.map(_.version))
      else
        val off = typed.append(keyOf(id), a, Ack.Durable)
        advance()
        PutResult.Applied(off)
    }
  }

  def delete(id: String, cond: Cond): PutResult ! Async = async {
    synchronized {
      advance()
      val current = state.get(id)
      val allowed = cond match
        case Cond.Always => true
        case Cond.IfAbsent => current.isEmpty
        case Cond.IfVersion(v) => current.exists(_.version == v)
      if !allowed then PutResult.Stale(current.map(_.version))
      else
        val off = topic.append(keyOf(id), Array.empty, Ack.Durable)
        advance()
        PutResult.Applied(off)
    }
  }

  def query(field: String, equals: String, max: Int)
  : Chunk[(String, A)] ! (Produce + Async) =
    type F = Produce + Async
    val f = indexes.getOrElse(field,
      throw IllegalArgumentException(
        s"field $field is not a declared index — a scan wearing a query's hat " +
          s"is refused (declared: ${indexes.keys.mkString(", ")})"))
    effect[F, Chunk[(String, A)]](Async.Run { () =>
      synchronized {
        advance()
        ChunkBuf.of(state.iterator
          .collect { case (id, v) if f(v.value) == equals => (id, v.value) }
          .take(max).toVector.sortBy(_._1))
      }
    }).flatMap(c => effect[F, Chunk[(String, A)]](c))

  /** a single fold has no weaker truth to offer */
  def grants(requested: Consistency): Consistency = Consistency.Strong
