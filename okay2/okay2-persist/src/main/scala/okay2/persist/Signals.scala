package okay2.persist

import okay2.codec.Schema

/**
 * SIGNALS (okay-persist's Signals.scala; specs/durable-workflow.md
 * stage 4): a mailbox topic keyed by dialogue id, and a cursor per
 * (id, name) so a delivered signal is not delivered twice. `send` is the
 * outside world's door; the worker asks `next` when a run waits on a
 * name, and marks `delivered` once the payload is in the journal.
 */
final class Signals(val mailbox: Topic, val cursors: Snapshots,
                    version: Int = 1,
                    upcasts: Map[Int, Typed.Upcast] = Map.empty) {

  private val typed = Typed[Signals.Sent](mailbox, version, upcasts)

  def send(id: String, name: String, payload: String): Long =
    typed.append(key(id), Signals.Sent(name, payload), Ack.Durable)

  /** the first undelivered signal of this name for this dialogue */
  def next(id: String, name: String): Option[(Long, String)] = {
    val k = key(id)
    var from = cursor(id, name).map(_ + 1).getOrElse(mailbox.begin(partition(id)))
    var found: Option[(Long, String)] = None
    var going = true
    while (going) {
      typed.read(partition(id), from, 256) match {
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if (rs.isEmpty) going = false
          else
            for (d <- rs if going) d match {
              case Typed.Decoded.Ok(off, _, rk, s) =>
                if (rk.sameElements(k) && s.name == name) {
                  found = Some((off, s.payload))
                  going = false
                } else from = off + 1
              case Typed.Decoded.Bad(off, _) => from = off + 1
            }
      }
    }
    found
  }

  def delivered(id: String, name: String, offset: Long): Unit = {
    val _ = cursors.putValue(ckey(id, name), Signals.Cursor(offset))
  }

  def cursor(id: String, name: String): Option[Long] =
    cursors.latestValue[Signals.Cursor](ckey(id, name)).flatMap(_._2.toOption).map(_.at)

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")
  private def ckey(id: String, name: String): Array[Byte] = s"$id|$name".getBytes("UTF-8")
  private def partition(id: String): Int = Topic.route(key(id), mailbox.partitions)
}

object Signals {
  final case class Sent(name: String, payload: String)
  implicit lazy val sentSchema: Schema[Sent] = Schema.derived
  final case class Cursor(at: Long)
  implicit lazy val cursorSchema: Schema[Cursor] = Schema.derived

  def over(store: Store, name: String = "__signals", partitions: Int = 1): Signals =
    new Signals(store.topic(name, partitions), Snapshots(store, name + "__cursors"))
}
