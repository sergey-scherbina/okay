package okay.persist

import okay.codec.Schema

/**
 * SIGNALS: WHAT THE OUTSIDE WORLD SENDS A RUNNING WORKFLOW
 * (workflow-signals, 2026-09-17).
 *
 * An answer replies to a question the program has ASKED. A signal
 * does not: it is sent when the sender has something to say, which
 * may be long before the run reaches the `awaitSignal` that wants it,
 * and there is nowhere in a journal to put an answer to a question
 * nobody asked — that is the one thing a journal must never hold.
 *
 * So a signal lands in a MAILBOX, and the worker moves it into the
 * journal at the moment the run actually waits for it. The mailbox is
 * an append-only topic keyed by dialogue id; a compacted topic of
 * CURSORS, keyed by id and name, remembers how far each name has been
 * delivered.
 *
 * WHY A CURSOR PER NAME rather than a count per dialogue: signals of
 * different names arrive interleaved, and a run waits for one name at
 * a time. A per-dialogue count would have to skip the names it is not
 * waiting for and could not then say what it had skipped. Per name,
 * "the first one after the cursor" is exactly the next one, and
 * ORDER WITHIN A NAME is kept — which is the only ordering anybody
 * can reasonably promise.
 *
 * DELIVERY IS AT MOST ONCE PER SIGNAL AND EXACTLY ONCE INTO THE
 * JOURNAL. The cursor moves only after the journal took the answer,
 * so a crash in between re-delivers the same signal — and the
 * journal's own `expect` rejects the second copy at a position that
 * is already filled. The failure mode is a repeated attempt, never a
 * doubled answer, which is the engine's third rule.
 *
 * Cost, stated: `next` scans the id's partition from the cursor, so
 * it is bounded by how much mail that dialogue has ever been sent.
 * Fine for the shape this is for; a dialogue that receives millions
 * of signals wants the mailbox compacted by delivery, which is a
 * change to this file alone.
 */
final class Signals(val mailbox: Topic, val cursors: Snapshots,
                    version: Int = 1,
                    upcasts: Map[Int, Typed.Upcast] = Map.empty):

  private val typed = Typed[Signals.Sent](mailbox, version, upcasts)

  /** send one, whether or not anybody is waiting for it yet */
  def send(id: String, name: String, payload: String): Long =
    typed.append(key(id), Signals.Sent(name, payload), Ack.Durable)

  /** the next undelivered signal of this name, with the offset that
   * has to be recorded once the journal has taken it */
  def next(id: String, name: String): Option[(Long, String)] =
    val from0 = cursor(id, name).map(_ + 1).getOrElse(mailbox.begin(partition(id)))
    val k = key(id)
    var from = from0
    var found: Option[(Long, String)] = None
    var going = true
    while going do
      typed.read(partition(id), from, 256) match
        case Typed.Read.TooEarly(b) => from = b
        case Typed.Read.Records(rs) =>
          if rs.isEmpty then going = false
          else
            for d <- rs if going do
              d match
                case Typed.Decoded.Ok(off, _, rk, s) =>
                  if rk.sameElements(k) && s.name == name then
                    found = Some((off, s.payload))
                    going = false
                  else from = off + 1
                case Typed.Decoded.Bad(off, _) =>
                  // damage is data here too: skip it and keep going,
                  // because one unreadable letter must not stop the
                  // post for every other name
                  from = off + 1
    found

  /** the journal has taken it: never before, so that a crash in
   * between re-delivers rather than loses */
  def delivered(id: String, name: String, offset: Long): Unit =
    val _ = cursors.putValue(ckey(id, name), Signals.Cursor(offset))

  /** how far this name has been delivered */
  def cursor(id: String, name: String): Option[Long] =
    cursors.latestValue[Signals.Cursor](ckey(id, name)).flatMap(_._2.toOption).map(_.at)

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")
  private def ckey(id: String, name: String): Array[Byte] =
    s"$id|$name".getBytes("UTF-8")
  private def partition(id: String): Int = Topic.route(key(id), mailbox.partitions)

object Signals:

  /** one letter */
  final case class Sent(name: String, payload: String)
  given Schema[Sent] = Schema.derived

  /** how far one name has been delivered for one dialogue */
  final case class Cursor(at: Long)
  given Schema[Cursor] = Schema.derived

  /** the conventional pair of topics */
  def over(store: Store, name: String = "__signals", partitions: Int = 1): Signals =
    new Signals(store.topic(name, partitions),
      Snapshots(store, name + "__cursors"))
