package okay.persist

import okay.codec.Schema

/**
 * SOMEBODY HAS ASKED THIS RUN TO STOP (workflow-cancel, 2026-09-17).
 *
 * A compacted keyed topic, id to reason, exactly like `Timers`: a
 * cancellation REQUEST is operational data about a run, not part of
 * it. Lose this topic and no journal is wrong — the runs that would
 * have stopped simply carry on, which is the failure mode this whole
 * design prefers.
 *
 * ── WHY THE REQUEST IS NOT AN ANSWER IN THE JOURNAL. The journal
 * holds answers to questions the program ASKED. Nobody asked "may I
 * be cancelled?" at the moment an operator decided it; putting the
 * request there would be an answer to a question that does not exist
 * at that position, and that is the one thing a journal must never
 * hold. What DOES reach the journal is the program's own
 * `w.cancelled` check — a question, asked where the author put it,
 * answered once and replayed forever after.
 *
 * ── WHAT THAT BUYS, and it is the reason to prefer it: the decision
 * is REPLAYABLE. A run that checked at 10:00 and was told "no" is
 * told "no" by every replay of that position, even after the cancel
 * arrives at 10:01 — so a rebuild never takes a branch the original
 * run did not take. A run that has not yet reached its check sees the
 * request when it gets there.
 *
 * ── WHAT IT COSTS, stated rather than discovered. Cancellation here
 * is COOPERATIVE: it is seen at a check, and a program with no check
 * is not cancellable. A run asleep for a year learns it was cancelled
 * when it wakes; a run waiting on a signal that never comes never
 * learns at all. The alternative — delivering cancellation as a
 * thrown exception — was refuted, and not on taste: a `direct`
 * block's `try/catch` guards the BUILDING of a program, not its
 * running (pinned in TestDelimLimits), so a throw could not be caught
 * by the program being cancelled. An `if` can.
 *
 * COST of `requests`: one scan of what compaction left, the same
 * honest-for-thousands bound as `Timers.armed`.
 */
final class Cancels(val snapshots: Snapshots):

  /** ask a run to stop, and say why; the newest reason wins */
  def cancel(id: String, why: String): Unit =
    val _ = snapshots.putValue(key(id), Cancels.Asked(why))

  /** never mind — a tombstone, the compacted topic's way of saying a
   * key is gone. A run that has ALREADY seen the request keeps what
   * it saw: the journal, not this topic, is what it decided from. */
  def withdraw(id: String): Unit =
    val _ = snapshots.put(key(id), Array.emptyByteArray)

  /** has somebody asked this run to stop, and why */
  def requested(id: String): Option[String] = requests.get(id)

  /** every standing request, newest reason per id */
  def requests: Map[String, String] =
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[String]]
    val t = snapshots.topic
    var p = 0
    while p < t.partitions do
      var from = t.begin(p)
      var going = true
      while going do
        t.read(p, from, 512) match
          case Topic.Read.TooEarly(b) => from = b
          case Topic.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              rs.foreach: r =>
                val id = new String(r.key, "UTF-8")
                if r.value.isEmpty then out(id) = None
                else out(id) = Cancels.decode(r.value)
              from = rs.last.offset + 1
      p += 1
    out.collect { case (id, Some(why)) => id -> why }.toMap

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")

object Cancels:

  /** what a key's value is: the reason given */
  final case class Asked(why: String)

  given Schema[Asked] = Schema.derived

  /** plain CBOR, no `Typed` envelope — a snapshot topic's convention,
   * and the trap `Timers` fell into first */
  private def decode(bytes: Array[Byte]): Option[String] =
    okay.codec.Codecs.readCbor[Asked](bytes).toOption.map(_.why)

  /** a `Cancels` over a compacted topic of its own */
  def over(store: Store, name: String = "__cancels"): Cancels =
    new Cancels(Snapshots(store, name))
