package okay.persist

import okay.Delim

/**
 * KEEPING THE PROGRAM BETWEEN CALLS (dialogue-resume-cache,
 * 2026-09-17): the last cost the design had not paid down.
 *
 * A cold start replays — the fold IS the program, so where a run
 * stands is re-derived by running it over its answers. `Snapshots`
 * cuts the READING and `Wf.Next` bounds the HISTORY, but a process
 * that answers one dialogue ten times still replays it ten times,
 * once per call, because nothing held the program in between. A
 * paused program is a closure: it cannot be written down, but it can
 * be KEPT, and that is all this is.
 *
 * With it, n answers to one dialogue cost one replay and n steps
 * instead of n replays — the warm path `step` already provides,
 * extended across calls rather than within one drive.
 *
 * ── THE ONLY HARD PART IS KNOWING WHEN IT IS STALE, and the
 * requirement is sharp: finding out must not cost a fold, or the
 * cache has paid exactly what it exists to save. `Dialogue.undisturbed`
 * is that check — one offset read against the partition's end — and
 * it is deliberately CONSERVATIVE: another dialogue sharing the
 * partition makes it say "disturbed" when this one was not. A false
 * "disturbed" costs one replay, which is the behaviour without a
 * cache at all; a false "undisturbed" would be a program that has
 * missed an answer. Only one of those is affordable.
 *
 * ── IT HOLDS THE DIALOGUE TOO, not just the paused program, and that
 * is not an accident: a `Dialogue` instance carries `seen`, the
 * offset that makes the won-the-race check free on the warm path. A
 * fresh instance per call would re-fold on every append and give back
 * the O(n²) that `dialogue-snapshots` paid to remove.
 *
 * ── WHAT IT IS NOT. Not shared, not durable, not a source of truth:
 * it is a per-process optimisation over a journal that remains the
 * only state. Drop it, restart, run two of them — nothing changes but
 * how often a replay happens.
 */
final class Resume[Q, A, R, F[+_]](val max: Int = 256):

  // insertion-ordered, and re-inserted on every hit, so the head is
  // the least recently used. Cheaper than a real LRU structure and
  // exactly as correct for a cache that may forget anything at will.
  private val held =
    scala.collection.mutable.LinkedHashMap.empty[String, Resume.Held[Q, A, R, F]]

  /** the program for this id, if it is held AND the log has not moved
   * under it. A miss is never an error — it is a replay. */
  def get(id: String): Option[Resume.Held[Q, A, R, F]] =
    held.remove(id) match
      case None => None
      case Some(h) =>
        if !h.dialogue.undisturbed then None
        else
          val _ = held.put(id, h)      // most recently used
          Some(h)

  /** keep this one, evicting the least recently used if full */
  def put(id: String, dialogue: Dialogue[Q, A, R, F],
          paused: Delim.Dialogue[Q, A, R, F], at: Int): Unit =
    val _ = held.remove(id)
    val _ = held.put(id, Resume.Held(dialogue, paused, at))
    while held.size > max do
      held.headOption.foreach((k, _) => held.remove(k))

  /** forget one — what a worker does when a run finishes, so a
   * finished program is not kept alive by the cache */
  def drop(id: String): Unit =
    val _ = held.remove(id)

  /** forget everything */
  def clear(): Unit = held.clear()

  def size: Int = held.size

  /** the ids currently held, oldest first. For a test and for an
   * operator; nothing decides from it. */
  def ids: List[String] = held.keys.toList

object Resume:

  /** a program in hand, the dialogue that owns it, and the position
   * the next answer goes to */
  final case class Held[Q, A, R, F[+_]](dialogue: Dialogue[Q, A, R, F],
                                        paused: Delim.Dialogue[Q, A, R, F],
                                        at: Int)
