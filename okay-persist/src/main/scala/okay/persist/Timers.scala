package okay.persist

import okay.codec.Schema

/**
 * DURABLE DEADLINES (workflow-timers, 2026-09-17): the half of a
 * sleeping workflow that lives outside its journal.
 *
 * When a drive returns `Waiting(Until(t))` the run is over — nothing
 * is held in memory, no thread is parked, and the journal says
 * exactly where the program stands. What is missing is somebody to
 * come back at `t`, and that is this: a compacted keyed topic whose
 * key is the dialogue's id and whose value is the instant it is
 * waiting for.
 *
 * WHY IT IS NOT IN THE JOURNAL, and this is the architecture's first
 * rule: a deadline is OPERATIONAL DATA ABOUT a run, not part of it.
 * The run's state is its answers and nothing else — which is what
 * lets a deadline be re-armed, moved or lost without changing what
 * the program will do when it wakes. Lose this whole topic and every
 * workflow is still correct; they simply sleep until somebody arms
 * them again.
 *
 * WHY IT DOES NOT APPEND THE ANSWER ITSELF. It could, and then it
 * would need every sleeping dialogue's `Schema`, program name and
 * body — which is the worker's business, not a clock's. `due(now)`
 * hands back ids; `workflow-worker` is what turns an id into an
 * appended `Elapsed`.
 *
 * COST, stated rather than discovered: `due` scans the topic, so it
 * is bounded by what compaction left, exactly as `Snapshots.latest`
 * is. That is honest for thousands of sleepers and wrong for
 * millions; the shape that fixes it is a time-bucketed key, and it is
 * a change to this file alone.
 */
final class Timers(val snapshots: Snapshots):

  /** wake this dialogue at this instant; the newest arm wins */
  def arm(id: String, atMillis: Long): Unit =
    val _ = snapshots.putValue(key(id), Timers.Due(atMillis))

  /** no longer waiting — a tombstone, the compacted topic's own way
   * of saying a key is gone */
  def disarm(id: String): Unit =
    val _ = snapshots.put(key(id), Array.emptyByteArray)

  /** every deadline still standing, newest arm per id */
  def armed: Map[String, Long] =
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[Long]]
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
                // an empty value is the tombstone `disarm` writes
                if r.value.isEmpty then out(id) = None
                else out(id) = Timers.decode(r.value)
              from = rs.last.offset + 1
      p += 1
    out.collect { case (id, Some(at)) => id -> at }.toMap

  /** the ids whose instant has passed */
  def due(nowMillis: Long): List[String] =
    armed.collect { case (id, at) if at <= nowMillis => id }.toList.sorted

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")

object Timers:

  /** what a key's value is: the instant to come back at */
  final case class Due(at: Long)

  given Schema[Due] = Schema.derived

  /** `Snapshots.putValue` writes PLAIN CBOR — no `Typed` envelope,
   * which is a snapshot topic's own convention and was worth checking
   * rather than assuming (the first cut tried to unwrap an envelope
   * that is not there, and every timer read as absent) */
  private def decode(bytes: Array[Byte]): Option[Long] =
    okay.codec.Codecs.readCbor[Due](bytes).toOption.map(_.at)

  /** a `Timers` over a compacted topic of its own, the convention the
   * rest of this module follows for operational state */
  def over(store: Store, name: String = "__timers"): Timers =
    new Timers(Snapshots(store, name))
