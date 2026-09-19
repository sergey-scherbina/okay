package okay.persist

import okay.codec.Schema

/**
 * AN ADVISORY LEASE (workflow-lease, 2026-09-17), and the word
 * advisory is the whole design rather than a disclaimer on it.
 *
 * `expect` already makes two workers on one dialogue SAFE: both
 * append, the fold accepts one, and the worst case is a repeated
 * attempt — never a doubled answer. So a lease cannot make this
 * engine more correct. What it buys is that the wasted attempt
 * usually does not happen, which is worth having and is worth
 * nothing to rely on.
 *
 * ── TWO HOLES, AND THEY ARE DIFFERENT. Naming both, because the
 * tempting misuse is to treat this as a mutex.
 *
 * FIRST: acquisition is not atomic. `Topic.append` has no conditional
 * form, and giving it one would change every store, the wire protocol
 * and the Kafka interop for the sake of one consumer — the same trade
 * already recorded for `expect`, which is optimistic concurrency in
 * the PROJECTION instead. So `acquire` reads, decides and writes, and
 * two workers whose reads both land before either write will both
 * come away holding it. `TestLease` STATES this rather than staging
 * it: sequential calls cannot interleave (the second correctly
 * refuses), so showing it needs true concurrency, which would make
 * the suite flaky to prove something the design concedes here.
 *
 * SECOND, and this one IS tested, because an atomic acquire would not
 * fix it either: A LEASE DOES NOT FENCE. Expiry is decided by a
 * clock, and a clock cannot stop a thread — the holder whose lease
 * just expired may be inside a slow call and about to append, while
 * the next worker takes the lease entirely legitimately. Every lease
 * of this shape has that hole; closing it needs a fencing token
 * checked AT THE WRITE, which is exactly what `expect` already is.
 *
 * So the lease is the optimisation and `expect` is the guard, and the
 * tests say so in that order.
 *
 * Lose this whole topic and nothing is wrong: every worker believes
 * it is free to work, which is where the engine started.
 */
final class Leases(val snapshots: Snapshots):

  /** take it if nobody holds it or the holder's time has passed.
   * `true` means THIS reader believes it holds the lease — see the
   * class header for how much that is worth. */
  def acquire(id: String, owner: String, untilMillis: Long, nowMillis: Long): Boolean =
    held(id, nowMillis) match
      case Some(l) if l.owner != owner => false
      case _ =>
        val _ = snapshots.putValue(key(id), Leases.Held(owner, untilMillis))
        true

  /** still working: push the deadline out. Refuses if somebody else
   * has taken it in the meantime, which is how a worker learns it
   * lost the lease it thought it had. */
  def renew(id: String, owner: String, untilMillis: Long, nowMillis: Long): Boolean =
    held(id, nowMillis) match
      case Some(l) if l.owner != owner => false
      case _ =>
        val _ = snapshots.putValue(key(id), Leases.Held(owner, untilMillis))
        true

  /** done. Only the holder may, so a worker cannot free somebody
   * else's work by finishing its own. */
  def release(id: String, owner: String, nowMillis: Long): Boolean =
    held(id, nowMillis) match
      case Some(l) if l.owner != owner => false
      case _ =>
        val _ = snapshots.put(key(id), Array.emptyByteArray)
        true

  /** who holds it right now, if anybody: an expired lease is nobody's */
  def held(id: String, nowMillis: Long): Option[Leases.Held] =
    snapshots.latestValue[Leases.Held](key(id))
      .flatMap(_._2.toOption)
      .filter(_.until > nowMillis)

  /** every lease still standing. Scans what compaction left, the same
   * honest-for-thousands bound `Timers.armed` carries. */
  def standing(nowMillis: Long): Map[String, Leases.Held] =
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[Leases.Held]]
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
                else out(id) = okay.codec.Codecs.readCbor[Leases.Held](r.value).toOption
              from = rs.last.offset + 1
      p += 1
    out.collect { case (id, Some(l)) if l.until > nowMillis => id -> l }.toMap

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")

object Leases:

  /** who believes they hold it, and until when */
  final case class Held(owner: String, until: Long)
  given Schema[Held] = Schema.derived

  /** plain CBOR, no `Typed` envelope — a snapshot topic's convention */
  def over(store: Store, name: String = "__leases"): Leases =
    new Leases(Snapshots(store, name))
