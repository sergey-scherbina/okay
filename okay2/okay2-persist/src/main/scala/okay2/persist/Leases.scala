package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * WHO IS DRIVING THIS DIALOGUE (okay-persist's Leases.scala): a lease
 * per dialogue id with an owner and a deadline, so two workers do not
 * advance one run at once. Liveness only: the journal's `expect` already
 * makes a lost race harmless, and a lease makes it rare.
 */
final class Leases(val snapshots: Snapshots) {

  def acquire(id: String, owner: String, untilMillis: Long, nowMillis: Long): Boolean = put(id, owner, untilMillis, nowMillis)

  def renew(id: String, owner: String, untilMillis: Long, nowMillis: Long): Boolean = put(id, owner, untilMillis, nowMillis)

  private def put(id: String, owner: String, untilMillis: Long, nowMillis: Long): Boolean =
    held(id, nowMillis) match {
      case Some(l) if l.owner != owner => false
      case _ =>
        val _ = snapshots.putValue(key(id), Leases.Held(owner, untilMillis))
        true
    }

  def release(id: String, owner: String, nowMillis: Long): Boolean =
    held(id, nowMillis) match {
      case Some(l) if l.owner != owner => false
      case _ =>
        val _ = snapshots.put(key(id), Array.emptyByteArray)
        true
    }

  /** the live lease on this dialogue, if any */
  def held(id: String, nowMillis: Long): Option[Leases.Held] =
    snapshots.latestValue[Leases.Held](key(id)).flatMap(_._2.toOption).filter(_.until > nowMillis)

  /** every live lease */
  def standing(nowMillis: Long): Map[String, Leases.Held] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[Leases.Held]]
    Keyed.foreach(snapshots.topic) { r =>
      val id = new String(r.key, "UTF-8")
      out(id) = if (r.value.isEmpty) None else Codecs.readCbor[Leases.Held](r.value).toOption
    }
    out.collect { case (id, Some(l)) if l.until > nowMillis => id -> l }.toMap
  }

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")
}

object Leases {
  final case class Held(owner: String, until: Long)
  implicit lazy val heldSchema: Schema[Held] = Schema.derived

  def over(store: Store, name: String = "__leases"): Leases = new Leases(Snapshots(store, name))
}
