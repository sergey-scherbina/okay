package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * DURABLE TIMERS (okay-persist's Timers.scala; specs/durable-workflow.md
 * stage 4): which dialogue sleeps until when, as a compacted keyed
 * topic — the latest record per dialogue id is its timer, an empty value
 * is "disarmed". A process that restarts reads the table and knows whom
 * to wake; the deadline itself lives in the dialogue's journal, so this
 * is an index, never the truth.
 */
final class Timers(val snapshots: Snapshots) {

  def arm(id: String, atMillis: Long): Unit = {
    val _ = snapshots.putValue(key(id), Timers.Due(atMillis))
  }

  def disarm(id: String): Unit = {
    val _ = snapshots.put(key(id), Array.emptyByteArray)
  }

  /** every armed timer, by dialogue id */
  def armed: Map[String, Long] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[Long]]
    Keyed.foreach(snapshots.topic) { r =>
      val id = new String(r.key, "UTF-8")
      out(id) = if (r.value.isEmpty) None else Timers.decode(r.value)
    }
    out.collect { case (id, Some(at)) => id -> at }.toMap
  }

  /** the ids whose deadline has passed, in a stable order */
  def due(nowMillis: Long): List[String] =
    armed.collect { case (id, at) if at <= nowMillis => id }.toList.sorted

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")
}

object Timers {
  final case class Due(at: Long)
  implicit lazy val dueSchema: Schema[Due] = Schema.derived

  private def decode(bytes: Array[Byte]): Option[Long] = Codecs.readCbor[Due](bytes).toOption.map(_.at)

  def over(store: Store, name: String = "__timers"): Timers = new Timers(Snapshots(store, name))
}
