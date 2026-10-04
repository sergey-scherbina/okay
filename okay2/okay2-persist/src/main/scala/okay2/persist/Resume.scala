package okay2.persist

import okay2.{Row, Shift}

/**
 * THE WARM CACHE OF HELD PROGRAMS (okay-persist's Resume.scala;
 * dialogue-resume-cache): a bounded, most-recently-used map from
 * dialogue id to the program in hand and its position, so a process
 * answering one dialogue n times replays it once. `get` hands a holding
 * back only while its dialogue is `undisturbed` — nobody wrote since —
 * and drops it otherwise.
 */
final class Resume[Q, A, R, F <: Row](val max: Int = 256) {

  private val held = scala.collection.mutable.LinkedHashMap.empty[String, Resume.Held[Q, A, R, F]]

  def get(id: String): Option[Resume.Held[Q, A, R, F]] =
    held.remove(id) match {
      case None => None
      case Some(h) =>
        if (!h.dialogue.undisturbed) None
        else {
          val _ = held.put(id, h)      // most recently used
          Some(h)
        }
    }

  def put(id: String, dialogue: Dialogue[Q, A, R, F], paused: Shift.Dialogue[Q, A, R, F], at: Int): Unit = {
    val _ = held.remove(id)
    val _ = held.put(id, Resume.Held(dialogue, paused, at))
    while (held.size > max) held.headOption.foreach { case (k, _) => held.remove(k) }
  }

  def drop(id: String): Unit = {
    val _ = held.remove(id)
  }

  def clear(): Unit = held.clear()
  def size: Int = held.size
  def ids: List[String] = held.keys.toList
}

object Resume {
  final case class Held[Q, A, R, F <: Row](dialogue: Dialogue[Q, A, R, F], paused: Shift.Dialogue[Q, A, R, F], at: Int)
}
