package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * COOPERATIVE CANCELLATION (okay-persist's Cancels.scala): a compacted
 * keyed table of "please stop, because ...". The program reads it when
 * it ASKS (`Wf.cancelled`), and the answer is journalled like any other,
 * so a replay sees the same decision.
 */
final class Cancels(val snapshots: Snapshots) {

  def cancel(id: String, why: String): Unit = {
    val _ = snapshots.putValue(key(id), Cancels.Asked(why))
  }

  def withdraw(id: String): Unit = {
    val _ = snapshots.put(key(id), Array.emptyByteArray)
  }

  def requested(id: String): Option[String] = requests.get(id)

  def requests: Map[String, String] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Option[String]]
    Keyed.foreach(snapshots.topic) { r =>
      val id = new String(r.key, "UTF-8")
      out(id) = if (r.value.isEmpty) None else Cancels.decode(r.value)
    }
    out.collect { case (id, Some(why)) => id -> why }.toMap
  }

  private def key(id: String): Array[Byte] = id.getBytes("UTF-8")
}

object Cancels {
  final case class Asked(why: String)
  implicit lazy val askedSchema: Schema[Asked] = Schema.derived

  private def decode(bytes: Array[Byte]): Option[String] = Codecs.readCbor[Asked](bytes).toOption.map(_.why)

  def over(store: Store, name: String = "__cancels"): Cancels = new Cancels(Snapshots(store, name))
}
