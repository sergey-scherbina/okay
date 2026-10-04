package okay2.persist

import okay2.codec.{Codecs, Schema}

/**
 * WHERE EVERY RUN STANDS (okay-persist's Statuses.scala): a compacted
 * keyed table the worker writes after each advance — state, the
 * question it is asking and the line it is asking from — so an operator
 * reads one table instead of replaying every journal.
 */
final class Statuses(val snapshots: Snapshots) {

  def put(s: Statuses.Status): Unit = {
    val _ = snapshots.putValue(s.id.getBytes("UTF-8"), s)
  }

  def get(id: String): Option[Statuses.Status] =
    snapshots.latestValue[Statuses.Status](id.getBytes("UTF-8")).flatMap(_._2.toOption)

  def all: List[Statuses.Status] = {
    val out = scala.collection.mutable.LinkedHashMap.empty[String, Statuses.Status]
    Keyed.foreach(snapshots.topic) { r =>
      Codecs.readCbor[Statuses.Status](r.value).toOption.foreach(s => out(s.id) = s)
    }
    out.values.toList
  }

  /** the runs that have not moved since `sinceMillis` and are not done */
  def idleSince(sinceMillis: Long): List[Statuses.Status] =
    all.filter(s => !s.state.isInstanceOf[Statuses.State.Finished] && s.at <= sinceMillis)

  /** the runs a human has to look at */
  def needsAttention: List[Statuses.Status] =
    all.filter { s =>
      s.state match {
        case Statuses.State.Broken(_) | Statuses.State.Incompatible(_) => true
        case _ => false
      }
    }

  /** the runs waiting on a named signal */
  def waitingOn(name: String): List[Statuses.Status] =
    all.filter(_.state == Statuses.State.Waiting(s"signal:$name"))
}

object Statuses {

  sealed trait State
  object State {
    case object Running extends State
    final case class Sleeping(untilMillis: Long) extends State
    final case class Waiting(what: String) extends State
    final case class Finished(answer: String) extends State
    final case class Broken(why: String) extends State
    final case class Failed(why: String) extends State
    final case class Incompatible(why: String) extends State
    implicit lazy val schema: Schema[State] = Schema.derived
  }

  final case class Status(id: String, program: String, state: State,
                          asking: Option[String], where: Option[String], at: Long)
  implicit lazy val statusSchema: Schema[Status] = Schema.derived

  def over(store: Store, name: String = "__status"): Statuses = new Statuses(Snapshots(store, name))
}
