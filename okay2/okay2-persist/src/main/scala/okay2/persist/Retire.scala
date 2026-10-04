package okay2.persist

import okay2.{!, +, At, Replayable, Row, Shift, Wf, pure}
import okay2.codec.Schema

/**
 * WHEN CAN THE OLD CODE GO (okay-persist's Retire.scala;
 * specs/durable-workflow.md stage 2): the questions an operator asks
 * before deleting a program version or the old half of a `patch` —
 * which programs still own records (`census`), where each run stands
 * (`states`), and which runs took which side of each patch (`patches`).
 */
object Retire {

  final case class Program(records: Int, ids: Set[String])

  final case class Census(programs: Map[String, Program], unreadable: List[(Long, String)]) {
    /** no record names this program any more */
    def gone(program: String): Boolean = !programs.contains(program)
  }

  sealed trait State
  object State {
    final case class Asking(question: String, where: Option[String]) extends State
    case object Finished extends State
    final case class Stopped(why: Dialogue.Stopped) extends State
  }

  /** which runs took a patch's new branch and which its old one */
  final case class Branch(taken: Set[String], skipped: Set[String]) {
    /** nobody is on the old side: the old half can be deleted */
    def oldHalfDead: Boolean = skipped.isEmpty
  }

  /** every program that owns records in this journal topic, with how
   * many and for which dialogues; a record that does not decode is named */
  def census[A](topic: Topic, version: Int = 1, upcasts: Map[Int, Typed.Upcast] = Map.empty)
               (implicit sa: Schema[A]): Census = {
    val typed = Typed[Dialogue.Entry[A]](topic, version, upcasts)
    val counts = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Set[String])]
    var bad = List.empty[(Long, String)]
    var p = 0
    while (p < topic.partitions) {
      var from = topic.begin(p)
      var going = true
      while (going) {
        typed.read(p, from, 256) match {
          case Typed.Read.TooEarly(b) => from = b
          case Typed.Read.Records(rs) =>
            if (rs.isEmpty) going = false
            else rs.foreach {
              case Typed.Decoded.Ok(off, _, k, e) =>
                from = off + 1
                val id = new String(k, "UTF-8")
                val (n, ids) = counts.getOrElse(e.program, (0, Set.empty[String]))
                counts(e.program) = (n + 1, ids + id)
              case Typed.Decoded.Bad(off, err) =>
                from = off + 1
                bad = bad :+ ((off, err))
            }
        }
      }
      p += 1
    }
    Census(counts.map { case (prog, (n, ids)) => prog -> Program(n, ids) }.toMap, bad)
  }

  /** where each of these runs stands, opened by `open` */
  def states[Q, A, R, F <: Row](ids: List[String])(open: String => Dialogue[Q, A, R, F]): Map[String, State] ! F = {
    // trampolined: each next run is deferred into the program's flatMap
    def go(left: List[String], acc: Map[String, State]): Map[String, State] ! F = left match {
      case Nil => pure[F, Map[String, State]](acc)
      case id :: rest => open(id).at.flatMap {
        case Left(why) => go(rest, acc + (id -> State.Stopped(why)))
        case Right(p) =>
          val st: State = p.asking match {
            case Some(q) => State.Asking(q.toString, p.where)
            case None => State.Finished
          }
          go(rest, acc + (id -> st))
      }
    }
    go(ids, Map.empty)
  }

  /** which runs took which side of each patch: replay each journal and
   * pair every `patch` question with the answer it was given */
  def patches[Q, A, R, F <: Row](journals: List[(String, Shift.Journal[Wf.Ans[A]])])
                                (body: Wf.Asks[Q, A, R, F] => R ! (Shift[Any] + F))
                                (implicit om: Shift.Machine[F], rp: Replayable[Shift[Any] + F], at: At): Map[String, Branch] ! F = {
    def go(left: List[(String, Shift.Journal[Wf.Ans[A]])], acc: Map[String, Branch]): Map[String, Branch] ! F = left match {
      case Nil => pure[F, Map[String, Branch]](acc)
      case (id, j) :: rest =>
        Wf.replaying[Q, A, R, F](body)(j).flatMap { case (_, seen) =>
          val next = seen.foldLeft(acc) {
            case (m, (Left(Wf.Sys.Patch(pid)), Left(Wf.SysA.Flag(on)))) =>
              val b = m.getOrElse(pid, Branch(Set.empty, Set.empty))
              m + (pid -> (if (on) b.copy(taken = b.taken + id) else b.copy(skipped = b.skipped + id)))
            case (m, _) => m
          }
          go(rest, next)
        }
    }
    go(journals, Map.empty)
  }
}
