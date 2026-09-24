package okay.persist

import okay.{!, +, At, Delim, Replayable, Wf, pure}
import okay.codec.Schema

/**
 * WHAT IS STILL RUNNING IN THIS TOPIC, AND ON WHICH BRANCH
 * (workflow-retire, 2026-09-17): the evidence for deleting code.
 *
 * A long-lived workflow outlives deploys, so the question an operator
 * has to answer before removing `booking/1` or the `else` half of a
 * `patch` is not "is it old" but "is anything still there". Guessing
 * has one failure mode and it is the expensive one: a journal folds
 * onto a program that no longer exists, the fold STOPS, and the run
 * is stuck until somebody puts the code back.
 *
 * Three questions, and they cost different things — which is why they
 * are three calls and not one:
 *
 *   `census`  — which PROGRAM NAMES wrote into this topic, and for
 *               which dialogue ids. Reads the envelopes only: no
 *               body, no replay, exact.
 *   `states`  — which of those runs is still ASKING rather than
 *               finished. Costs one replay per run, because a journal
 *               does not record that a program ended — where it
 *               stands is re-derived, and that is the whole design.
 *   `patches` — which `patch` branches those runs are on. Costs a
 *               replay AND the body, and the reason is worth stating:
 *               a journal holds ANSWERS, and the id of a patch lives
 *               in the QUESTION. No amount of reading records can say
 *               which branch a `Flag(true)` belongs to; only running
 *               the program pairs them up again. `Wf.replaying` is
 *               that walk, reporting what it answered.
 *
 * COST: every call scans the topic's partitions, so this is an
 * OPERATOR'S tool — run it when deciding a deletion, not on a request
 * path. `states` and `patches` are additionally O(journal) per run.
 * Said plainly because the tempting misuse is a dashboard, and
 * `Statuses` is what a dashboard should read.
 */
object Retire:

  /** one program's footprint in a topic */
  final case class Program(records: Int, ids: Set[String])

  /** what the envelopes say, with the records that did not decode —
   * damage is data here too, and a census that hid it would be
   * evidence for a deletion it has not actually checked */
  final case class Census(programs: Map[String, Program],
                          unreadable: List[(Long, String)]):
    /** the one-line answer: is this program gone from this topic */
    def gone(program: String): Boolean = !programs.contains(program)

  /** where one run stands */
  enum State:
    case Asking(question: String, where: Option[String])
    case Finished
    case Stopped(why: Dialogue.Stopped)

  /** who is on a branch and who predates it */
  final case class Branch(taken: Set[String], skipped: Set[String]):
    /** the deletion test for the OLD half of a patch: nobody is still
     * answering `false`, so the `else` cannot be reached again */
    def oldHalfDead: Boolean = skipped.isEmpty

  /**
   * WHICH PROGRAMS WROTE HERE. Envelopes only — the cheap, exact
   * half, and usually the whole answer: a program with no records is
   * gone, and no replay can make it more gone than that.
   */
  def census[A](topic: Topic, version: Int = 1,
                upcasts: Map[Int, Typed.Upcast] = Map.empty)
               (using Schema[A]): Census =
    val typed = Typed[Dialogue.Entry[A]](topic, version, upcasts)
    val counts = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Set[String])]
    var bad = List.empty[(Long, String)]
    var p = 0
    while p < topic.partitions do
      var from = topic.begin(p)
      var going = true
      while going do
        typed.read(p, from, 256) match
          case Typed.Read.TooEarly(b) => from = b
          case Typed.Read.Records(rs) =>
            if rs.isEmpty then going = false
            else
              rs.foreach:
                case Typed.Decoded.Ok(off, _, k, e) =>
                  from = off + 1
                  val program = e match
                    case Dialogue.Entry.Answered(prog, _, _) => prog
                    case Dialogue.Entry.Continued(prog, _, _) => prog
                  val id = new String(k, "UTF-8")
                  val (n, ids) = counts.getOrElse(program, (0, Set.empty[String]))
                  counts(program) = (n + 1, ids + id)
                // a record this reader cannot decode is NOT evidence
                // of absence: it is named, and a deletion decided on a
                // census with unreadable records is a decision made
                // over a gap the operator can at least see
                case Typed.Decoded.Bad(off, err) =>
                  from = off + 1
                  bad = bad :+ (off, err)
      p += 1
    Census(counts.map((prog, v) => prog -> Program(v._1, v._2)).toMap, bad)

  /**
   * WHERE EACH RUN STANDS. One replay apiece, because a journal does
   * not record that a program finished — the fold IS the program, so
   * "finished" is something you find out by running it.
   */
  def states[Q, A, R, F[+_]](ids: List[String])
                            (open: String => Dialogue[Q, A, R, F])
                            : Map[String, State] ! F =
    def go(left: List[String], acc: Map[String, State]): Map[String, State] ! F =
      left match
        case Nil => pure(acc)
        case id :: rest => open(id).at.flatMap:
          case Left(why) => go(rest, acc + (id -> State.Stopped(why)))
          case Right(p) => go(rest, acc + (id -> (p.asking match
            case Some(q) => State.Asking(q.toString, p.where)
            case None => State.Finished)))
    go(ids, Map.empty)

  /**
   * WHICH BRANCHES ARE STILL LIVE. Needs the BODY as well as the
   * journals, and the header says why: a patch id is in the question,
   * never in the record. The journals come from `Dialogue.journal`;
   * passing them in rather than reaching into a `Dialogue` keeps this
   * a tool over data instead of a second way to build a dialogue.
   */
  def patches[Q, A, R, F[+_]](journals: List[(String, Delim.Journal[Wf.Ans[A]])])
                             (body: Wf.Asks[Q, A, R, F] ?=> R ! Delim + F)
                             (using Delim.OneMachine[F], Replayable[Delim + F], At)
                             : Map[String, Branch] ! F =
    def go(left: List[(String, Delim.Journal[Wf.Ans[A]])],
           acc: Map[String, Branch]): Map[String, Branch] ! F = left match
      case Nil => pure(acc)
      case (id, j) :: rest =>
        Wf.replaying[Q, A, R, F](body)(j).flatMap: (_, seen) =>
          val next = seen.foldLeft(acc):
            case (m, (Left(Wf.Sys.Patch(pid)), Left(Wf.SysA.Flag(on)))) =>
              val b = m.getOrElse(pid, Branch(Set.empty, Set.empty))
              m + (pid -> (if on then b.copy(taken = b.taken + id)
                           else b.copy(skipped = b.skipped + id)))
            case (m, _) => m
          go(rest, next)
    go(journals, Map.empty)
