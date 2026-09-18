import okay.*
import okay.Optic.arrows.*

/**
 * COMPENSATION AS STRUCTURE (specs/static-workflow.md, stage 5 —
 * `static-workflow-undo`).
 *
 * `okay.persist.Saga` already compensates a LINEAR sequence of steps,
 * journaled intent-first, with forward and backward recovery. What a
 * term adds is that the sequence need not be linear and the journal
 * need not be a second one: the undos are found at the paths where
 * their steps ran, and what comes back is a `Wf.Proc` — a workflow,
 * which the landed engine runs, draws and resumes.
 *
 * The undos are FOUND and not CARRIED, which is the only reason this
 * is possible in an arrow at all: a stack of computations riding on
 * the edge, to be run later, is `ArrowApply`, which is a monad.
 */
class TestProcUndo extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  val A: Optic.Arrow[[X, Y] =>> Proc[Sig, X, Y]] & Optic.Choice[[X, Y] =>> Proc[Sig, X, Y]] =
    Proc.procArrow[Sig]

  given Wf.Runtime = Wf.Runtime.scripted(millis = 1_700_000_000_000L,
                                         id = "id-1", dice = 0.25)

  /** ask something, and say how to take it back */
  def step(q: String, undo: String): Wf.Proc[String, String, Unit, String] =
    Proc.undoable(Wf.Proc.ask[String, String, Unit](_ => q))(
      Wf.Proc.ask[String, String, (Unit, String)]((p: (Unit, String)) => s"$undo ${p._2}")
        >>> A.arr(_ => ()))

  /** the same, where the question is built from what is on the edge */
  def step2(q: String, undo: String): Wf.Proc[String, String, List[String], String] =
    Proc.undoable(Wf.Proc.ask[String, String, List[String]](got => s"$q ${got.length + 1}?"))(
      Wf.Proc.ask[String, String, (List[String], String)]((p: (List[String], String)) =>
        s"$undo ${p._2}") >>> A.arr(_ => ()))

  /** reserve, then charge — both compensable, in that order */
  val booking: Wf.Proc[String, String, Unit, (String, String)] =
    A.arr((_: Unit) => ((), ())) >>>
      A.first[Unit, String, Unit](step("reserve?", "release")) >>>
      A.second[Unit, String, String](step("charge?", "refund")) 

  def names(p: Wf.Proc[String, String, ?, ?]): Vector[String] = p.leaves.map(_.name)

  /** drive a compensation term and collect what it asked, in order */
  def questions(p: Wf.Proc[String, String, Unit, Unit]): List[String] =
    val asked = List.newBuilder[String]
    val start = !.run(Wf.resumable[String, String, Unit, P](Wf.Proc.program(p)(())))
    val _ = !.run(Wf.drive(start) { q => asked += q; okay.pure("ok") })
    asked.result()

  // ── the step runs; the inverse does not ──────────────────────────

  test("an Undo runs its STEP and nothing else — the inverse is metadata until it is asked for"):
    val start = !.run(Wf.resumable[String, String, (String, String), P](
      Wf.Proc.program(booking)(())))
    val (st, j) = !.run(Wf.drive(start)(_ => okay.pure("yes")))
    st match
      case Wf.Step.Done(v) => assertEquals(v, ("yes", "yes"))
      case other => fail(s"expected the drive to finish, it said $other")
    assertEquals(j.length, 2, "two steps, two questions — no compensation ran")

  // ── and what it would take to undo ───────────────────────────────

  test("NOTHING DONE, NOTHING TO UNDO: the compensation of an untouched run asks nobody"):
    val c = Wf.Proc.compensating(booking)((), Nil)
    assertEquals(questions(c), Nil)

  test("one step done: the compensation is that step's, fed what it produced"):
    val c = Wf.Proc.compensating(booking)((), List(Right("r-7")))
    assertEquals(questions(c), List("release r-7"))

  test("BOTH DONE: the last thing done is the first thing undone"):
    val c = Wf.Proc.compensating(booking)((), List(Right("r-7"), Right("c-9")))
    assertEquals(questions(c), List("refund c-9", "release r-7"))

  test("the compensation is a TERM: it names its leaves and draws itself"):
    val c = Wf.Proc.compensating(booking)((), List(Right("r-7"), Right("c-9")))
    assertEquals(names(c), Vector("ask", "ask"))
    assert(c.mermaid().contains("flowchart TD"))

  // ── the shape a linear saga cannot have ──────────────────────────

  test("A LOOP'S ROUNDS ARE EACH UNDONE, newest first — the shape a Vector of steps has not got"):
    // one room per night, where the number of nights is an ANSWER.
    // A saga that is a `Vector[Step]` cannot express this: it has to
    // know its steps before it starts. A term can, because `Iter` is
    // a node and the walk goes round it as many times as the journal
    // says it went.
    val body: Wf.Proc[String, String, List[String], Either[List[String], List[String]]] =
      Proc.alongside(step2("room", "cancel")) >>>
        A.arr: (p: (List[String], String)) =>
          val next = p._1 :+ p._2
          if next.length < 3 then Left(next) else Right(next)
    val nights: Wf.Proc[String, String, List[String], List[String]] = Proc.iter(body)
    // three rounds ran, so three rooms are cancelled, last one first
    val c = Wf.Proc.compensating(nights)(Nil, List(Right("a"), Right("b"), Right("c")))
    assertEquals(questions(c), List("cancel c", "cancel b", "cancel a"))
    // TWO rounds ran: the third was never booked and is not cancelled
    val half = Wf.Proc.compensating(nights)(Nil, List(Right("a"), Right("b")))
    assertEquals(questions(half), List("cancel b", "cancel a"))

  // ── the undo of a step that never ran must not run ───────────────

  test("A BRANCH NOT TAKEN LEAVES NOTHING TO UNDO"):
    val either: Wf.Proc[String, String, Either[String, Unit], Either[String, String]] =
      Proc.onRight(step("charge?", "refund"))
    // the Left side passes through: the step never ran
    assertEquals(questions(Wf.Proc.compensating(either)(Left("skipped"), Nil)), Nil)
    // the Right side did
    assertEquals(questions(Wf.Proc.compensating(either)(Right(()), List(Right("c-9")))),
      List("refund c-9"))

  test("a Par's UNANSWERED branch is not compensated — the speculative walk must not collect"):
    // `walk` looks INTO the right branch of a Par to report both
    // pending questions, over a journal that has not reached it. A
    // compensation collected there would undo a step nobody took.
    //
    // THE SHAPE THAT CATCHES IT is a compensable step that needs no
    // answer: an `Op` on an empty journal stops at `Asking` and
    // collects nothing whatever the buffer is, so the first version of
    // this test passed with the guard removed and proved nothing. A
    // PURE step completes on an empty journal, so the speculative walk
    // reaches its `Undo` — and the run has not.
    val slot: Wf.Proc[String, String, Unit, String] =
      Proc.undoable(A.arr((_: Unit) => "slot-1"))(
        Wf.Proc.ask[String, String, (Unit, String)](p => s"free ${p._2}") >>> A.arr(_ => ())) >>>
        A.arr((_: String) => ()) >>> Wf.Proc.ask[String, String, Unit](_ => "charge?")
    val pair: Wf.Proc[String, String, Unit, (String, String)] =
      Proc.par(step("reserve?", "release"), slot)
    // both branches outstanding: the right one has not run at all
    assertEquals(questions(Wf.Proc.compensating(pair)((), Nil)), Nil)
    // the left is answered; the right's pure step has NOW run
    assertEquals(questions(Wf.Proc.compensating(pair)((), List(Right("r-7")))),
      List("free slot-1", "release r-7"))

  // ── the term says what it can take back, before it runs ──────────

  test("the undos are IN the term: leaves names them beside the steps, with the side they are on"):
    val ls = booking.leaves
    assertEquals(ls.length, 4, "two steps and two compensations")
    // the path says which SIDE of the node a leaf is on: a
    // compensation lives under `undo`, its step under `do`
    assertEquals(ls.count(_.at.show.split("/").contains("undo")), 2, ls.map(_.at.show).toString)
    assertEquals(ls.count(_.at.show.split("/").contains("do")), 2, ls.map(_.at.show).toString)

  test("render shows a step and its inverse, and the picture puts the inverse off the path"):
    val r = step("charge?", "refund").render()
    assert(r.contains("undoable"), r)
    val m = step("charge?", "refund").mermaid()
    assert(m.contains("-.->"), s"a compensation is not on the forward path\n$m")
    assert(m.contains("""{"on failure"}"""), m)
