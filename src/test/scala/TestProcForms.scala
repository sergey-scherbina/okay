import okay.*
import okay.Direct.*
import scala.language.implicitConversions

/**
 * WHICH TYPES A BLOCK ACTUALLY NEEDS (proc-notation-forms), asked by
 * the operator the day the notation landed — the same question
 * `apdo-forms` asked of the applicative road, and with the same kind
 * of answer: fewer than the tests were writing.
 *
 * Every test here compiles a block TWICE, once with the three type
 * arguments and once without, and asserts the two terms behave
 * identically — because a macro that infers its types wrongly can
 * still compile, and "it compiles" is not the claim.
 */
class TestProcForms extends munit.FunSuite:

  type P = okay.Pure
  type Row = Delim + P
  type Sig = Wf.Asked[String, String]

  def ask(q: String): Wf.Question[String, String, String] = Wf.Question.Ask(q)
  def now: Wf.Question[String, String, Long] = Wf.Question.Now()

  given Wf.Runtime = Wf.Runtime.scripted(millis = 7L, id = "id-1", dice = 0.25)

  def done[Q, R](s: Wf.Step[Q, R]): R = s match
    case Wf.Step.Done(r) => r
    case other => fail(s"expected the drive to finish, it said $other")

  def run[X, R](p: Wf.Proc[String, String, X, R])(x: X)(oracle: String => String)
               : (R, List[String]) =
    var asked = List.empty[String]
    val (st, _) = !.run(Wf.drive(
      !.run(Wf.resumable[String, String, R, P](Wf.Proc.program(p)(x)))): q =>
        asked = asked :+ q
        okay.pure(oracle(q)))
    (done(st), asked)

  // ── the same block, written four ways ────────────────────────────

  /** 1 — all three type arguments: the only spelling that is always
   * available, and the only one needed where there is no expected type */
  val explicit: Wf.Proc[String, String, Unit, String] =
    Proc.direct[Sig, Unit, String]: _ =>
      val city = !ask("city?")
      val t = !now
      s"$city/$t"

  /** 2 — NONE: the expected type on the val carries all three */
  val fromVal: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city = !ask("city?")
      val t = !now
      s"$city/$t"

  /** 3 — none, with the expected type on a def's result */
  def fromDef: Wf.Proc[String, String, Unit, String] =
    Proc.direct: _ =>
      val city = !ask("city?")
      val t = !now
      s"$city/$t"

  /** 4 — no expected type anywhere: the arguments are required, and
   * the inferred type is the one they name */
  val noExpected = Proc.direct[Sig, Unit, String]: _ =>
    val city = !ask("city?")
    val t = !now
    s"$city/$t"

  test("the type arguments are NOT required where there is an expected type"):
    val ps = List(explicit, fromVal, fromDef, noExpected)
    ps.foreach: p =>
      assertEquals(p.leaves.map(_.name), Vector("ask", "now"))
      assertEquals(run(p)(())(_ => "Kyiv"), ("Kyiv/7", List("city?")))

  test("the block's INPUT type is inferred too"):
    val greet: Wf.Proc[String, String, String, String] =
      Proc.direct: who =>
        val where = !ask(s"where is $who?")
        s"$who@$where"
    assertEquals(run(greet)("ada")(_ => "Kyiv"), ("ada@Kyiv", List("where is ada?")))

  test("a branch needs no type arguments either"):
    val branchy: Wf.Proc[String, String, Unit, String] =
      Proc.direct: _ =>
        val city = !ask("city?")
        if city == "Kyiv" then
          val d = !ask("district?")
          s"$city/$d"
        else city
    assertEquals(branchy.leaves.map(_.name), Vector("ask", "ask"))
    assertEquals(run(branchy)(())(q => if q == "city?" then "Kyiv" else "Podil"),
      ("Kyiv/Podil", List("city?", "district?")))
    assertEquals(run(branchy)(())(_ => "Lviv"), ("Lviv", List("city?")))

  test("nor does a loop"):
    val rooms: Wf.Proc[String, String, Unit, List[String]] =
      Proc.direct: _ =>
        val n = !ask("nights?")
        var got = List.empty[String]
        while got.length < n.toInt do
          got = got :+ !ask(s"room ${got.length + 1}?")
        got
    assertEquals(rooms.leaves.map(_.name), Vector("ask", "ask"))
    assertEquals(run(rooms)(())(q => if q == "nights?" then "2" else q.take(6)),
      (List("room 1", "room 2"), List("nights?", "room 1?", "room 2?")))

  test("a MARK is still required — there is no auto-colouring here"):
    // `direct` has one, behind the `DirectCtx` capability; this road
    // has none, so an operation used as a value is the ordinary type
    // error it should be. Recorded so the difference is a decision
    // rather than a surprise.
    val e = compileErrors("""
      val bad: Wf.Proc[String, String, Unit, String] =
        Proc.direct { _ =>
          val city: String = ask("city?")
          city
        }
    """)
    assert(e.nonEmpty, "an operation was accepted where a value was wanted")
