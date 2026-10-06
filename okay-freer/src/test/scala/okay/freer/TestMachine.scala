package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md, the machine: one rule per node, no cast anywhere, measured on programs */
class TestMachine extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  type P = Pure

  /** a program of the empty row has only a value */
  def value[A](p: Freer[Pure, Unit, Unit, A]): A =
    val head: Freer[Pure, Unit, Unit, A] = Machine.run(p)
    head match
    case Return(a) => a
    case Inject(op) => op
    case Perform(op) => op
    case Bind(h, _) => (h: @unchecked) match
      case Inject(op) => op
      case Perform(op) => op
    case other => fail(s"not a value: $other")

  /** `Ask` answered with `in`, over the head forms the machine hands out */
  @tailrec final def runAsk[A](p: Freer[Diag[Ask], Unit, Unit, A], in: Int): A =
    val head: Freer[Diag[Ask], Unit, Unit, A] = Machine.run(p)
    head match
    case Return(a) => a
    case Inject(op) => op match
      case Ask.Number => in
    case Perform(op) => op match
      case Ask.Number => in
    case Bind(h, k) => (h: @unchecked) match
      case Inject(op) => op match
        case Ask.Number => runAsk(k(in), in)
    case other => fail(s"not handled: $other")

  @tailrec final def runState[S, R, A](p: Freer[PState, S, R, A], s: R): (S, A) =
    val head: Freer[PState, S, R, A] = Machine.run(p)
    head match
      case Return(a) => (s, a)
      case Inject(op) => runState(Bind(Inject(op), (x: A) => Return(x)), s)
      case Perform(op) => runState(Bind(Perform(op), (x: A) => Return(x)), s)
      case Bind(h, k) => (h: @unchecked) match
        case Inject(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)
        case Perform(op) => op match
          case PState.Get() => runState(k(s), s)
          case PState.Put(t) => runState(k(()), t)
      case other => fail(s"not handled: $other")

  val p = Prompt[Pure, Unit, Int]("p")
  val q = Prompt[Pure, Unit, Int]("q")

  test("shift0: the continuation, delimiter included, applied twice"):
    val prog = reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))
    assertEquals(value((prog)), 4)

  test("multi-shot: three resumptions, their values summed"):
    val prog = reset(p)(shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))
    assertEquals(value((prog)), 60)

  test("a delimiter of another prompt between the capture and its own is captured and put back"):
    val prog = reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))
    assertEquals(value((prog)), 64)

  test("dollar is derived and keeps λ$'s law: k carries ret with the delimiter, f(k) never meets ret"):
    // k(1) = ret(1 * 2) = 3, k(3) = ret(3 * 2) = 7; the answer is 7, not ret(…) once more
    assertEquals(value(dollar(p)((x: Int) => pure(x + 1))(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))), 7)

  test("a run over a run allocates nothing: a head form handed out comes back as itself"):
    val pa = Prompt[Diag[Ask], Unit, Int]("ask")
    val head = Machine.run(reset(pa)(inject(Ask.Number).map(_ + 1)))
    assert(Machine.run(head) eq head)

  test("a capture with no delimiter of its prompt: NoPrompt"):
    val e = intercept[NoPrompt](value((shift0[Int](p)(k => k(1)))))
    assertEquals(e.wanted, "p")

  test("answer-type modification realised: a Put in a shift0 body moves the state Int -> String"):
    // an index-moving delimiter and capture: the nodes themselves, their indexes named
    val p = Prompt[PState, String, Int]("state")
    val prog: Freer[PState, String, Int, Int] =
      Reset(p, Shift0[PState, String, String, Int, Int, Int](p, k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    assertEquals(runState((prog), 1), ("s", 6))

  test("an effect inside a delimiter is handed out, answered outside, and the run goes on"):
    val p = Prompt[Diag[Ask], Unit, Int]("ask")
    val prog = reset(p)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](p)(k => k(n).flatMap(k))
      yield x + 1)
    assertEquals(runAsk((prog), 10), 12)

  test("a body with an effect outside the prompt's row is refused at compile time"):
    val errors = compileErrors("""
      enum Other[+A]:
        case Op extends Other[Int]
      val p = Prompt[Pure, Unit, Int]("p")
      reset(p)(inject(Other.Op).map(_ + 1))""")
    note(errors)
    assert(errors.contains("Required"), errors)

  test("a program wider than its prompt's row: the delimiter's k stays at the prompt's row, the rest is wider"):
    val wide: Freer[Diag[Ask] + Pure, Unit, Unit, Int] =
      reset(p)(shift0[Int](p)(k => k(1)).map(_ + 1)).flatMap(x => inject(Ask.Number).map(_ + x))
    assertEquals(runAsk((wide), 10), 12)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    def loop(n: Int): Freer[P, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value((reset(p)(loop(100000)))), 100000)

  test("a continuation that escaped its delimiter is a program: run later, its delimiter comes with it"):
    var saved: Int => Freer[P, Unit, Unit, Int] = null
    val prog = reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1))
    assertEquals(value((prog)), 0)
    assertEquals(value((saved(41))), 42)

  test("the machine runs plain programs too: a million deferred calls, 100 000 left-nested maps"):
    def even(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Freer[Pure, Unit, Unit, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value((odd(1000001))), true)
    val chain = (1 to 100000).foldLeft(pure[Int, Unit](0): Freer[Pure, Unit, Unit, Int])((p, _) => p.map(_ + 1))
    assertEquals(value((chain)), 100000)
