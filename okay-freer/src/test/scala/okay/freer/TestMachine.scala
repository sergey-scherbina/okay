package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md, the machine: five rules over `Control`, measured on programs */
class TestMachine extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  type P = Row[Pure]

  /** a program of the empty row has only a value */
  def value[A](p: Freer[Pure, Unit, Unit, A]): A = p.resume match
    case Return(a) => a
    case Inject(op) => op
    case Perform(op) => op
    case Bind(h, _) => (h: @unchecked) match
      case Inject(op) => op
      case Perform(op) => op

  /** `Ask` answered with `in`, over the head forms the machine hands out */
  @tailrec final def runAsk[A](p: Freer[Diag[Ask], Unit, Unit, A], in: Int): A = p.resume match
    case Return(a) => a
    case Inject(op) => op match
      case Ask.Number => in
    case Perform(op) => op match
      case Ask.Number => in
    case Bind(h, k) => (h: @unchecked) match
      case Inject(op) => op match
        case Ask.Number => runAsk(k(in), in)

  @tailrec final def runState[S, R, A](p: Freer[PState, S, R, A], s: R): (S, A) =
    p.resume match
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

  val p = Prompt[Pure, Unit, Int]("p")
  val q = Prompt[Pure, Unit, Int]("q")

  test("shift0: the continuation, delimiter included, applied twice"):
    val prog = reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))
    assertEquals(value(Machine.run(prog)), 4)

  test("multi-shot: three resumptions, their values summed"):
    val prog = reset(p)(shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))
    assertEquals(value(Machine.run(prog)), 60)

  test("a delimiter of another prompt between the capture and its own is captured and put back"):
    val prog = reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))
    assertEquals(value(Machine.run(prog)), 64)

  test("a capture with no delimiter of its prompt: NoPrompt"):
    val e = intercept[NoPrompt](value(Machine.run(shift0[Int](p)(k => k(1)))))
    assertEquals(e.wanted, "p")

  test("answer-type modification realised: a Put in a shift0 body moves the state Int -> String"):
    // an index-moving delimiter and capture: the general forms, their indexes named
    val p = Prompt[PState, String, Int]("state")
    val prog: Freer[Row[PState], String, Int, Int] =
      perform(Control.Reset[PState, String, String, Int, Int, Int](p, y => pure(y),
        perform(Control.Shift0[PState, String, String, Int, Int, Int](p,
          k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5)))).map(_ + 1)))
    assertEquals(runState(Machine.run(prog), 1), ("s", 6))

  test("an effect inside a delimiter is handed out, answered outside, and the run goes on"):
    val p = Prompt[Diag[Ask], Unit, Int]("ask")
    val prog = reset(p)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](p)(k => k(n).flatMap(k))
      yield x + 1)
    assertEquals(runAsk(Machine.run(prog), 10), 12)

  test("a body with an effect outside the prompt's row is refused at compile time"):
    val errors = compileErrors("""
      enum Other[+A]:
        case Op extends Other[Int]
      val p = Prompt[Pure, Unit, Int]("p")
      reset(p)(inject(Other.Op).map(_ + 1))""")
    note(errors)
    assert(errors.contains("Cannot prove"), errors)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    def loop(n: Int): Freer[P, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value(Machine.run(reset(p)(loop(100000)))), 100000)

  test("a continuation that escaped its delimiter runs the piece alone, with or without the machine"):
    var saved: Int => Freer[P, Unit, Unit, Int] = null
    val prog = reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1))
    assertEquals(value(Machine.run(prog)), 0)
    assertEquals(value(Machine.run(saved(41))), 42)
    // forced by the tree's own `resume`, with no machine around: the piece runs alone
    saved(41).resume match
      case Return(a) => assertEquals(a, 42)
      case other => fail(s"not a value: $other")
