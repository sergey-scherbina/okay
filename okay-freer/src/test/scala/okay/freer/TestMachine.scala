package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md, the machine: one rule per node, no cast, the delimiter stack in the index */
class TestMachine extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum PState[S, R, +A]:
    case Get[S]() extends PState[S, S, S]
    case Put[S, T](t: T) extends PState[T, S, Unit]

  type Top[G[_, _, +_], A] = Freer[G, EmptyTuple, Unit, Unit, A]

  def value[S, A](p: Freer[Pure, EmptyTuple, S, S, A]): A =
    val head: Freer[Pure, EmptyTuple, S, S, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  @tailrec final def runAsk[A](p: Top[Diag[Ask], A], in: Int): A =
    val head: Top[Diag[Ask], A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Inject(op), k) => op match
        case Ask.Number => runAsk(k(in), in)
      case other => fail(s"not handled: $other")

  @tailrec final def runState[S, R, A](p: Freer[PState, EmptyTuple, S, R, A], s: R): (S, A) =
    val head: Freer[PState, EmptyTuple, S, R, A] = Machine.run(p)
    head match
      case Return(a) => (s, a)
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
  type P = At["p", Freer[Pure, EmptyTuple, Unit, Unit, Int]]

  test("shift0: the continuation, delimiter included, applied twice"):
    assertEquals(value(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))), 4)

  test("multi-shot: three resumptions, their values summed"):
    assertEquals(value(reset(p)(shift0[Int](p)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))), 60)

  test("a delimiter of another prompt between the capture and its own is captured and put back"):
    assertEquals(value(reset(p)(reset(q)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))), 64)

  test("the same prompt twice: the innermost delimiter is the one (Head before Tail)"):
    assertEquals(value(reset(p)(reset(p)(shift0[Int](p)(k => k(1).flatMap(k)).map(_ + 10)).map(_ * 2))), 42)

  test("dollar is derived and keeps λ$'s law: k carries ret with the delimiter, f(k) never meets ret"):
    assertEquals(value(dollar(p)((x: Int) => pure(x + 1))(shift0[Int](p)(k => k(1).flatMap(k)).map(_ * 2))), 7)

  test("a capture with no delimiter of its prompt is a COMPILE error: no witness"):
    val errors = compileErrors("""
      val p = Prompt[Pure, Unit, Int]("p")
      val bad: Freer[Pure, EmptyTuple, Unit, Unit, Int] = shift0[Int](p)(k => k(1))""")
    note(errors)
    assert(errors.contains("Has["), errors)

  test("a program that claims a delimiter it did not install cannot be run"):
    val errors = compileErrors("""
      val claims: Freer[Pure, At["p", Freer[Pure, EmptyTuple, Unit, Unit, Int]] *: EmptyTuple, Unit, Unit, Int] = pure(1)
      Machine.run(claims)""")
    note(errors)
    assert(errors.nonEmpty)

  test("answer-type modification with static prompts: a Put in a shift0 body moves the state Int -> String"):
    val ps = Prompt[PState, String, Int]("state")
    type E = At["state", Freer[PState, EmptyTuple, String, String, Int]]
    // the index-moving forms are the nodes themselves, their indexes named, the witness by hand
    val prog: Freer[PState, EmptyTuple, String, Int, Int] =
      Reset[PState, EmptyTuple, String, Int, Int, "state"](ps,
        Shift0[PState, E *: EmptyTuple, EmptyTuple, String, String, Int, Int, Int, "state"](ps, Has.Head(),
          k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    assertEquals(runState(prog, 1), ("s", 6))

  test("an effect inside a delimiter is handed out, answered outside, and the run goes on"):
    val pa = Prompt[Diag[Ask], Unit, Int]("ask")
    val prog = reset(pa)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](pa)(k => k(n).flatMap(k))
      yield x + 1)
    assertEquals(runAsk(prog, 10), 12)

  test("a body with an effect outside the prompt's row is refused at compile time"):
    val errors = compileErrors("""
      enum Other[+A]:
        case Op extends Other[Int]
      val p = Prompt[Pure, Unit, Int]("p")
      reset(p)(inject(Other.Op).map(_ + 1))""")
    note(errors)
    assert(errors.nonEmpty, errors)

  test("a program wider than its prompt's row: the delimiter's k stays at the prompt's row, the rest is wider"):
    val wide: Top[Diag[Ask] + Pure, Int] = reset(p)(shift0[Int](p)(k => k(1)).map(_ + 1)).flatMap(x => inject(Ask.Number).map(_ + x))
    assertEquals(runAsk(wide, 10), 12)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    // a program written OUTSIDE its reset declares the stack it is for
    def loop(n: Int)(using In[P *: EmptyTuple]): Freer[Pure, P *: EmptyTuple, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](p)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value(reset(p)(loop(100000))), 100000)

  test("a continuation that escaped its delimiter is a program of the outside: run later, its delimiter comes with it"):
    var saved: Int => Top[Pure, Int] = null
    assertEquals(value(reset(p)(shift0[Int](p)(k => { saved = k; pure(0) }).map(_ + 1))), 0)
    assertEquals(value(saved(41)), 42)

  test("a run over a run allocates nothing: a head form handed out comes back as itself"):
    val pa = Prompt[Diag[Ask], Unit, Int]("ask")
    val head = Machine.run(reset(pa)(inject(Ask.Number).map(_ + 1)))
    assert(Machine.run(head) eq head)

  test("the machine runs plain programs too: a million deferred calls, 100 000 left-nested maps"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(odd(1000001)), true)
    val chain = (1 to 100000).foldLeft(pure[Int, EmptyTuple, Unit](0): Top[Pure, Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)
