package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md, the machine: one rule per node, no cast, no prompt — the nearest delimiter is the head
 * of the stack in the index */
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

  val d = Delimiter[Pure, Unit, Int]()
  type P = Entry[Pure, Unit, Int]

  test("shift0: the continuation, delimiter included, applied twice"):
    assertEquals(value(reset(d)(shift0[Int](d)(k => k(1).flatMap(k)).map(_ * 2))), 4)

  test("multi-shot: three resumptions, their values summed"):
    assertEquals(value(reset(d)(shift0[Int](d)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))), 60)

  test("dollar is derived and keeps λ$'s law: k carries ret with the delimiter, f(k) never meets ret"):
    assertEquals(value(dollar(d)((x: Int) => pure(x + 1))(shift0[Int](d)(k => k(1).flatMap(k)).map(_ * 2))), 7)

  test("a capture ACROSS an inner delimiter is derived: two captures to the nearest, the inner put back by dollar"):
    val crossing = reset(d)(reset(d)(
      shift0[Int](d)(k1 => shift0[Int](d)(k2 =>
        val k: Int => Top[Pure, Int] = x => dollar(d)(k2)(k1(x))
        k(1).flatMap(k))).map(_ + 10)).map(_ * 2))
    assertEquals(value(crossing), 64)

  test("a capture with no delimiter in force is a COMPILE error: the stack is empty"):
    val errors = compileErrors("""
      val d = Delimiter[Pure, Unit, Int]()
      val bad: Freer[Pure, EmptyTuple, Unit, Unit, Int] = shift0[Int](d)(k => k(1))""")
    note(errors)
    assert(errors.contains("In["), errors)

  test("a program that claims a delimiter it did not install cannot be run"):
    val errors = compileErrors("""
      val claims: Freer[Pure, Entry[Pure, Unit, Int] *: EmptyTuple, Unit, Unit, Int] = pure(1)
      Machine.run(claims)""")
    note(errors)
    assert(errors.nonEmpty)

  test("answer-type modification across shift0: a Put in the body moves the state Int -> String"):
    // the index-moving forms are the nodes themselves, their indexes named
    val prog: Freer[PState, EmptyTuple, String, Int, Int] =
      Reset[PState, EmptyTuple, String, Int, Int](
        Shift0[PState, EmptyTuple, String, String, Int, Int, Int](k => perform(PState.Put[Int, String]("s")).flatMap(_ => k(5))).map(_ + 1))
    assertEquals(runState(prog, 1), ("s", 6))

  test("an effect inside a delimiter is handed out, answered outside, and the run goes on"):
    val da = Delimiter[Diag[Ask], Unit, Int]()
    val prog = reset(da)(
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](da)(k => k(n).flatMap(k))
      yield x + 1)
    assertEquals(runAsk(prog, 10), 12)

  test("a program wider than its delimiter's row: the k stays at the delimiter's row, the rest is wider"):
    val wide: Top[Diag[Ask] + Pure, Int] = reset(d)(shift0[Int](d)(k => k(1)).map(_ + 1)).flatMap(x => inject(Ask.Number).map(_ + x))
    assertEquals(runAsk(wide, 10), 12)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    def loop(n: Int)(using In[P *: EmptyTuple]): Freer[Pure, P *: EmptyTuple, Unit, Unit, Int] =
      if n == 0 then pure(0) else shift0[Int](d)(k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value(reset(d)(loop(100000))), 100000)

  test("a continuation that escaped its delimiter is a program of the outside: run later, its delimiter comes with it"):
    var saved: Int => Top[Pure, Int] = null
    assertEquals(value(reset(d)(shift0[Int](d)(k => { saved = k; pure(0) }).map(_ + 1))), 0)
    assertEquals(value(saved(41)), 42)

  test("a run over a run allocates nothing: a head form handed out comes back as itself"):
    val da = Delimiter[Diag[Ask], Unit, Int]()
    val head = Machine.run(reset(da)(inject(Ask.Number).map(_ + 1)))
    assert(Machine.run(head) eq head)

  test("the machine runs plain programs too: a million deferred calls, 100 000 left-nested maps"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(odd(1000001)), true)
    val chain = (1 to 100000).foldLeft(pure[Int, EmptyTuple, Unit](0): Top[Pure, Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)
