package okay

import okay.core.Eff

/** specs/freer-min.md, stage 47: the interface over rows, the one monad its instance, `A ! R` its program */
class TestEffectsRows extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum State[S, +A]:
    case Get[S]() extends State[S, S]
    case Put[S](s: S) extends State[S, Unit]

  def reader[A](n: Int): Answering[Ask, A, A] = new Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number => n
  def writer[A]: Handler[Say, A, (List[String], A)] = new Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[T <: Row, X](op: Say[X], k: X => Eff[T, (List[String], A)]): Eff[T, (List[String], A)] = op match
      case Say.Line(s) => k(()).map((log, a) => (s :: log, a))
  /** a state in a cell of the handler: `get` and `put` answered where they are performed */
  def cell[S, A](s0: S): Answering[State % S, A, (S, A)] = new Answering[State % S, A, (S, A)]:
    private var s = s0
    def ret(a: A): (S, A) = (s, a)
    def value[X](op: State[S, X]): X = op match
      case State.Get() => s
      case State.Put(v) => s = v

  /** a program written once over the interface: its row declared, each operation by its path */
  def prog[M[_ <: Row, _]](using E: Effects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  test("the one monad is the instance found with no import; handlers apply in any order"):
    val E = Effects[Eff]
    assertEquals(E.run(E.handle(writer[Int])(E.handle(reader[Int](41))(prog[Eff]))), (List("41"), 42))
    assertEquals(E.run(E.handle(reader[(List[String], Int)](41))(E.handle(writer[Int])(prog[Eff]))), (List("41"), 42))

  test("the facade: A ! R is the one monad's program, an operation at the row expected, handle wherever the effect is"):
    val counting: Int ! (State % Int +: Say +: Pure) =
      for
        n <- effect[State % Int, Int](State.Get[Int]())   // an effect of two parameters names its effect
        _ <- effect[State % Int, Unit](State.Put(n + 1))
        _ <- effect(Say.Line(s"$n"))
        m <- effect[State % Int, Int](State.Get[Int]())
      yield m
    assertEquals(counting.handle(cell[Int, Int](41)).handle(writer[(Int, Int)]).value, (List("41"), (42, 42)))
    assertEquals(counting.handle(writer[Int]).handle(cell[Int, (List[String], Int)](41)).value, (42, (List("41"), 42)))

  test("Pure + A + B and A +: B +: Pure are one row; a tail call costs no frame"):
    def loop[M[_ <: Row, _]](n: Int, acc: Int)(using E: Effects[M]): M[Pure + Ask, Int] =
      if n == 0 then E.pure(acc)
      else E.perform[Ask, Pure + Ask, Int](Ask.Number).flatMap(x => E.tailcall(loop[M](n - 1, acc + x)))
    val E = Effects[Eff]
    assertEquals(E.run(E.handle(reader[Int](1))(loop[Eff](100000, 0))), 100000)
