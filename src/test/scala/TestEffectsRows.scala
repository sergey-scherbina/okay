package okay

import okay.core.Eff
import okay.Effects.machine.{effect, handle}

/** specs/freer-min.md, stage 47: the interface over rows, the one monad its instance, `A ! R` its program */
class TestEffectsRows extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum State[S, +A]:
    case Get[S]() extends State[S, S]
    case Put[S](s: S) extends State[S, Unit]

  def reader[A](n: Int): okay.core.Answering[Ask, A, A] = new okay.core.Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number => n
  def writer[A]: okay.core.Handler[Say, A, (List[String], A)] = new okay.core.Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[T <: Row, X](op: Say[X], k: X => Eff[T, (List[String], A)]): Eff[T, (List[String], A)] = op match
      case Say.Line(s) => k(()).map((log, a) => (s :: log, a))
  /** a state in a cell of the handler: `get` and `put` answered where they are performed */
  def cell[S, A](s0: S): okay.core.Answering[State % S, A, (S, A)] = new okay.core.Answering[State % S, A, (S, A)]:
    private var s = s0
    def ret(a: A): (S, A) = (s, a)
    def value[X](op: State[S, X]): X = op match
      case State.Get() => s
      case State.Put(v) => s = v

  /** a program written once over the interface: its row declared, each operation by its path */
  def prog[F[_ <: Row, _]](using E: Effects[F]): F[Ask +: Say +: okay.core.Pure, Int] =
    E.perform[Ask, Ask +: Say +: okay.core.Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: okay.core.Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  test("the one monad is the instance found with no import; handlers apply in any order"):
    val E = Effects[Eff]
    assertEquals(E.run(E.handleAt(writer[Int])(E.handleAt(reader[Int](41))(prog[Eff]))), (List("41"), 42))
    assertEquals(E.run(E.handleAt(reader[(List[String], Int)](41))(E.handleAt(writer[Int])(prog[Eff]))), (List("41"), 42))

  test("the facade: A ! R is the one monad's program, an operation at the row expected, handle wherever the effect is"):
    // a program needing its effects, in any order: fitted where it is handled
    val counting: Int !! State % Int + Say =
      for
        n <- effect[State % Int, Int](State.Get[Int]())   // an effect of two parameters names its effect
        _ <- effect[State % Int, Unit](State.Put(n + 1))
        _ <- effect(Say.Line(s"$n"))
        m <- effect[State % Int, Int](State.Get[Int]())
      yield m
    assertEquals(Effects.machine.run(handle(writer[(Int, Int)])(handle(cell[Int, Int](41))(counting))), (List("41"), (42, 42)))
    assertEquals(Effects.machine.run(handle(cell[Int, (List[String], Int)](41))(handle(writer[Int])(counting))), (42, (List("41"), 42)))

  test("a row named, A +: okay.core.Pure; a tail call costs no frame"):
    def loop[F[_ <: Row, _]](n: Int, acc: Int)(using E: Effects[F]): F[Ask +: okay.core.Pure, Int] =
      if n == 0 then E.pure(acc)
      else E.perform[Ask, Ask +: okay.core.Pure, Int](Ask.Number).flatMap(x => E.tailcall(loop[F](n - 1, acc + x)))
    val E = Effects[Eff]
    assertEquals(E.run(E.handleAt(reader[Int](1))(loop[Eff](100000, 0))), 100000)

  /** delimited control in the interface, beside pure, perform, flatMap: `reset`, `shift` — written once over it */
  def twice[F[_ <: Row, _]](using E: Effects[F]): F[Ask +: okay.core.Pure, Int] =
    type T = Ask +: okay.core.Pure
    E.reset[Int, T](E.shift[Int, T, Int](k => k(1).flatMap(a => k(a * 10).map(b => a + b)))
      .flatMap(x => E.perform[Ask, okay.core.Shifts[Int, T] +: T, Int](Ask.Number).map(n => x + n)))

  test("reset and shift in the interface: k twice, the reader outside each time — the kernel's own delimiter"):
    val E = Effects[Eff]
    assertEquals(E.run(E.handleAt(reader[Int](41))(twice[Eff])), 42 + 461)
