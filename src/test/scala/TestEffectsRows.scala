package okay

import okay.cont.{Answering, Handler, Ctx, Cont, State, StateCell}

/** specs/freer-min.md, stage 47: the interface over rows, the machine its instance, `A ! R` its program */
class TestEffectsRows extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  def reader[A](n: Int): Answering[Ask, A, A] = new Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number => n
  def writer[A]: Handler[Say, A, (List[String], A)] = new Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Say[X], k: X => Cont[o.Here, o.Here, (List[String], A)]): Cont[o.Here, o.Here, (List[String], A)] =
      op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a))

  /** a program written once over the interface: its row declared, each operation by its path */
  def prog[M[_ <: Row, _]](using E: Effects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  test("the machine is the instance found with no import; handlers apply in any order"):
    val E = Effects[okay.cont.Free]
    assertEquals(E.run(E.handle(writer[Int])(E.handle(reader[Int](41))(prog[okay.cont.Free]))), (List("41"), 42))
    assertEquals(E.run(E.handle(reader[(List[String], Int)](41))(E.handle(writer[Int])(prog[okay.cont.Free]))), (List("41"), 42))

  test("the facade: A ! R is the machine's program, an operation at the row expected, handle wherever the effect is"):
    val counting: Int ! (State % Int +: Say +: Pure) =
      for
        n <- effect(State.Get[Int]())
        _ <- effect(State.Put(n + 1))
        _ <- effect(Say.Line(s"$n"))
        m <- effect(State.Get[Int]())
      yield m
    assertEquals(counting.handle(StateCell[Int, Int](41)).handle(writer[(Int, Int)]).value, (List("41"), (42, 42)))
    assertEquals(counting.handle(writer[Int]).handle(StateCell[Int, (List[String], Int)](41)).value, (42, (List("41"), 42)))

  test("Pure + A + B and A +: B +: Pure are one row; a tail call costs no frame"):
    def loop[M[_ <: Row, _]](n: Int, acc: Int)(using E: Effects[M]): M[Pure + Ask, Int] =
      if n == 0 then E.pure(acc)
      else E.perform[Ask, Pure + Ask, Int](Ask.Number).flatMap(x => E.tailcall(loop[M](n - 1, acc + x)))
    val E = Effects[okay.cont.Free]
    assertEquals(E.run(E.handle(reader[Int](1))(loop[okay.cont.Free](100000, 0))), 100000)
