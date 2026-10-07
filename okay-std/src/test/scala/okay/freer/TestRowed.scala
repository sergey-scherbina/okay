package okay.freer


import okay.{Effects, Row, Pure, +:, %}
import okay.cont.{Answering, Handler, Ctx, Cont, StateCell, State}

/** specs/freer-min.md, stage 47: the classic tree under the facade — the same program as the core's TestEffectsRows,
 * run in the tree, its handlers the machine's, run on the machine and reified back */
class TestRowed extends munit.FunSuite:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]
  enum Choose[+A]:
    case Flip extends Choose[Boolean]

  def reader[A](n: Int): Answering[Ask, A, A] = new Answering[Ask, A, A]:
    def ret(a: A): A = a
    def value[X](op: Ask[X]): X = op match
      case Ask.Number => n
  def writer[A]: Handler[Say, A, (List[String], A)] = new Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Say[X], k: X => Cont[o.Here, o.Here, (List[String], A)]): Cont[o.Here, o.Here, (List[String], A)] =
      op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a))
  def every[A]: Handler[Choose, A, List[A]] = new Handler[Choose, A, List[A]]:
    def ret(a: A): List[A] = List(a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Choose[X], k: X => Cont[o.Here, o.Here, List[A]]): Cont[o.Here, o.Here, List[A]] = op match
      case Choose.Flip => k(true).flatMap(xs => k(false).map(ys => xs ++ ys))

  def prog[M[_ <: Row, _]](using E: Effects[M]): M[Ask +: Say +: Pure, Int] =
    E.perform[Ask, Ask +: Say +: Pure, Int](Ask.Number).flatMap(n =>
      E.perform[Say, Ask +: Say +: Pure, Unit](Say.Line(n.toString)).map(_ => n + 1))

  test("the tree is an instance of the interface over rows: handlers in either order, the same answers"):
    val E = Effects[Rowed]
    assertEquals(E.run(E.handle(writer[Int])(E.handle(reader[Int](41))(prog[Rowed]))), (List("41"), 42))
    assertEquals(E.run(E.handle(reader[(List[String], Int)](41))(E.handle(writer[Int])(prog[Rowed]))), (List("41"), 42))

  test("a multi-shot handler through the tree: the reified rest is resumed once per world, in order"):
    val E = Effects[Rowed]
    type R = Choose +: Ask +: Say +: Pure
    val p: Rowed[R, Int] =
      E.perform[Choose, R, Boolean](Choose.Flip).flatMap(a =>
        E.perform[Ask, R, Int](Ask.Number).flatMap(n =>
          E.perform[Say, R, Unit](Say.Line(s"$a$n")).map(_ => if a then n else -n)))
    val run = E.handle(writer[List[Int]])(E.handle(reader[List[Int]](10))(E.handle(every[Int])(p)))
    assertEquals(E.run(run), (List("true10", "false10"), List(10, -10)))
    // the choice handled LAST: the other handlers' answers are reified into the tree twice over
    val run2 = E.handle(every[(List[String], Int)])(E.handle(writer[Int])(E.handle(reader[Int](10))(p)))
    assertEquals(E.run(run2), List((List("true10"), 10), (List("false10"), -10)))

  test("an answering handler, the state shared by the resumptions, through the tree"):
    val E = Effects[Rowed]
    type R = State % Int +: Say +: Pure
    val p: Rowed[R, Int] =
      E.perform[State % Int, R, Int](State.Get[Int]()).flatMap(n =>
        E.perform[State % Int, R, Unit](State.Put(n + 1)).flatMap(_ =>
          E.perform[Say, R, Unit](Say.Line(s"$n")).flatMap(_ =>
            E.perform[State % Int, R, Int](State.Get[Int]()))))
    assertEquals(E.run(E.handle(writer[(Int, Int)])(E.handle(StateCell[Int, Int](41))(p))), (List("41"), (42, 42)))

  test("100 000 operations through the tree, in constant stack"):
    val E = Effects[Rowed]
    def loop(n: Int, acc: Int): Rowed[Ask +: Pure, Int] =
      if n == 0 then E.pure(acc) else E.perform[Ask, Ask +: Pure, Int](Ask.Number).flatMap(x => E.tailcall(loop(n - 1, acc + x)))
    assertEquals(E.run(E.handle(reader[Int](1))(loop(100000, 0))), 100000)

  test("the facade's words over the tree, chosen by the import: the same program as the core's TestEffectsRows"):
    import okay.freer.tree.*
    val counting: Int ! (State % Int +: Say +: Pure) =
      for
        n <- effect(State.Get[Int]())
        _ <- effect(State.Put(n + 1))
        _ <- effect(Say.Line(s"$n"))
        m <- effect(State.Get[Int]())
      yield m
    assertEquals(counting.handle(StateCell[Int, Int](41)).handle(writer[(Int, Int)]).value, (List("41"), (42, 42)))
    assertEquals(counting.handle(writer[Int]).handle(StateCell[Int, (List[String], Int)](41)).value, (42, (List("41"), 42)))
