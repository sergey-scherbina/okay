package okay.cont

import Machine.value

/** specs/freer-min.md, stage 27: `Free[R, A]` with rows, over `Cont` */
class TestFree extends okay.testkit.Munit.Diagnosed:
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


  /** a program over a row, no handler in sight: the rows join as it is built */
  val prog: Free[Ask +: Say +: Pure, Int] =
    for
      n <- Free.inject(Ask.Number)
      _ <- Free.inject(Say.Line(n.toString))
    yield n + 1

  test("a Free program with a row runs under its handlers, head effect first"):
    val run: Free[Pure, (List[String], Int)] = Free.handle(writer[Int])(Free.handle(reader[Int](41))(prog))
    assertEquals(value(Free.top(run)), (List("41"), 42))

  test("the handlers in the other order, the row as it was: a handler takes its effect off wherever it is"):
    val run: Free[Pure, (List[String], Int)] = Free.handle(reader[(List[String], Int)](41))(Free.handle(writer[Int])(prog))
    assertEquals(value(Free.top(run)), (List("41"), 42))

  test("the same effects in another order are one type of program: no widen written"):
    def wants(p: Free[Say +: Ask +: Pure, Int]): Free[Say +: Ask +: Pure, Int] = p
    val run = Free.handle(writer[Int])(Free.handle(reader[Int](41))(wants(prog)))
    assertEquals(value(Free.top(run)), (List("41"), 42))
    assertEquals(value(Free.top(Free.handle(reader[(List[String], Int)](41))(Free.handle(writer[Int])(wants(prog))))), (List("41"), 42))
    // the unions, by name
    summon[Union[Ask +: Say +: Pure, Int] =:= Union[Say +: Ask +: Pure, Int]]
    summon[Union[Ask +: Say +: Pure, Int] =:= (Ask[Int] | Say[Int])]

  test("a row with fewer effects is not widened silently"):
    assert(compileErrors("""def wants(p: Free[Say +: Ask +: Choose +: Pure, Int]): Unit = (); wants(prog)""").nonEmpty)

  test("widen: a row with the same effects in another order, by Sub"):
    val wide: Free[Say +: Ask +: Pure, Int] = prog.widen
    assertEquals(value(Free.top(Free.handle(writer[Int])(Free.handle(reader[Int](41))(wide)))), (List("41"), 42))

  test("an effect not in the row cannot be injected into it; a row cannot be run with a handler missing"):
    assert(compileErrors("""val p: Free[Ask +: Pure, Unit] = Free.inject(Say.Line("x"))""").nonEmpty)
    assert(compileErrors("""val r: Free[Pure, Int] = Free.handle(reader[Int](1))(prog)""").nonEmpty)

  test("a multi-shot handler over Free: every world, every line, in order"):
    val p: Free[Choose +: Ask +: Say +: Pure, Int] =
      for
        a <- Free.inject(Choose.Flip)
        n <- Free.inject(Ask.Number)
        _ <- Free.inject(Say.Line(s"$a$n"))
      yield if a then n else -n
    val run = Free.handle(writer[List[Int]])(Free.handle(reader[List[Int]](10))(Free.handle(every[Int])(p)))
    assertEquals(value(Free.top(run)), (List("true10", "false10"), List(10, -10)))

  test("100 000 operations over Free in constant stack; the join keeps every occurrence, `widen` folds them"):
    def loop(n: Int, acc: Int): Free[Ask +: Pure, Int] =
      if n == 0 then Free.pure(acc).widen else Free.inject(Ask.Number).flatMap(x => loop(n - 1, acc + x)).widen
    assertEquals(value(Free.top(Free.handle(reader[Int](1))(loop(100000, 0)))), 100000)

  test("the classic spelling: A ! R over the machine's row, % for a parameterised effect, run at the end"):
    val counting: Int ! (State % Int +: Say +: Pure) =
      for
        n <- effect(State.Get[Int]())
        _ <- effect(State.Put(n + 1))
        _ <- effect(Say.Line(s"$n"))
        m <- effect(State.Get[Int]())
      yield m
    val run: (List[String], (Int, Int)) ! Pure = Free.handle(writer[(Int, Int)])(Free.handle(StateCell[Int, Int](41))(counting))
    assertEquals(run.value, (List("41"), (42, 42)))
