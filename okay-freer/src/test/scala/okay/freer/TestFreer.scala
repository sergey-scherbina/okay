package okay.freer

import Freer.*

/** specs/freer-min.md: HANDLERS AS DELIMITERS — an operation is a capture to the handler's delimiter, the clause
 * runs outside; the effect is discharged from the row outside; a handler not the nearest is reached through the
 * delimiters between, each forwarding, found in the context at compile time */
class TestFreer extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  /** a reader: `Number` is `n`; the value as it is */
  def reader[G[+_], A](n: Int): Handler[Ask, G, A, A] = new Handler[Ask, G, A, A]:
    def ret(a: A): A = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Freer[G, o.Here, o.Here, A]): Freer[G, o.Here, o.Here, A] = op match
      case Ask.Number => k(n)

  /** a writer: the lines said, beside the value; deep — `k` brings the delimiter, each line goes in front */
  def writer[G[+_], A]: Handler[Say, G, A, (List[String], A)] = new Handler[Say, G, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Say[X], k: X => Freer[G, o.Here, o.Here, (List[String], A)]): Freer[G, o.Here, o.Here, (List[String], A)] =
      op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a))

  def value[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("one handler: the operation is a capture to its delimiter, the clause outside; the effect is gone from the row outside"):
    val prog: Top[Pure, Int] = handle(reader[Pure, Int](41))(perform(Ask.Number).map(_ + 1))
    assertEquals(value(prog), 42)

  test("two handlers: the inner one forwards what it does not handle — found in the context, at compile time"):
    val prog: Top[Pure, (List[String], Int)] =
      handle(reader[Pure, (List[String], Int)](41)):
        handle(writer[Ask + Pure, Int]):
          for
            n <- perform(Ask.Number)
            _ <- perform(Say.Line(n.toString))
            m <- perform(Ask.Number)
            _ <- perform(Say.Line((n + m).toString))
          yield n + 1
    assertEquals(value(prog), (List("41", "82"), 42))

  test("an operation with no handler in the context is no program"):
    val errors = compileErrors("""
      val prog: Top[Pure, Int] = reset[Pure, Int](perform(Ask.Number).map(_ + 1))""")
    note(errors)
    assert(errors.nonEmpty, errors)

  test("a handler's body row says what is handled; a program claiming more is refused"):
    val errors = compileErrors("""
      val prog: Top[Pure, Int] = handle(reader[Pure, Int](1))(perform(Say.Line("x")).map(_ => 1))""")
    assert(errors.nonEmpty, errors)

  test("100 000 operations, each a capture to the handler, in constant stack"):
    def loop(n: Int, acc: Int)(using c: In[Ask + Pure, Pure, Int, Root.type], p: Perform[Ask, Ask + Pure, c.type]): c.Body[Int] =
      if n == 0 then pure(acc) else perform(Ask.Number).flatMap(x => loop(n - 1, acc + x))
    val prog: Top[Pure, Int] = handle(reader[Pure, Int](1))(loop(100000, 0))
    assertEquals(value(prog), 100000)

  test("delay and defer: a million mutual tail calls in constant stack, through the one loop"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(even(1000000)), true)
    assertEquals(value(odd(1000001)), true)

  test("100 000 left-nested maps run in constant stack"):
    val chain = (1 to 100000).foldLeft(pure(0): Top[Pure, Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)
