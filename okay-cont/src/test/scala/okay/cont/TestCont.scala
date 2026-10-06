package okay.cont

import Cont.*

/** specs/freer-min.md: HANDLERS AS DELIMITERS — an operation is a capture to the handler's delimiter, the clause
 * runs outside; what a program may perform is what its context reaches; a handler not the nearest is reached by
 * one capture through the delimiters between, found in the context at compile time */
class TestCont extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]
  enum Say[+A]:
    case Line(s: String) extends Say[Unit]

  /** a reader: `Number` is `n`; the value as it is */
  def reader[A](n: Int): Handler[Ask, A, A] = new Handler[Ask, A, A]:
    def ret(a: A): A = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Cont[o.Here, o.Here, A]): Cont[o.Here, o.Here, A] = op match
      case Ask.Number => k(n)

  /** a writer: the lines said, beside the value; deep — `k` brings the delimiter, each line goes in front */
  def writer[A]: Handler[Say, A, (List[String], A)] = new Handler[Say, A, (List[String], A)]:
    def ret(a: A): (List[String], A) = (Nil, a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Say[X], k: X => Cont[o.Here, o.Here, (List[String], A)]): Cont[o.Here, o.Here, (List[String], A)] =
      op match
        case Say.Line(s) => k(()).map((log, a) => (s :: log, a))

  def value[A](p: Top[A]): A =
    val head: Top[A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("one handler: the operation is a capture to its delimiter, the clause outside; the effect is gone from the row outside"):
    val prog: Top[Int] = handle(reader[Int](41))(perform(Ask.Number).map(_ + 1))
    assertEquals(value(prog), 42)

  test("two handlers: the inner one forwards what it does not handle — found in the context, at compile time"):
    val prog: Top[(List[String], Int)] =
      handle(reader[(List[String], Int)](41)):
        handle(writer[Int]):
          for
            n <- perform(Ask.Number)
            _ <- perform(Say.Line(n.toString))
            m <- perform(Ask.Number)
            _ <- perform(Say.Line((n + m).toString))
          yield n + 1
    assertEquals(value(prog), (List("41", "82"), 42))

  test("an operation with no handler in the context is no program"):
    val errors = compileErrors("""
      val prog: Top[Int] = reset[Int](perform(Ask.Number).map(_ + 1))""")
    note(errors)
    assert(errors.nonEmpty, errors)

  test("an operation whose handler is not in the context is refused: a reader's body performing Say"):
    val errors = compileErrors("""
      val prog: Top[Int] = handle(reader[Int](1))(perform(Say.Line("x")).map(_ => 1))""")
    assert(errors.nonEmpty, errors)

  test("100 000 operations, each a capture to the handler, in constant stack"):
    def loop(n: Int, acc: Int)(using c: In[Int, Root.type], p: Perform[Ask, c.type]): c.Body[Int] =
      if n == 0 then pure(acc) else perform(Ask.Number).flatMap(x => loop(n - 1, acc + x))
    val prog: Top[Int] = handle(reader[Int](1))(loop(100000, 0))
    assertEquals(value(prog), 100000)

  test("delay and defer: a million mutual tail calls in constant stack, through the one loop"):
    def even(n: Int): Top[Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(even(1000000)), true)
    assertEquals(value(odd(1000001)), true)

  test("100 000 left-nested maps run in constant stack"):
    val chain = (1 to 100000).foldLeft(pure(0): Top[Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)

  enum Choose[+A]:
    case Flip extends Choose[Boolean]
  def every[A]: Handler[Choose, A, List[A]] = new Handler[Choose, A, List[A]]:
    def ret(a: A): List[A] = List(a)
    def apply[X, Oc <: Ctx](using o: Oc)(op: Choose[X], k: X => Cont[o.Here, o.Here, List[A]]): Cont[o.Here, o.Here, List[A]] = op match
      case Choose.Flip => k(true).flatMap(xs => k(false).map(ys => xs ++ ys))

  test("THREE HANDLERS, ONE CAPTURE EACH: Ask crosses two delimiters, Say one, under a multi-shot handler — every world, every line, in order"):
    val prog: Top[(List[String], List[Int])] =
      handle(reader[(List[String], List[Int])](10)):
        handle(writer[List[Int]]):
          handle(every[Int]):
            for
              a <- perform(Choose.Flip)
              n <- perform(Ask.Number)
              _ <- perform(Say.Line(s"$a$n"))
            yield if a then n else -n
    assertEquals(value(prog), (List("true10", "false10"), List(10, -10)))
