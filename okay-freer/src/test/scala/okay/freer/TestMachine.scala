package okay.freer

import scala.annotation.tailrec
import Freer.*

/** specs/freer-min.md, the machine: one rule per node, no cast, shift0 and reset, Danvy–Filinski's answer types */
class TestMachine extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]


  def value[U, A](p: Freer[Pure, Lvl[Pure, U, U] *: EmptyTuple, A]): A =
    val head: Freer[Pure, Lvl[Pure, U, U] *: EmptyTuple, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  @tailrec final def runAsk[A](p: Top[Ask, A], in: Int): A =
    val head: Top[Ask, A] = Machine.run(p)
    head match
      case Return(a) => a
      case Bind(Inject(op), k) => op match
        case Ask.Number => runAsk(k(in), in)
      case other => fail(s"not handled: $other")

  test("shift: the continuation, delimiter included, applied twice"):
    assertEquals(value(reset[Pure, Int](shift0[Int](k => k(1).flatMap(k(_))).map(_ * 2))), 4)

  test("multi-shot: three resumptions, their values summed"):
    assertEquals(value(reset[Pure, Int](shift0[Int](k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))), 60)

  test("ANSWER-TYPE MODIFICATION, Danvy–Filinski's own: reset(1 + shift(k => \"a\")) answers a String"):
    val prog: Freer[Pure, Lvl[Pure, String, String] *: EmptyTuple, String] = reset[Pure, Int](shift0[Int](_ => pure("a")).map(_ + 1))
    assertEquals(value(prog), "a")

  test("answer-type modification with the continuation used: the answer is a String built from the Int context"):
    // k : Int => Int [Int, Int] — the context `_ + 1` with the delimiter; the body answers a String
    val prog: Freer[Pure, Lvl[Pure, String, String] *: EmptyTuple, String] = reset[Pure, Int](shift0[Int](k => k(1).flatMap(a => k(a)).map(n => "n=" + n)).map(_ + 1))
    assertEquals(value(prog), "n=3")

  test("abort: a shift that drops its continuation answers the delimiter at once"):
    assertEquals(value(reset[Pure, Int](shift0[Int](_ => pure(0)).map(_ + 1))), 0)

  test("nested delimiters: a shift's delimiter is the reset it is written in, the inner given"):
    assertEquals(value(reset[Pure, Int](reset[Pure, Int](shift0[Int](k => k(1)).map(_ + 10)).map(_ * 2))), 22)

  test("a capture with no delimiter on the run is handed out as a head form, for a machine outside"):
    // the node itself, at a stack claiming a delimiter: the sugar refuses a capture with no delimiter in scope
    val head = Machine.run(Shift0[Pure, Pure, EmptyTuple, Int, Int, Int, Int](k => k(1)))
    head match
      case Bind(Shift0(_), _) => ()
      case other => fail(s"not a capture handed out: $other")

  test("an effect inside a delimiter is handed out, answered outside, and the run goes on"):
    val prog: Top[Ask, Int] = reset[Ask, Int](
      for
        n <- inject(Ask.Number)
        x <- shift0[Int](k => k(n).flatMap(k(_)))
      yield x + 1)
    assertEquals(runAsk(prog, 10), 12)

  test("a delimiter's row is its body's: a program at Top[Pure, Int] cannot hold a delimiter over Other"):
    val errors = compileErrors("""
      enum Other[+A]:
        case Op extends Other[Int]
      val prog: Top[Pure, Int] = reset[Pure, Int](inject(Other.Op).map(_ + 1))""")
    note(errors)
    assert(errors.nonEmpty, errors)

  test("a program wider than its prompt's row: the delimiter's k stays at the prompt's row, the rest is wider"):
    val wide: Top[Ask + Pure, Int] = reset[Pure, Int](shift0[Int](k => k(1)).map(_ + 1)).flatMap(x => inject(Ask.Number).map(_ + x))
    assertEquals(runAsk(wide, 10), 12)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    // a fragment with a shift says which delimiter it is under: its context, and its index
    def loop(n: Int)(using in: Under[Pure, Int]): in.Body[Pure, Int, Int] =
      if n == 0 then pure(0) else shift0[Int](k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value(reset[Pure, Int](loop(100000))), 100000)

  test("a continuation that escaped its delimiter is a program: run later, its delimiter comes with it"):
    // the body knows the level outside only as the context's `in.Out`: the escaped `k` is a program there, and is
    // run there, by a fragment of that context
    def escaping(using in: Under[Pure, Int]): (in.Body[Pure, Int, Int], () => Int) =
      var saved: Int => Freer[Pure, in.Out, Int] = null
      val body = shift0[Int](k => { saved = x => k(x); pure(0) }).map(_ + 1)
      def run(): Int =
        val head: Freer[Pure, in.Out, Int] = Machine.run(saved(41))
        head match
          case Return(a) => a
          case other => fail(s"not a value: $other")
      (body, () => run())
    var later: () => Int = null
    assertEquals(value(reset[Pure, Int] { val (body, run) = escaping; later = run; body }), 0)
    assertEquals(later(), 42)

  test("a run over a run allocates nothing: a head form handed out comes back as itself"):
    val head = Machine.run(reset[Ask, Int](inject(Ask.Number).map(_ + 1)))
    assert(Machine.run(head) eq head)

  test("the machine runs plain programs too: a million deferred calls, 100 000 left-nested maps"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(odd(1000001)), true)
    val chain = (1 to 100000).foldLeft(pure(0): Top[Pure, Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)
