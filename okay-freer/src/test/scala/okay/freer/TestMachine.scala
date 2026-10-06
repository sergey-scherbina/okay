package okay.freer

import Cont.*

/** specs/freer-min.md, the machine: one rule per node, no cast, shift0 and reset, Danvy–Filinski's answer types */
class TestMachine extends okay.testkit.Munit.Diagnosed:
  enum Ask[+A]:
    case Number extends Ask[Int]

  /** a reader: `Number` is `n` */
  def reader[G[+_], A](n: Int): Handler[Ask, G, A, A] = new Handler[Ask, G, A, A]:
    def ret(a: A): A = a
    def apply[X, Oc <: Ctx](using o: Oc)(op: Ask[X], k: X => Cont[G, o.Here, o.Here, A]): Cont[G, o.Here, o.Here, A] = op match
      case Ask.Number => k(n)

  def value[A](p: Top[Pure, A]): A =
    val head: Top[Pure, A] = Machine.run(p)
    head match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  test("shift: the continuation, delimiter included, applied twice"):
    assertEquals(value(reset[Pure, Int](shift0[Int](k => k(1).flatMap(k(_))).map(_ * 2))), 4)

  test("multi-shot: three resumptions, their values summed"):
    assertEquals(value(reset[Pure, Int](shift0[Int](k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a + b + c)))).map(_ * 10))), 60)

  test("ANSWER-TYPE MODIFICATION, Danvy–Filinski's own: reset(1 + shift(k => \"a\")) answers a String"):
    val prog: Top[Pure, String] = (reset[Pure, Int](shift0[Int](_ => pure("a")).map(_ + 1)))
    assertEquals(value(prog), "a")

  test("answer-type modification with the continuation used: the answer is a String built from the Int context"):
    // k : Int => Int [Int, Int] — the context `_ + 1` with the delimiter; the body answers a String
    val prog: Top[Pure, String] = (reset[Pure, Int](shift0[Int](k => k(1).flatMap(a => k(a)).map(n => "n=" + n)).map(_ + 1)))
    assertEquals(value(prog), "n=3")

  test("abort: a shift that drops its continuation answers the delimiter at once"):
    assertEquals(value(reset[Pure, Int](shift0[Int](_ => pure(0)).map(_ + 1))), 0)

  test("nested delimiters: a shift's delimiter is the reset it is written in, the inner given"):
    assertEquals(value(reset[Pure, Int](reset[Pure, Int](shift0[Int](k => k(1)).map(_ + 10)).map(_ * 2))), 22)

  /** THE HIERARCHY, in the nodes (the sugar leaves the outside as it is; a move of the outside is written in its
   * structure): the outer delimiter's body at Int, the inner one's at Int, the inner shift's body OUTSIDE the inner
   * delimiter shifts to the outer one and moves the outer level's answer from Int to String */
  type L2 = EmptyTuple
  type D1 = At[Pure, Pure, L2, Int] *: L2
  type O1 = At[Pure, Pure, L2, String] *: L2

  test("THE HIERARCHY: a shift's body, outside its delimiter, shifts to the next delimiter and MOVES ITS ANSWER, Int to String"):
    val inner: Cont[Pure, D1, O1, Int] = Reset[Pure, Pure, D1, O1, Int, Int](
      Shift0[Pure, Pure, D1, D1, O1, Int, Int, Int](_ => Shift0[Pure, Pure, L2, L2, L2, Int, String, Int](_ => pure("a"))).map(_ + 10))
    val prog: Top[Pure, String] = Reset[Pure, Pure, L2, L2, Int, String](inner.map(_ * 2))
    assertEquals(value(prog), "a")

  test("the hierarchy IN THE SUGAR: the context is structural, a body knows the levels outside it"):
    val prog: Top[Pure, String] = (reset[Pure, Int](reset[Pure, Int](shift0[Int](_ => shift0[Int](_ => pure("a"))).map(_ + 10)).map(_ * 2)))
    assertEquals(value(prog), "a")
    val twice: Top[Pure, Int] = (reset[Pure, Int](reset[Pure, Int](shift0[Int](k => k(1).flatMap(a => shift0[Int](k2 => k2(a).flatMap(b => k2(b))))).map(_ + 10)).map(_ * 2)))
    assertEquals(value(twice), 44)

  test("the hierarchy, resumed: the inner body resumes its own k, then shifts out through the outer delimiter, which resumes twice"):
    type D1i = At[Pure, Pure, EmptyTuple, Int] *: EmptyTuple
    val inner: Cont[Pure, D1i, D1i, Int] = Reset[Pure, Pure, D1i, D1i, Int, Int](
      Shift0[Pure, Pure, D1i, D1i, D1i, Int, Int, Int](k => k(1).flatMap(a => Shift0[Pure, Pure, EmptyTuple, EmptyTuple, EmptyTuple, Int, Int, Int](k2 => k2(a).flatMap(b => k2(b))))).map(_ + 10))
    val prog: Top[Pure, Int] = Reset[Pure, Pure, EmptyTuple, EmptyTuple, Int, Int](inner.map(_ * 2))
    // inner: k(1) = 11; outer: k2(11) = 22, k2(22) = 44
    assertEquals(value(prog), 44)

  test("a capture with no delimiter on the run is handed out as a head form, for a machine outside"):
    // the node itself, at a stack claiming a delimiter: the sugar refuses a capture with no delimiter in scope
    val head = Machine.run(Shift0[Pure, Pure, EmptyTuple, EmptyTuple, EmptyTuple, Int, Int, Int](k => k(1)))
    // at the row `Pure` dotty holds a `Bind` (at `F + G`) unreachable — `Pure + Pure` is `Pure`, but not to the
    // reachability check; the scrutinee at any row
    (head: Cont[[A] =>> Any, At[Pure, Pure, EmptyTuple, Int] *: EmptyTuple, At[Pure, Pure, EmptyTuple, Int] *: EmptyTuple, Int]) match
      case Bind(Shift0(_), _) => ()
      case other => fail(s"not a capture handed out: $other")

  test("an operation inside a delimiter reaches its handler outside: the delimiter forwards, and the run goes on"):
    val prog: Top[Pure, Int] = handle(reader[Pure, Int](10)):
      reset[Ask + Pure, Int]:
        for
          n <- perform(Ask.Number)
          x <- shift0[Int](k => k(n).flatMap(k(_)))
        yield x + 1
    assertEquals(value(prog), 12)

  test("a delimiter's row is its body's: a program at Top[Pure, Int] cannot hold a delimiter over Other"):
    val errors = compileErrors("""
      enum Other[+A]:
        case Op extends Other[Int]
      val prog: Top[Pure, Int] = reset[Other, Int](pure(1))""")
    note(errors)
    assert(errors.nonEmpty, errors)

  test("a program wider than a delimiter's row: the delimiter's k stays at its row, the rest is wider"):
    val wide: Top[Pure, Int] = handle(reader[Pure, Int](10))(reset[Pure, Int](shift0[Int](k => k(1)).map(_ + 1)).flatMap(x => perform(Ask.Number).map(_ + x)))
    assertEquals(value(wide), 12)

  test("100 000 captures and resumptions in one delimiter run in constant stack"):
    // a fragment with a shift says which delimiter it is under: its context, and its index
    def loop(n: Int)(using in: Under[Pure, Int]): in.Body[Int] =
      if n == 0 then pure(0) else shift0[Int](k => k(1)).flatMap(x => loop(n - 1).map(_ + x))
    assertEquals(value(reset[Pure, Int](loop(100000))), 100000)

  test("a continuation that escaped its delimiter is a program: run later, its delimiter comes with it"):
    // the body knows the level outside only as the context's `in.Out`: the escaped `k` is a program there, and is
    // run there, by a fragment of that context
    def escaping(using in: Under[Pure, Int]): (in.Body[Int], () => Int) =
      var saved: Int => Cont[Pure, in.D, in.D, Int] = null
      val body = shift0[Int](k => { saved = x => k(x); pure(0) }).map(_ + 1)
      def run(): Int =
        val head: Cont[Pure, in.D, in.D, Int] = Machine.run(saved(41))
        head match
          case Return(a) => a
          case other => fail(s"not a value: $other")
      (body, () => run())
    var later: () => Int = null
    assertEquals(value(reset[Pure, Int] { val (body, run) = escaping; later = run; body }), 0)
    assertEquals(later(), 42)

  test("a run over a run allocates nothing: a head form handed out comes back as itself"):
    val head = Machine.run(Shift0[Pure, Pure, EmptyTuple, EmptyTuple, EmptyTuple, Int, Int, Int](k => k(1)))
    assert(Machine.run(head) eq head)

  test("the machine runs plain programs too: a million deferred calls, 100 000 left-nested maps"):
    def even(n: Int): Top[Pure, Boolean] = if n == 0 then pure(true) else delay(odd(n - 1))
    def odd(n: Int): Top[Pure, Boolean] = if n == 0 then pure(false) else defer(even(n - 1))(b => pure(b))
    assertEquals(value(odd(1000001)), true)
    val chain = (1 to 100000).foldLeft(pure(0): Top[Pure, Int])((p, _) => p.map(_ + 1))
    assertEquals(value(chain), 100000)
