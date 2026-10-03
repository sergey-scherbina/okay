package okay

import okay.Freer.{Bind, Delay, Diag, Return}

/**
 * specs/freer-kont.md, the oracle: the `($v)` and `($/S0)` rules of
 * λ$ (Materzok & Biernacki, APLAS 2012), TestDollar's bodies without
 * prompts' machinery, the depth tests, the head form. Every expected
 * value is the one TestDollar pins for the same body on the Shift
 * machine.
 */
class TestKont extends munit.FunSuite:

  /** a foreign operation: nobody on the stack answers it */
  enum Ask[+A]:
    case Get() extends Ask[Int]

  type F = Freer.Lift[Ask]
  type P[S, R, A] = Freer[Cont0.Row[F], S, R, A]
  /** every index the same: the shape of an unmodified program */
  type Str = P[String, String, String]

  def pure[S, A](a: A): P[S, S, A] = Return(a)

  /** run under a boundary, as `Shift.run` does: a capture that finds no
   * delimiter is `NoPrompt` by name, not an operation let out */
  def run[S, A](p: P[S, S, A]): A =
    LambdaDollar.machine[F].runHead[S, S, A](LambdaDollar.machine[F].reset[S, S, A](Cont0.boundary[A, S])(p)) match
      case Return(a) => a
      case other => fail(s"not a value: $other")

  def shift0(p: Prompt[String])(f: (String => Str) => Str): Str =
    LambdaDollar.machine[F].shift0[String, String, String, String, String](Cont0.delimiter(p))(f)

  def shift(p: Prompt[String])(f: (String => Str) => Str): Str =
    LambdaDollar.machine[F].shift[String, String, String, String, String](Cont0.delimiter(p))(f)

  def dollar(p: Prompt[String])(v: String => Str)(e: Str): Str =
    LambdaDollar.machine[F].dollar[String, String, String, String](Cont0.delimiter(p))(v)(e)

  def reset(p: Prompt[String])(e: Str): Str = LambdaDollar.machine[F].reset[String, String, String](Cont0.delimiter(p))(e)

  val angle: String => Str = x => pure(s"<$x>")

  // ------------------------------------------------ the two rules

  test("($v): a body that returns runs ret once") {
    val p = Cont0.prompt[String]
    assertEquals(run(dollar(p)(angle)(pure("x"))), "<x>")
  }

  test("($/S0): k dropped — ret never runs") {
    val p = Cont0.prompt[String]
    assertEquals(run(dollar(p)(angle)(shift0(p)(_ => pure("dropped")).map(_ + "!"))), "dropped")
  }

  test("($/S0): k called twice — ret runs per call") {
    val p = Cont0.prompt[String]
    val body = shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!><b!>")
  }

  test("($/S0): k once, then the rest of the clause") {
    val p = Cont0.prompt[String]
    val body = shift0(p)(k => k("a").map(_ + "|after")).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!>|after")
  }

  test("shift under a dollar: the body runs under a PLAIN delimiter, k carries ret") {
    val p = Cont0.prompt[String]
    val body = shift(p)(k => k("a").map(_ + "|body")).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!>|body")
  }

  test("k RE-INSTALLS the dollar: a second shift0 inside the continuation is caught by it") {
    val p = Cont0.prompt[String]
    val body = shift0(p)(k => k("a")).flatMap(s => shift0(p)(_ => pure(s + "-second")))
    assertEquals(run(dollar(p)(angle)(body)), "a-second")
  }

  test("ret runs OUTSIDE the re-installed dollar: a shift0 in ret escapes to the next delimiter") {
    val p = Cont0.prompt[String]
    val ret: String => Str = _ => shift0(p)(_ => pure("R"))
    val body = shift0(p)(k => k("a").map(_ + "|f"))
    assertEquals(run(reset(p)(dollar(p)(ret)(body))), "R")
  }

  test("nested delimiters: an inner prompt's shift0 does not reach the outer one, and the other way about") {
    val p = Cont0.prompt[String]
    val q = Cont0.prompt[String]
    // the inner shift0 names q: its k is the inner segment only; the outer $ still runs its ret
    val inner = dollar(q)(x => pure(s"[$x]"))(shift0(q)(k => k("i")).map(_ + "!"))
    assertEquals(run(dollar(p)(angle)(inner)), "<[i!]>")
    // naming p from inside q: k spans BOTH delimiters and both rets
    val across = dollar(q)(x => pure(s"[$x]"))(shift0(p)(k => k("o")).map(_ + "!"))
    assertEquals(run(dollar(p)(angle)(across)), "<[o!]>")
  }

  // ------------------------------------------------ answer-type modification

  test("the delimiter changes the VALUE type: the body answers Int, ret and the $ answer String, k twice") {
    // Cont0.Prompt[S, Y]: Y = String is what the delimiter answers, the body's value is Int;
    // the index S stays one type through the run — see the spec's Results on why the lazy
    // machine's (S, R) pair cannot carry an ESCAPE type the way Cont's strict k can
    val p = Cont0.prompt[String]
    val ret: Int => P[Int, Int, String] = n => pure(s"n=$n")
    val body: P[Int, Int, Int] =
      LambdaDollar.machine[F].shift0[String, Int, Int, Int, Int](Cont0.delimiter(p))(k => k(1).flatMap(s => k(2).map(t => s + t)))
    val prog: P[Int, Int, String] = LambdaDollar.machine[F].dollar[String, Int, Int, Int](Cont0.delimiter(p))(ret)(body)
    assertEquals(run(prog), "n=1n=2")
  }

  // ------------------------------------------------ depth

  test("depth: 100 000 dollars nested, each ret run once on the way out, in constant stack") {
    val p = Cont0.prompt[Int]
    def nest(n: Int): P[Int, Int, Int] =
      if n == 0 then pure(0)
      else LambdaDollar.machine[F].dollar[Int, Int, Int, Int](Cont0.delimiter(p))(x => pure(x + 1))(Delay(() => nest(n - 1)))
    assertEquals(run(nest(100_000)), 100_000)
  }

  test("depth: a generator of 100 000 yields, each clause resuming k — lazy resumptions, one loop") {
    type L = List[Int]
    val p = Cont0.prompt[L]
    def emit(i: Int): P[L, L, Unit] =
      LambdaDollar.machine[F].shift0[L, L, L, L, Unit](Cont0.delimiter(p))(k => k(()).map(i :: _))
    def body(i: Int, n: Int): P[L, L, Unit] =
      if i > n then pure(()) else emit(i).flatMap(_ => Delay(() => body(i + 1, n)))
    val prog = LambdaDollar.machine[F].dollar[L, Unit, L, L](Cont0.delimiter(p))(_ => pure(Nil))(body(1, 100_000))
    val got = run(prog)
    assertEquals(got.length, 100_000)
    assertEquals(got.take(3), List(1, 2, 3))
  }

  test("depth: 100 000 left-nested binds run without a rotation, in constant stack") {
    val prog = (1 to 100_000).foldLeft(pure[Int, Int](0))((acc, _) => acc.flatMap(x => pure[Int, Int](x + 1)))
    assertEquals(run(prog), 100_000)
  }

  test("depth: a k of 100 000 frames, resumed twice") {
    val p = Cont0.prompt[Int]
    // the frames pile up under the shift0: each recursive step adds a map
    def deep(n: Int): P[Int, Int, Int] =
      if n == 0 then LambdaDollar.machine[F].shift0[Int, Int, Int, Int, Int](Cont0.delimiter(p))(k => k(0).flatMap(a => k(1).map(b => a + b)))
      else Delay(() => deep(n - 1)).map(_ + 1)
    assertEquals(run(LambdaDollar.machine[F].reset[Int, Int, Int](Cont0.delimiter(p))(deep(100_000))), 200_001)
  }

  // ------------------------------------------------ the two costs the segmented stack pays O(1)

  test("depth: 20 000 captures to ONE delimiter from under 20 000 frames — each capture takes the segment, copies no frame") {
    // left-nested: at the i-th shift0 the n - i binds still to run sit
    // between it and the reset; the single-list machine copied them per
    // capture (O(n²)), the segmented one takes the segment as it is
    val n = 20_000
    val p = Cont0.prompt[Int]
    val prog = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.flatMap(x => LambdaDollar.machine[F].shift0[Int, Int, Int, Int, Int](Cont0.delimiter(p))(k => k(x + 1))))
    val t0 = System.nanoTime()
    assertEquals(run(LambdaDollar.machine[F].reset[Int, Int, Int](Cont0.delimiter(p))(prog)), n)
    assert(System.nanoTime() - t0 < 5_000_000_000L, "20 000 captures took seconds: a capture is copying frames")
  }

  test("depth: 100 000 NESTED resumptions, k(v) + 1 at every level — each pushes k's nodes, copies no frame") {
    // each body waits on its k, and each k holds every frame below it:
    // the single-list machine copied them per resumption (O(n²))
    val n = 100_000
    val p = Cont0.prompt[Int]
    val prog = (1 to n).foldLeft(pure[Int, Int](0))((m, _) =>
      m.flatMap(x => LambdaDollar.machine[F].shift0[Int, Int, Int, Int, Int](Cont0.delimiter(p))(k => k(x + 1).map(_ + 1))))
    val t0 = System.nanoTime()
    assertEquals(run(LambdaDollar.machine[F].reset[Int, Int, Int](Cont0.delimiter(p))(prog)), 2 * n)
    assert(System.nanoTime() - t0 < 5_000_000_000L, "100 000 nested resumptions took seconds: a resumption is copying frames")
  }

  // ------------------------------------------------ the head form

  /** a unary operation enters an indexed row on the DIAGONAL (`Freer.Diag`),
   * which is what lets a handler loop keep its indexes (State.handleIndexed) */
  val ask: P[String, String, Int] = Freer.diag[Cont0.Row[F], String, Int](Ask.Get())

  test("a foreign operation comes out as Bind(Inject(e), k); k fed twice gives two answers, the shift0 after it handled") {
    val p = Cont0.prompt[String]
    val body: Str = ask.flatMap(n => shift0(p)(k => k(n.toString).map(_ + "!")))
    LambdaDollar.machine[F].runHead[String, String, String](dollar(p)(angle)(body)) match
      case Bind(Diag(_: Ask[?]), k) =>
        val fs = k.asInstanceOf[Int => P[String, String, String]]
        assertEquals(run(fs(1)), "<1>!")
        assertEquals(run(fs(2)), "<2>!")
      case other => fail(s"not the head form: $other")
  }

  /**
   * A HANDLER LOOP OF TODAY'S SHAPE, over the real `Freer.resume` (the
   * rotation), answering `Ask` and forwarding everything else — the
   * outer interpreter the machine's head form is handed to. It knows
   * nothing of frames; what it gets as `k` must bring the machine back.
   */
  def answer[S, T, A](n: Int)(p: P[S, T, A]): P[S, T, A] =
    @scala.annotation.tailrec def loop(x: P[S, T, A]): P[S, T, A] = (x.resume: @unchecked) match
      case _: Return[?, ?, ?] => x
      case b: Bind[Cont0.Row[F], S, t, T, x0, A] => b.a match
        case Diag(_: Ask[?]) => loop(b.f.asInstanceOf[Int => P[S, T, A]](n))
        case i => Bind(i, (v: x0) => answer[S, t, A](n)(b.f(v)))
      case i => i
    loop(p)

  test("orthogonal: a loop handler over Freer.resume OUTSIDE the machine; its k re-enters the machine, shift0 still finds the delimiter") {
    val p = Cont0.prompt[String]
    // the ask is answered by the loop outside; the shift0 after it is the machine's, and its k is resumed twice
    val body: Str = ask.flatMap(n => shift0(p)(k => k(n.toString).flatMap(a => k((n + 1).toString).map(b => a + b))))
    val prog = answer[String, String, String](7)(LambdaDollar.machine[F].runHead[String, String, String](dollar(p)(angle)(body)))
    assertEquals(run(prog), "<7><8>")
  }

  test("a shift0 with no dollar for its prompt fails by name and lists the installed delimiters") {
    val p = Cont0.prompt[String]
    val q = Cont0.prompt[String]
    val e = intercept[NoPrompt](run(dollar(p)(angle)(shift0(q)(_ => pure("x")))))
    assertEquals(e.wanted, q.label)
    assertEquals(e.installed, List(p.label))
  }
