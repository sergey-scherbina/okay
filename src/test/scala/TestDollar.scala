package okay

/**
 * specs/shift0-dollar.md STAGE 1: `Delim.dollar`, λ$'s primitive
 * delimiter, in the machine. Every expected value below was worked by
 * hand from APLAS 2012's rules before the first run:
 *
 *     v' $ v      ⇝ v' v                   ($v)
 *     K̂[S0 f.e]   ⇝ e[λx.K̂[x]/f]          ($/S0)   K̂ = v $ K
 */
class TestDollar extends munit.FunSuite:

  type Row = Delim + Pure

  def run[A](p: A ! Row): A = !.run(Delim.run[A, Pure](p))

  def shift0(p: Prompt[String])(f: (String => String ! Row) => String ! Row): String ! Row =
    Delim.shift0[String, String, Pure](p)(f)

  def dollar(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Delim.dollar[String, String, Pure](p)(v)(e)

  /** stage 0's reference: APLAS 2012's `$` through reset0 and shift0 */
  def dollarMacro(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Delim.push(p)(e.flatMap(x => shift0(p)(_ => v(x))))

  val angle: String => String ! Row = x => okay.pure(s"<$x>")

  /** the bodies stage 0 separated the encodings with, plus two more */
  val bodies: List[(String, Prompt[String] => String ! Row)] = List(
    "returns" -> (_ => okay.pure("x")),
    "drops k" -> (p => shift0(p)(_ => okay.pure("dropped")).map(_ + "!")),
    "k twice" -> (p => shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")),
    "k once" -> (p => shift0(p)(k => k("a").map(_ + "|after")).map(_ + "!")),
    "shift under" -> (p => Delim.shift[String, String, Pure](p)(k => k("a").map(_ + "|body")).map(_ + "!")),
    "abort" -> (p => Delim.abort[String, String, Pure](p)("gone").map(_ + "!")))

  // ------------------------------------------------ the two rules

  test("($v): a body that returns runs ret once") {
    val p = Delim.prompt[String]
    assertEquals(run(dollar(p)(angle)(okay.pure("x"))), "<x>")
  }

  test("($/S0): k dropped — ret never runs") {
    val p = Delim.prompt[String]
    assertEquals(run(dollar(p)(angle)(shift0(p)(_ => okay.pure("dropped")).map(_ + "!"))), "dropped")
  }

  test("($/S0): k called twice — ret runs per call") {
    val p = Delim.prompt[String]
    val body = shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!><b!>")
  }

  test("shift under a dollar: the body runs under a PLAIN delimiter, k carries ret") {
    val p = Delim.prompt[String]
    val body = Delim.shift[String, String, Pure](p)(k => k("a").map(_ + "|body")).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!>|body")
  }

  test("k RE-INSTALLS the dollar: a second shift0 inside the continuation is caught by it, not by the outside") {
    // k = λx. ⟨·⟩ $ E[x] with E[x] = x >>= (s => S0 _. s ++ "-second"): the
    // re-installed dollar catches the second capture, and it drops ITS k,
    // so ret does not run on that path
    val p = Delim.prompt[String]
    val body = shift0(p)(k => k("a")).flatMap(s => shift0(p)(_ => okay.pure(s + "-second")))
    assertEquals(run(dollar(p)(angle)(body)), "a-second")
  }

  // ------------------------------------------------ against stage 0's references

  test("the primitive agrees with APLAS 2012's macro on every body") {
    for (name, body) <- bodies do
      val p = Delim.prompt[String]
      val q = Delim.prompt[String]
      assertEquals(run(dollar(p)(angle)(body(p))), run(dollarMacro(q)(angle)(body(q))), name)
  }

  test("reset0 = pure $: dollar with the unit as ret is push, on every body") {
    for (name, body) <- bodies do
      val p = Delim.prompt[String]
      val q = Delim.prompt[String]
      assertEquals(run(dollar(p)(okay.pure)(body(p))), run(Delim.push(q)(body(q))), name)
  }

  // ------------------------------------------------ what the macro could not type

  test("R0 and R differ: an Int body leaves through ret as a String, and k answers the String") {
    val p = Delim.prompt[String]
    val body: Int ! Row =
      Delim.shift0[String, Int, Pure](p)(k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
    assertEquals(run(Delim.dollar[Int, String, Pure](p)(i => okay.pure(s"n=$i"))(body)), "n=10|n=20")
  }

  // ------------------------------------------------ nesting: ICFP 2011's example, with rets

  test("two dollars at one prompt: ⟨[·] $ \"Alice\" ++ ({·} $ \" has \" ++ S0 k1. S0 k2. \"A cat\" ++ k1 (k2 \".\"))⟩") {
    // k1 = λx. {·} $ (" has " ++ x),  k2 = λy. [·] $ ("Alice" ++ y)
    // "A cat" ++ k1 (k2 ".") = "A cat" ++ "{ has [Alice.]}"
    val p = Delim.prompt[String]
    val square: String => String ! Row = s => okay.pure(s"[$s]")
    val curly: String => String ! Row = s => okay.pure(s"{$s}")
    val inner = shift0(p)(k1 => shift0(p)(k2 => k2(".").flatMap(k1).map("A cat" + _)))
    val prog = dollar(p)(square)(dollar(p)(curly)(inner.map(" has " + _)).map("Alice" + _))
    assertEquals(run(prog), "A cat{ has [Alice.]}")
  }

  // ------------------------------------------------ what is refused, and depth

  test("a control-capture to a dollar is refused: its bare continuation answers the body's type") {
    for ctl <- List("control", "control0") do
      val p = Delim.prompt[String]
      val body =
        if ctl == "control" then Delim.control[String, String, Pure](p)(k => k("a"))
        else Delim.control0[String, String, Pure](p)(k => k("a"))
      val e = intercept[UnsupportedOperationException](run(dollar(p)(angle)(body)))
      assert(e.getMessage.contains("dollar"), e.getMessage)
  }

  test("depth: 100 000 dollars nested, each ret run once on the way out, in constant stack") {
    val p = Delim.prompt[Int]
    def nest(n: Int): Int ! Row =
      if n == 0 then okay.pure(0)
      else Delim.dollar[Int, Int, Pure](p)(x => okay.pure(x + 1))(Free.delay(() => nest(n - 1)))
    assertEquals(run(nest(100_000)), 100_000)
  }
