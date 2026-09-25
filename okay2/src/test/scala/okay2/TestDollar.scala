package okay2

/**
 * okay2-dollar: `Delim.dollar`, λ$'s primitive delimiter, in the okay2
 * machine — the twin of the Scala 3 core's TestDollar, with the same
 * expected values, worked from APLAS 2012's rules:
 *
 *     v' $ v      ⇝ v' v                   ($v)
 *     K̂[S0 f.e]   ⇝ e[λx.K̂[x]/f]          ($/S0)   K̂ = v $ K
 */
class TestDollar extends munit.FunSuite {

  type P = Pure
  type Row = Delim + P

  def run[A](p: A ! Row): A = !.run(Delim.run[A, P](p))

  def shift0(p: Prompt[String])(f: (String => String ! Row) => String ! Row): String ! Row =
    Delim.shift0[String, String, P](p)(f)

  def dollar(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Delim.dollar[String, String, P](p)(v)(e)

  /** APLAS 2012's `$` through reset0 and shift0: the reference */
  def dollarMacro(p: Prompt[String])(v: String => String ! Row)(e: String ! Row): String ! Row =
    Delim.push[String, P](p)(e.flatMap(x => shift0(p)(_ => v(x))))

  val angle: String => String ! Row = x => pure[Row, String](s"<$x>")

  val bodies: List[(String, Prompt[String] => String ! Row)] = List(
    "returns" -> (_ => pure[Row, String]("x")),
    "drops k" -> (p => shift0(p)(_ => pure[Row, String]("dropped")).map(_ + "!")),
    "k twice" -> (p => shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")),
    "k once" -> (p => shift0(p)(k => k("a").map(_ + "|after")).map(_ + "!")),
    "shift under" -> (p => Delim.shift[String, String, P](p)(k => k("a").map(_ + "|body")).map(_ + "!")),
    "abort" -> (p => Delim.abort[String, String, P](p)("gone").map(_ + "!")))

  // ------------------------------------------------ the two rules

  test("($v): a body that returns runs ret once") {
    val p = Delim.prompt[String]
    assertEquals(run(dollar(p)(angle)(pure[Row, String]("x"))), "<x>")
  }

  test("($/S0): k dropped — ret never runs") {
    val p = Delim.prompt[String]
    assertEquals(run(dollar(p)(angle)(shift0(p)(_ => pure[Row, String]("dropped")).map(_ + "!"))), "dropped")
  }

  test("($/S0): k called twice — ret runs per call") {
    val p = Delim.prompt[String]
    val body = shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!><b!>")
  }

  test("shift under a dollar: the body runs under a PLAIN delimiter, k carries ret") {
    val p = Delim.prompt[String]
    val body = Delim.shift[String, String, P](p)(k => k("a").map(_ + "|body")).map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "<a!>|body")
  }

  test("k RE-INSTALLS the dollar: a second shift0 inside the continuation is caught by it, not by the outside") {
    val p = Delim.prompt[String]
    val body = shift0(p)(k => k("a")).flatMap(s => shift0(p)(_ => pure[Row, String](s + "-second")))
    assertEquals(run(dollar(p)(angle)(body)), "a-second")
  }

  test("ret runs OUTSIDE the re-installed dollar: a shift0 in ret escapes past it to the next delimiter") {
    // k(v) = ret $ E[v], so ret is not under p: its shift0 takes the rest of
    // f (the "|f") up to the OUTER push. push(p)(E[v] >>= ret) would catch it
    val p = Delim.prompt[String]
    val ret: String => String ! Row = _ => shift0(p)(_ => pure[Row, String]("R"))
    val body = shift0(p)(k => k("a").map(_ + "|f"))
    assertEquals(run(Delim.push[String, P](p)(dollar(p)(ret)(body))), "R")
  }

  test("a capture to an OUTER prompt passing a dollar keeps it: k re-installs the dollar with its ret") {
    val p = Delim.prompt[String]
    val q = Delim.prompt[String]
    val body = Delim.shift0[String, String, P](q)(k => k("a").map(_ + "|q"))
    assertEquals(run(Delim.push[String, P](q)(dollar(p)(angle)(body))), "<a>|q")
  }

  // ------------------------------------------------ against the references

  test("the primitive agrees with APLAS 2012's macro on every body") {
    for ((name, body) <- bodies) {
      val p = Delim.prompt[String]
      val q = Delim.prompt[String]
      assertEquals(run(dollar(p)(angle)(body(p))), run(dollarMacro(q)(angle)(body(q))), name)
    }
  }

  test("reset0 = pure $: dollar with the unit as ret is push, on every body") {
    for ((name, body) <- bodies) {
      val p = Delim.prompt[String]
      val q = Delim.prompt[String]
      assertEquals(run(dollar(p)(s => pure[Row, String](s))(body(p))), run(Delim.push[String, P](q)(body(q))), name)
    }
  }

  // ------------------------------------------------ what the macro could not type

  test("R0 and R differ: an Int body leaves through ret as a String, and k answers the String") {
    val p = Delim.prompt[String]
    val body: Int ! Row =
      Delim.shift0[String, Int, P](p)(k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
    assertEquals(run(Delim.dollar[Int, String, P](p)(i => pure[Row, String](s"n=$i"))(body)), "n=10|n=20")
  }

  // ------------------------------------------------ nesting: ICFP 2011's example, with rets

  test("two dollars at one prompt: ICFP 2011's \"A cat has Alice.\" with rets") {
    // k1 = λx. {·} $ (" has " ++ x),  k2 = λy. [·] $ ("Alice" ++ y)
    val p = Delim.prompt[String]
    val square: String => String ! Row = s => pure[Row, String](s"[$s]")
    val curly: String => String ! Row = s => pure[Row, String](s"{$s}")
    val inner = shift0(p)(k1 => shift0(p)(k2 => k2(".").flatMap(k1).map("A cat" + _)))
    val prog = dollar(p)(square)(dollar(p)(curly)(inner.map(" has " + _)).map("Alice" + _))
    assertEquals(run(prog), "A cat{ has [Alice.]}")
  }

  // ------------------------------------------------ what is refused, and depth

  test("abort to a dollar SKIPS ret: k is dropped, so v is never applied") {
    val p = Delim.prompt[String]
    val body = Delim.abort[String, String, P](p)("gone").map(_ + "!")
    assertEquals(run(dollar(p)(angle)(body)), "gone")
    assertEquals(run(Delim.push[String, P](p)(body).flatMap(angle)), "<gone>")
  }

  test("a control-capture to a dollar is refused: its bare continuation answers the body's type") {
    for (ctl <- List("control", "control0")) {
      val p = Delim.prompt[String]
      val body =
        if (ctl == "control") Delim.control[String, String, P](p)(k => k("a"))
        else Delim.control0[String, String, P](p)(k => k("a"))
      val e = intercept[UnsupportedOperationException](run(dollar(p)(angle)(body)))
      assert(e.getMessage.contains("dollar"), e.getMessage)
    }
  }

  test("a control-capture to a PLAIN delimiter still works (push keeps its plain mark)") {
    val p = Delim.prompt[String]
    val body = Delim.control[String, String, P](p)(k => k("a").map(_ + "|c")).map(_ + "!")
    assertEquals(run(Delim.push[String, P](p)(body)), "a!|c")
  }

  test("depth: 100 000 dollars nested, each ret run once on the way out, in constant stack") {
    val p = Delim.prompt[Int]
    def nest(n: Int): Int ! Row =
      if (n == 0) pure[Row, Int](0)
      else Delim.dollar[Int, Int, P](p)(x => pure[Row, Int](x + 1))(Free.delay(() => nest(n - 1)))
    assertEquals(run(nest(100000)), 100000)
  }
}
