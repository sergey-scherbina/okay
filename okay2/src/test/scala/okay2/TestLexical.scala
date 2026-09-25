package okay2

/** a user effect for the multi-shot clause test: a coin */
sealed trait Flip extends Row { type Op[+A] = Flip.Op[A] }
object Flip {
  sealed trait Op[+A]
  final case class Coin() extends Op[Boolean]
}

/**
 * okay2-lexical: the twin of the Scala 3 core's TestLexical and
 * TestLexicalTail (specs/lexical-instances.md), every expected value the
 * same. Where the Scala 3 suite reaches multi-shot through `Layered`
 * (no twin yet), this one uses a raw `Delim.shift` to an outer prompt,
 * which is what `reify[List]` is underneath.
 */
class TestLexical extends munit.FunSuite {

  type P = Pure
  type DP = Delim + P

  def run[A](p: A ! DP): A = !.run(Delim.run[A, P](p))

  // ------------------------------------------------ two of a kind

  test("TWO State[Int] instances in one program, each operation reaching its own — deep") {
    val prog = Lexical.State.deep[Int, (Int, Int), P](0) { a =>
      Lexical.State.deep[Int, Int, P](10) { b =>
        for {
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        } yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    // a: 0 -> 10; b: 10 -> 20; body answers 10; a is read after b's handler returned
    assertEquals(run(prog), (10, (10, 10)))
  }

  test("the same two instances through `shallow`: the same answer") {
    val prog = Lexical.State.shallow[Int, (Int, Int), P](0) { a =>
      Lexical.State.shallow[Int, Int, P](10) { b =>
        for {
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        } yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    assertEquals(run(prog), (10, (10, 10)))
  }

  // ------------------------------------------------ no accidental handling

  test("NO ACCIDENTAL HANDLING: an inner State[Int] instance does not catch the outer's get") {
    val lex = run(Lexical.State.deep[Int, Int, P](0) { outer =>
      Lexical.State.deep[Int, Int, P](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
        .map(_._2)
    })
    assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
  }

  // ------------------------------------------------ against the row, by value

  type W = Writer[String]

  val counter: Lexical.State.Inst[Int, Delim + W] => Int ! (Delim + W) = s =>
    for {
      a <- s.get
      _ <- Writer.tell(s"a=$a").plus[Delim]
      _ <- s.set(a + 10)
      b <- s.get
      _ <- Writer.tell(s"b=$b").plus[Delim]
    } yield a + b

  val counterRow: Int ! (State[Int] + W) = for {
    a <- State.get[Int].plus[W]
    _ <- Writer.tell(s"a=$a").plus[State[Int]]
    _ <- State.set(a + 10).plus[W]
    b <- State.get[Int].plus[W]
    _ <- Writer.tell(s"b=$b").plus[State[Int]]
  } yield a + b

  def told(p: (Int, Int) ! W): (Seq[String], (Int, Int)) = !.run(Writer.run[String, (Int, Int), P](p))

  test("ONE instance, deep, shallow and tail, answers as State.handleAt does on the Writer row") {
    val row = told(State.handleAt[Int, Int, W](1)(counterRow))
    assertEquals(row, (Seq("a=1", "b=11"), (11, 12)))
    assertEquals(told(Delim.run[(Int, Int), W](Lexical.State.deep[Int, Int, W](1)(counter))), row)
    assertEquals(told(Delim.run[(Int, Int), W](Lexical.State.shallow[Int, Int, W](1)(counter))), row)
    assertEquals(told(Delim.run[(Int, Int), W](Lexical.State.tail[Int, Int, W](1)(counter))), row)
  }

  // ------------------------------------------------ what `tail` cannot run

  test("a NON-tail-resumptive handler from user clauses: every answer of two coin flips, k called twice") {
    type Out = List[(Boolean, Boolean)]
    val all = new Lexical.Clauses[Flip, (Boolean, Boolean), Out, DP] {
      def ret(a: (Boolean, Boolean)): Out ! DP = pure[DP, Out](List(a))
      def op[X](e: Flip.Op[X], k: X => Out ! DP): Out ! DP = e match {
        // Scala 2 does not refine X to Boolean from `Coin extends Op[Boolean]`: the twin's one cast in a user clause
        case Flip.Coin() => k(true.asInstanceOf[X]).flatMap(xs => k(false.asInstanceOf[X]).map(xs ++ _))
      }
    }
    val r = run(Lexical.deep[Flip, (Boolean, Boolean), Out, P](all) { f =>
      for { x <- f.perform(Flip.Coin()); y <- f.perform(Flip.Coin()) } yield (x, y)
    })
    assertEquals(r, List((true, true), (true, false), (false, true), (false, false)))
  }

  test("depth: 10 000 get/set through one deep instance, in constant stack") {
    def spin(s: Lexical.State.Inst[Int, DP], n: Int): Int ! DP =
      if (n == 0) s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(Lexical.State.deep[Int, Int, P](0)(s => spin(s, 10000))), (10000, 10000))
  }

  // ------------------------------------------------ the tail strategy

  test("strategies MIX in one program: a deep outer and a tail inner State[Int], the same answer as two deeps") {
    val prog = Lexical.State.deep[Int, (Int, Int), P](0) { a =>
      Lexical.State.tail[Int, Int, P](10) { b =>
        for {
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        } yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    assertEquals(run(prog), (10, (10, 10)))
  }

  /** the body both multi-shot tests run: pick 1, 2, 3 from an outer
   * prompt (what `reify[List]` does underneath), read and bump the state */
  def pick(p0: Prompt[List[Int]])(s: Lexical.State.Inst[Int, DP]): List[Int] ! DP =
    for {
      x <- Delim.shift[List[Int], Int, P](p0)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a ++ b ++ c))))
      v <- s.get
      _ <- s.set(v + x)
    } yield List(v)

  test("multi-shot INSIDE the installation: tail threads the cell through the branches exactly as deep does") {
    val p0 = Delim.prompt[List[Int]]
    val deep = run(Lexical.State.deep[Int, List[Int], P](0)(s => Delim.push[List[Int], P](p0)(pick(p0)(s))))
    val tail = run(Lexical.State.tail[Int, List[Int], P](0)(s => Delim.push[List[Int], P](p0)(pick(p0)(s))))
    assertEquals(deep, (6, List(0, 1, 3)))
    assertEquals(tail, deep)
  }

  /** the same pick, with the prompt OUTSIDE the installation: the answer is one (state, value) per branch */
  def pickAcross(p0: Prompt[List[(Int, Int)]])(s: Lexical.State.Inst[Int, DP]): Int ! DP =
    for {
      x <- Delim.shift[List[(Int, Int)], Int, P](p0)(k => k(1).flatMap(a => k(2).flatMap(b => k(3).map(c => a ++ b ++ c))))
      v <- s.get
      _ <- s.set(v + x)
    } yield v

  test("multi-shot ACROSS the installation: deep keeps a state per branch, tail refuses loudly instead of sharing its cell") {
    val p0 = Delim.prompt[List[(Int, Int)]]
    val deep = run(Delim.push[List[(Int, Int)], P](p0)(Lexical.State.deep[Int, Int, P](0)(pickAcross(p0)).map(List(_))))
    assertEquals(deep, List((1, 0), (2, 0), (3, 0)))
    val e = intercept[Lexical.MultiShotAcrossTail](
      run(Delim.push[List[(Int, Int)], P](p0)(Lexical.State.tail[Int, Int, P](0)(pickAcross(p0)).map(List(_)))))
    assert(e.getMessage.contains("deep"), e.getMessage)
  }

  test("ACROSS, leaving by abort: a second resumption that never RETURNS through the guard still trips it") {
    // lexical-tail-guard-abort: the guard counts RUNS of a captured
    // context, not returns through `ret`
    val p0 = Delim.prompt[Int]
    val twice: Unit ! DP = Delim.shift[Int, Unit, P](p0)(k => k(()).flatMap(a => k(()).map(b => a * 10 + b)))
    def body(s: Lexical.State.Inst[Int, DP]): Int ! DP =
      for {
        _ <- twice
        v <- s.get
        _ <- s.set(v + 1)
        r <- s.get
        _ <- Delim.abort[Int, Unit, P](p0)(r)
      } yield r
    // deep: each resumption starts from the state the capture saw, 0 -> 1, twice
    assertEquals(run(Delim.push[Int, P](p0)(Lexical.State.deep[Int, Int, P](0)(body).map(_._2))), 11)
    // tail: the second resumption would read the first's cell (1 -> 2, answer 12) — refused instead
    val e = intercept[Lexical.MultiShotAcrossTail](
      run(Delim.push[Int, P](p0)(Lexical.State.tail[Int, Int, P](0)(body).map(_._2))))
    assert(e.getMessage.contains("deep"), e.getMessage)
  }

  test("the same program run TWICE is not a multi-shot: the cell and the guard are made per run") {
    val once = Lexical.State.tail[Int, Int, P](5)(s => s.get.flatMap(v => s.set(v + 1)))
    assertEquals(run(once), (6, 6))
    assertEquals(run(once), (6, 6))
  }

  test("depth: 100 000 operations through one tail instance, in constant stack") {
    def spin(s: Lexical.State.Inst[Int, DP], n: Int): Int ! DP =
      if (n == 0) s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(Lexical.State.tail[Int, Int, P](0)(s => spin(s, 100000))), (100000, 100000))
  }

  // ------------------------------------------------ pay as you go: no Delim, no guard, no machine

  test("tailPure on a row WITHOUT Delim: the program is (S, A) ! Writer, and answers as the row handler does") {
    val prog: (Int, Int) ! W = Lexical.State.tailPure[Int, Int, W](1) { s =>
      for {
        a <- s.get
        _ <- Writer.tell(s"a=$a")
        _ <- s.set(a + 10)
        b <- s.get
        _ <- Writer.tell(s"b=$b")
      } yield a + b
    }
    assertEquals(told(prog), (Seq("a=1", "b=11"), (11, 12)))
  }

  test("tailPure on the pure row: !.run alone, no Delim anywhere; and 100 000 operations in constant stack") {
    assertEquals(!.run(Lexical.State.tailPure[Int, Int, P](0)(s => s.get.flatMap(v => s.set(v + 1)))), (1, 1))
    def spin(s: Lexical.State.Inst[Int, P], n: Int): Int ! P =
      if (n == 0) s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(!.run(Lexical.State.tailPure[Int, Int, P](0)(s => spin(s, 100000))), (100000, 100000))
  }
}
