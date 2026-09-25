package okay

import Bisim.{Answers, Verdict}
import Lexical.State.{get, set}
import okay.Row.at

/**
 * specs/lexical-instances.md stage 0: handler instances as prompts,
 * `deep` and `shallow` as named strategies.
 */
class TestLexical extends munit.FunSuite:

  type W = Writer % String

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

  // ------------------------------------------------ two of a kind

  test("TWO State[Int] instances in one program, each operation reaching its own — deep") {
    val prog = Lexical.State.deep[Int, (Int, Int), Pure](0) { a =>
      Lexical.State.deep[Int, Int, Pure](10) { b =>
        for
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    // a: 0 -> 10; b: 10 -> 20; body answers 10; a is read after b's handler returned
    assertEquals(run(prog), (10, (10, 10)))
  }

  test("the same two instances through `shallow`: the same answer") {
    val prog = Lexical.State.shallow[Int, (Int, Int), Pure](0) { a =>
      Lexical.State.shallow[Int, Int, Pure](10) { b =>
        for
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    assertEquals(run(prog), (10, (10, 10)))
  }

  test("on a ROW the same two instances need Tag: without it Distinct refuses the handler") {
    val e = compileErrors("""
      okay.State.handle[Int](0)(okay.State.handle[Int](10)(
        okay.State.get[Int].at[okay.State % Int + okay.State % Int]))""")
    assert(e.contains("cannot be told apart in one row"), s"compiled, or refused for another reason: $e")
  }

  // ------------------------------------------------ no accidental handling

  test("NO ACCIDENTAL HANDLING: an inner State[Int] instance does not catch the outer's get") {
    // on a row this program cannot even be written: `get` can only mean
    // the innermost State % Int handler (and two in one row are refused,
    // above). An instance NAMES its installation, and the capture crosses
    // the inner one of the same effect untouched.
    val lex = run(Lexical.State.deep[Int, Int, Pure](0) { outer =>
      Lexical.State.deep[Int, Int, Pure](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
        .map(_._2)
    })
    assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
  }

  // ------------------------------------------------ against the row, by Bisim

  given Answers[W] = Answers.writer[String]

  val counter: Lexical.Inst[State % Int, Lexical.State.Ans[Int, Int, W], W] => Int ! Delim + W = s =>
    for
      a <- s.get
      _ <- effect[Delim + W, Unit](Writer(s"a=$a"))
      _ <- s.set(a + 10)
      b <- s.get
      _ <- effect[Delim + W, Unit](Writer(s"b=$b"))
    yield a + b

  val counterRow: Int ! State % Int + W = for
    a <- State.get[Int].at[State % Int + W]
    _ <- Writer.tell(s"a=$a").at[State % Int + W]
    _ <- State.set(a + 10).at[State % Int + W]
    b <- State.get[Int].at[State % Int + W]
    _ <- Writer.tell(s"b=$b").at[State % Int + W]
  yield a + b

  test("ONE instance, deep and shallow, is Bisim-equivalent to State.handle on the Writer row") {
    val row = State.handle[Int](1)(counterRow)
    val deep = Delim.run[(Int, Int), W](Lexical.State.deep[Int, Int, W](1)(counter))
    val shallow = Delim.run[(Int, Int), W](Lexical.State.shallow[Int, Int, W](1)(counter))
    assertEquals(Bisim.check(deep, row), Verdict.Same(1, 0))
    assertEquals(Bisim.check(shallow, row), Verdict.Same(1, 0))
  }

  // ------------------------------------------------ what `tail` cannot run

  enum Flip[+A]:
    case Coin() extends Flip[Boolean]

  test("a NON-tail-resumptive handler from user clauses: every answer of two coin flips, k called twice") {
    val all = new Lexical.Clauses[Flip, (Boolean, Boolean), List[(Boolean, Boolean)], Pure]:
      def ret(a: (Boolean, Boolean)): List[(Boolean, Boolean)] ! Delim + Pure = okay.pure(List(a))
      def op[X](e: Flip[X], k: X => List[(Boolean, Boolean)] ! Delim + Pure): List[(Boolean, Boolean)] ! Delim + Pure =
        e match
          case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
    val r = run(Lexical.deep(all) { f =>
      for x <- f.perform(Flip.Coin()); y <- f.perform(Flip.Coin()) yield (x, y)
    })
    assertEquals(r, List((true, true), (true, false), (false, true), (false, false)))
  }

  test("depth: 10 000 get/set through one deep instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Lexical.State.Ans[Int, Int, Pure], Pure], n: Int): Int ! Delim + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(Lexical.State.deep[Int, Int, Pure](0)(s => spin(s, 10_000))), (10_000, 10_000))
  }
