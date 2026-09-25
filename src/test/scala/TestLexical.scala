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
    val prog = Lexical.State.deep[Int, (Int, Int), Delim + Pure](0) { a =>
      Lexical.State.deep[Int, Int, Delim + Pure](10) { b =>
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
    val prog = Lexical.State.shallow[Int, (Int, Int), Delim + Pure](0) { a =>
      Lexical.State.shallow[Int, Int, Delim + Pure](10) { b =>
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
    val lex = run(Lexical.State.deep[Int, Int, Delim + Pure](0) { outer =>
      Lexical.State.deep[Int, Int, Delim + Pure](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
        .map(_._2)
    })
    assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
  }

  // ------------------------------------------------ against the row, by Bisim

  given Answers[W] = Answers.writer[String]

  val counter: Lexical.Inst[State % Int, Delim + W] => Int ! Delim + W = s =>
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
    val deep = Delim.run[(Int, Int), W](Lexical.State.deep[Int, Int, Delim + W](1)(counter))
    val shallow = Delim.run[(Int, Int), W](Lexical.State.shallow[Int, Int, Delim + W](1)(counter))
    assertEquals(Bisim.check(deep, row), Verdict.Same(1, 0))
    assertEquals(Bisim.check(shallow, row), Verdict.Same(1, 0))
  }

  // ------------------------------------------------ what `tail` cannot run

  enum Flip[+A]:
    case Coin() extends Flip[Boolean]

  test("a NON-tail-resumptive handler from user clauses: every answer of two coin flips, k called twice") {
    val all = new Lexical.Clauses[Flip, (Boolean, Boolean), List[(Boolean, Boolean)], Delim + Pure]:
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
    def spin(s: Lexical.Inst[State % Int, Delim + Pure], n: Int): Int ! Delim + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(Lexical.State.deep[Int, Int, Delim + Pure](0)(s => spin(s, 10_000))), (10_000, 10_000))
  }

/** specs/lexical-instances.md stage 1: the `tail` strategy */
class TestLexicalTail extends munit.FunSuite:
  import Lexical.State.{get, set}
  import Layered.{reify, reflect}

  type W = Writer % String
  given Answers[W] = Answers.writer[String]

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

  test("tail is Bisim-equivalent to State.handle on the Writer row") {
    val t = new TestLexical
    val tail = Delim.run[(Int, Int), W](Lexical.State.tail[Int, Int, Delim + W](1)(t.counter))
    assertEquals(Bisim.check(tail, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("strategies MIX in one program: a deep outer and a tail inner State[Int], the same answer as two deeps") {
    val prog = Lexical.State.deep[Int, (Int, Int), Delim + Pure](0) { a =>
      Lexical.State.tail[Int, Int, Delim + Pure](10) { b =>
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

  /** the body both multi-shot tests run: pick from a List layer, read and bump the state */
  def pick[R](s: Lexical.Inst[State % Int, Delim + Pure])(using Layered.Reflect[List, R]): Int ! Delim + Pure =
    for
      x <- List(1, 2, 3).reflect[R, Pure]
      v <- s.get
      _ <- s.set(v + x)
    yield v

  test("multi-shot INSIDE the installation: tail threads the cell through the branches exactly as deep does") {
    val deep = run(Lexical.State.deep[Int, List[Int], Delim + Pure](0)(s => reify[List, Int, Pure](pick(s))))
    val tail = run(Lexical.State.tail[Int, List[Int], Delim + Pure](0)(s => reify[List, Int, Pure](pick(s))))
    assertEquals(deep, (6, List(0, 1, 3)))
    assertEquals(tail, deep)
  }

  test("multi-shot ACROSS the installation: deep keeps a state per branch, tail refuses loudly instead of sharing its cell") {
    val deep = run(reify[List, (Int, Int), Pure](Lexical.State.deep[Int, Int, Delim + Pure](0)(s => pick(s))))
    assertEquals(deep, List((1, 0), (2, 0), (3, 0)))
    val e = intercept[Lexical.MultiShotAcrossTail](
      run(reify[List, (Int, Int), Pure](Lexical.State.tail[Int, Int, Delim + Pure](0)(s => pick(s)))))
    assert(e.getMessage.contains("deep"), e.getMessage)
  }

  test("the same program run TWICE is not a multi-shot: the cell and the guard are made per run") {
    val once = Lexical.State.tail[Int, Int, Delim + Pure](5)(s => s.get.flatMap(v => s.set(v + 1)))
    assertEquals(run(once), (6, 6))
    assertEquals(run(once), (6, 6))
  }

  test("depth: 100 000 operations through one tail instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Delim + Pure], n: Int): Int ! Delim + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(Lexical.State.tail[Int, Int, Delim + Pure](0)(s => spin(s, 100_000))), (100_000, 100_000))
  }

/** specs/lexical-instances.md stage 2: stacked instances */
class TestLexicalStacked extends munit.FunSuite:
  import okay.Delim.Stacked.delimited
  import okay.Prog.{flatMap, map}

  type P = okay.Pure

  object StateClauses:
    type Ans = Int => (Int, Int) ! Delim + P
    val deep = new Lexical.Clauses[State % Int, Int, Ans, Delim + P]:
      def ret(a: Int): Ans ! Delim + P = okay.pure((s: Int) => okay.pure((s, a)))
      def op[X](e: State[Int, X], k: X => Ans ! Delim + P): Ans ! Delim + P = e match
        case State.Get() => okay.pure((s: Int) => k(s).flatMap(f => f(s)))
        case State.Set(s1) => okay.pure((_: Int) => k(s1).flatMap(f => f(s1)))
    val tail = new Lexical.TailClauses[State % Int, Int]:
      def op[X](e: State[Int, X], s: Int): (Int, X) = e match
        case State.Get() => (s, s)
        case State.Set(s1) => (s1, s1)

  test("stacked deep and tail instances, one inside the other: each operation reaches its own") {
    val r = !.run(delimited[(Int, Int), P] { root =>
      import root.given
      Lexical.Stacked.tail[State % Int, Int, Int, P](0)(StateClauses.tail) { a =>
        import a.given
        Lexical.Stacked.deep[State % Int, Int, StateClauses.Ans, P](StateClauses.deep) { b =>
          import b.given
          for
            x <- a.perform(State.Get[Int, Int]())
            y <- b.perform(State.Get[Int, Int]())
            _ <- a.perform(State.Set[Int, Int](x + 5))
          yield x + y
        }.flatMap(f => okay.Prog.diag(f(10))).map(_._2)
      }
    })
    assertEquals(r, (5, 10))
  }

  test("a stacked instance used AFTER its installation returned does not compile") {
    val e = compileErrors("""
      okay.Delim.Stacked.delimited[(Int, Int), okay.Pure] { root =>
        import root.given
        var leaked: okay.Lexical.Stacked.Tail[okay.State % Int, Int, Int, okay.Pure, ?] | Null = null
        okay.Lexical.Stacked.tail[okay.State % Int, Int, Int, okay.Pure](0)(null) { a =>
          import a.given
          leaked = a
          okay.Prog.pure(1)
        }.flatMap(_ => leaked.nn.perform(okay.State.Get[Int, Int]()).map(v => (v, v)))
      }""")
    assert(e.contains("not on the prompt stack"), s"compiled, or not our message: $e")
  }

/** specs/lexical-instances.md stage 3: the default, and the manual choice kept */
class TestLexicalDefault extends munit.FunSuite:
  import Lexical.State.{get, set}
  import Layered.{reify, reflect}

  def run[A](p: A ! Delim + Pure): A = !.run(Delim.run[A, Pure](p))

  def pick[R](s: Lexical.Inst[State % Int, Delim + Pure])(using Layered.Reflect[List, R]): Int ! Delim + Pure =
    for
      x <- List(1, 2, 3).reflect[R, Pure]
      v <- s.get
      _ <- s.set(v + x)
    yield v

  test("Lexical.State(s0) is tail: the guard trips across a multi-shot capture, and naming `deep` is the way out") {
    val e = intercept[Lexical.MultiShotAcrossTail](run(reify[List, (Int, Int), Pure](Lexical.State[Int, Int, Delim + Pure](0)(s => pick(s)))))
    assert(e.getMessage.contains("deep"), "the refusal names the way out")
    assertEquals(run(reify[List, (Int, Int), Pure](Lexical.State.deep[Int, Int, Delim + Pure](0)(s => pick(s)))),
      List((1, 0), (2, 0), (3, 0)))
  }

  enum Flip[+A]:
    case Coin() extends Flip[Boolean]

  test("Lexical.handle picks by clause kind: general clauses run deep (multi-shot works), tail clauses run tail") {
    val all = new Lexical.Clauses[Flip, Boolean, List[Boolean], Delim + Pure]:
      def ret(a: Boolean): List[Boolean] ! Delim + Pure = okay.pure(List(a))
      def op[X](e: Flip[X], k: X => List[Boolean] ! Delim + Pure): List[Boolean] ! Delim + Pure = e match
        case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
    assertEquals(run(Lexical.handle(all)(f => f.perform(Flip.Coin()))), List(true, false))
    val counter = new Lexical.TailClauses[State % Int, Int]:
      def op[X](e: State[Int, X], s: Int): (Int, X) = e match
        case State.Get() => (s, s)
        case State.Set(s1) => (s1, s1)
    assertEquals(run(Lexical.handle(7)(counter)(s => s.get.flatMap(v => s.set(v * 2)))), (14, 14))
  }

/** lexical-tail-allocs: pay only for what the row can do */
class TestLexicalPayAsYouGo extends munit.FunSuite:
  import Lexical.State.{get, set}

  type W = Writer % String
  given Answers[W] = Answers.writer[String]

  val counterW: Lexical.Inst[State % Int, W] => Int ! W = s =>
    for
      a <- s.get
      _ <- Writer.tell(s"a=$a")
      _ <- s.set(a + 10)
      b <- s.get
      _ <- Writer.tell(s"b=$b")
    yield a + b

  test("tail on a row WITHOUT Delim: no guard, no machine — the program is (S, A) ! Writer, and Bisim-equal to State.handle") {
    val t = new TestLexical
    val lex: (Int, Int) ! W = Lexical.State.tail[Int, Int, W](1)(counterW)
    assertEquals(Bisim.check(lex, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("the default on the pure row: Lexical.State(0) runs with !.run alone — no Delim anywhere") {
    val p: (Int, Int) ! Pure = Lexical.State[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3)))
    assertEquals(!.run(p), (15, 15))
  }

  test("deep NEEDS Delim in the row: on a Writer-only row it does not compile") {
    val e = compileErrors("okay.Lexical.State.deep[Int, Int, okay.Writer % String](0)(s => okay.Lexical.State.get(s))")
    assert(e.contains("Delim"), s"compiled, or refused for another reason: $e")
  }

  test("depth, unguarded: 100 000 operations in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Pure], n: Int): Int ! Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(!.run(Lexical.State[Int, Int, Pure](0)(s => spin(s, 100_000))), (100_000, 100_000))
  }

/** lexical-tagged-walk: the optional `walk` strategy */
class TestLexicalWalk extends munit.FunSuite:
  import Lexical.State.{get, set}
  import Layered.{reify, reflect}
  import okay.Row.{at, up}

  type W = Writer % String
  given Answers[W] = Answers.writer[String]

  test("walk is Bisim-equal to State.handle on the Writer row, after runLocal") {
    val t = new TestLexical
    val walked: (Int, Int) ! W = Lexical.runLocal(Lexical.State.walk[Int, Int, W](1) { s =>
      for
        a <- s.get
        _ <- Writer.tell(s"a=$a").at[Local + W]
        _ <- s.set(a + 10)
        b <- s.get
        _ <- Writer.tell(s"b=$b").at[Local + W]
      yield a + b
    })
    assertEquals(Bisim.check(walked, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("walk on the pure row: runLocal and !.run, no Delim anywhere") {
    assertEquals(!.run(Lexical.runLocal(Lexical.State.walk[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3))))), (15, 15))
  }

  test("two walk instances of one effect: the outer's get passes the inner walk and reaches its own") {
    val r = !.run(Lexical.runLocal(Lexical.State.walk[Int, Int, Pure](0) { outer =>
      Lexical.State.walk[Int, Int, Local + Pure](10) { inner =>
        outer.get.flatMap(o => inner.get.map(i => o * 100 + i))
      }.map(_._2)
    }))
    assertEquals(r, (0, 10))
  }

  test("an instance used after its walk returned escapes to runLocal, which throws LocalEscaped") {
    var leaked: Lexical.Inst[State % Int, Local + Pure] | Null = null
    val p = Lexical.State.walk[Int, Int, Pure](0)(s => { leaked = s; s.get })
      .flatMap(_ => leaked.nn.get.map(v => (v, v)))
    intercept[Lexical.LocalEscaped](!.run(Lexical.runLocal(p)))
  }

  test("multi-shot ACROSS the walk (List outside): each branch resumes the walk at its captured state — deep's answer") {
    def pick(s: Lexical.Inst[State % Int, Local + Delim + Pure])(using Layered.Reflect[List, (Int, Int)]): Int ! Local + Delim + Pure =
      for
        x <- List(1, 2, 3).reflect[(Int, Int), Local + Pure]
        v <- s.get
        _ <- s.set(v + x)
      yield v
    val r = !.run(Lexical.runLocal(Delim.run[List[(Int, Int)], Local + Pure](
      reify[List, (Int, Int), Local + Pure](Lexical.State.walk[Int, Int, Delim + Pure](0)(s => pick(s))))))
    assertEquals(r, List((1, 0), (2, 0), (3, 0)))
  }

  test("multi-shot INSIDE the walk with the machine OUTSIDE it: the operation inside the delimiter escapes, loudly") {
    def pick(s: Lexical.Inst[State % Int, Local + Delim + Pure])(using Layered.Reflect[List, Int]): Int ! Local + Delim + Pure =
      for
        x <- List(1, 2, 3).reflect[Int, Local + Pure]
        v <- s.get
        _ <- s.set(v + x)
      yield v
    intercept[Lexical.LocalEscaped](!.run(Lexical.runLocal(Delim.run[(Int, List[Int]), Local + Pure](
      Lexical.State.walk[Int, List[Int], Delim + Pure](0)(s => reify[List, Int, Local + Pure](pick(s)))))))
  }

  test("multi-shot INSIDE the walk with the machine INSIDE it: the walk threads the state through the branches — deep's answer") {
    def pick(s: Lexical.Inst[State % Int, Local + Pure])(using Layered.Reflect[List, Int]): Int ! Delim + Local + Pure =
      for
        x <- List(1, 2, 3).reflect[Int, Local + Pure]
        v <- s.get.up[Delim + Local + Pure]
        _ <- s.set(v + x).up[Delim + Local + Pure]
      yield v
    val r = !.run(Lexical.runLocal(Lexical.State.walk[Int, List[Int], Pure](0)(s =>
      Delim.run[List[Int], Local + Pure](reify[List, Int, Local + Pure](pick(s))))))
    assertEquals(r, (6, List(0, 1, 3)))
  }

  test("depth: 100 000 operations through one walk, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Local + Pure], n: Int): Int ! Local + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(!.run(Lexical.runLocal(Lexical.State.walk[Int, Int, Pure](0)(s => spin(s, 100_000)))), (100_000, 100_000))
  }
