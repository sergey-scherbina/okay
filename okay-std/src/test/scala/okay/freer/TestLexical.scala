package okay.freer


import okay.std.*
import okay.std.given
import okay.{! as _, pure as _, effect as _, value as _, + as _, % as _, Pure as _, *}
import Bisim.{Answers, Verdict}
import LexicalState.{get, set}
import okay.freer.Row.at

/**
 * specs/lexical-instances.md stage 0: handler instances as prompts,
 * `deep` as a named strategy.
 */
class TestLexical extends munit.FunSuite:

  type W = Writer % String

  def run[A](p: A ! Shift % ? + Pure): A = !.run(Shift.run[A, Pure](p))

  // ------------------------------------------------ two of a kind

  test("TWO State[Int] instances in one program, each operation reaching its own — deep") {
    val prog = LexicalState.deep[Int, (Int, Int), Shift % ? + Pure](0) { a =>
      LexicalState.deep[Int, Int, Shift % ? + Pure](10) { b =>
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

  test("on a ROW the same two instances need Tag: without it Distinct refuses the handler") {
    val e = compileErrors("""
      okay.std.State.handle[Int](0)(okay.std.State.handle[Int](10)(
        okay.std.State.get[Int].at[okay.std.State % Int + okay.std.State % Int]))""")
    assert(e.contains("cannot be told apart in one row"), s"compiled, or refused for another reason: $e")
  }

  // ------------------------------------------------ no accidental handling

  test("NO ACCIDENTAL HANDLING: an inner State[Int] instance does not catch the outer's get") {
    // on a row this program cannot even be written: `get` can only mean
    // the innermost State % Int handler (and two in one row are refused,
    // above). An instance NAMES its installation, and the capture crosses
    // the inner one of the same effect untouched.
    val lex = run(LexicalState.deep[Int, Int, Shift % ? + Pure](0) { outer =>
      LexicalState.deep[Int, Int, Shift % ? + Pure](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
        .map(_._2)
    })
    assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
  }

  // ------------------------------------------------ against the row, by Bisim

  given Answers[W] = Answers.writer[String]

  val counter: Lexical.Inst[State % Int, Shift % ? + W] => Int ! Shift % ? + W = s =>
    for
      a <- s.get
      _ <- effect[Shift % ? + W, Unit](Writer(s"a=$a"))
      _ <- s.set(a + 10)
      b <- s.get
      _ <- effect[Shift % ? + W, Unit](Writer(s"b=$b"))
    yield a + b

  val counterRow: Int ! State % Int + W = for
    a <- State.get[Int].at[State % Int + W]
    _ <- Writer.tell(s"a=$a").at[State % Int + W]
    _ <- State.set(a + 10).at[State % Int + W]
    b <- State.get[Int].at[State % Int + W]
    _ <- Writer.tell(s"b=$b").at[State % Int + W]
  yield a + b

  test("ONE instance, deep, is Bisim-equivalent to State.handle on the Writer row") {
    val row = State.handle[Int](1)(counterRow)
    val deep = Shift.run[(Int, Int), W](LexicalState.deep[Int, Int, Shift % ? + W](1)(counter))
    assertEquals(Bisim.check(deep, row), Verdict.Same(1, 0))
  }

  // ------------------------------------------------ what `tail` cannot run

  enum Flip[+A]:
    case Coin() extends Flip[Boolean]

  test("a NON-tail-resumptive handler from user clauses: every answer of two coin flips, k called twice") {
    val all = new Lexical.Clauses[Flip, (Boolean, Boolean), List[(Boolean, Boolean)], Lexical.Unstacked[Shift % ? + Pure]]:
      def ret(a: (Boolean, Boolean)): List[(Boolean, Boolean)] ! Shift % ? + Pure = okay.freer.pure(List(a))
      def op[X](e: Flip[X], k: X => List[(Boolean, Boolean)] ! Shift % ? + Pure): List[(Boolean, Boolean)] ! Shift % ? + Pure =
        e match
          case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
    val r = run(Lexical.deep(all) { f =>
      for x <- f.perform(Flip.Coin()); y <- f.perform(Flip.Coin()) yield (x, y)
    })
    assertEquals(r, List((true, true), (true, false), (false, true), (false, false)))
  }

  test("depth: 10 000 get/set through one deep instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Shift % ? + Pure], n: Int): Int ! Shift % ? + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(LexicalState.deep[Int, Int, Shift % ? + Pure](0)(s => spin(s, 10_000))), (10_000, 10_000))
  }

/** specs/lexical-instances.md stage 1: the `tail` strategy */
class TestLexicalTail extends munit.FunSuite:
  import LexicalState.{get, set}
  import Layered.{reify, reflect}

  type W = Writer % String
  given Answers[W] = Answers.writer[String]

  def run[A](p: A ! Shift % ? + Pure): A = !.run(Shift.run[A, Pure](p))

  test("tail is Bisim-equivalent to State.handle on the Writer row") {
    val t = new TestLexical
    val tail = Shift.run[(Int, Int), W](LexicalState.tail[Int, Int, Shift % ? + W](1)(t.counter))
    assertEquals(Bisim.check(tail, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("strategies MIX in one program: a deep outer and a tail inner State[Int], the same answer as two deeps") {
    val prog = LexicalState.deep[Int, (Int, Int), Shift % ? + Pure](0) { a =>
      LexicalState.tail[Int, Int, Shift % ? + Pure](10) { b =>
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
  def pick[R](s: Lexical.Inst[State % Int, Shift % ? + Pure])(using Layered.Reflect[List, R]): Int ! Shift % ? + Pure =
    for
      x <- List(1, 2, 3).reflect[R, Pure]
      v <- s.get
      _ <- s.set(v + x)
    yield v

  test("multi-shot INSIDE the installation: tail threads the cell through the branches exactly as deep does") {
    val deep = run(LexicalState.deep[Int, List[Int], Shift % ? + Pure](0)(s => reify[List, Int, Pure](pick(s))))
    val tail = run(LexicalState.tail[Int, List[Int], Shift % ? + Pure](0)(s => reify[List, Int, Pure](pick(s))))
    assertEquals(deep, (6, List(0, 1, 3)))
    assertEquals(tail, deep)
  }

  test("multi-shot ACROSS the installation: tail in a Shift row is installed deep, a state per branch") {
    val deep = run(reify[List, (Int, Int), Pure](LexicalState.deep[Int, Int, Shift % ? + Pure](0)(s => pick(s))))
    assertEquals(deep, List((1, 0), (2, 0), (3, 0)))
    val tail = run(reify[List, (Int, Int), Pure](LexicalState.tail[Int, Int, Shift % ? + Pure](0)(s => pick(s))))
    assertEquals(tail, deep)
  }

  test("ACROSS, leaving by abort: each resumption starts from the state the capture saw, never the other's") {
    // lexical-tail-guard-abort found the shape: a resumption that leaves
    // the body by `abort` to an outer prompt never returns through the
    // installation, so a cell written by the first resumption would be
    // read by the second. Installed deep (cont-core-design), there is no
    // cell to share.
    val p0 = Shift.prompt[Int]
    val twice: Unit ! Shift % ? + Pure =
      Shift.shift[Int, Unit, Pure](p0)(k => k(()).flatMap(a => k(()).map(b => a * 10 + b)))
    def body(s: Lexical.Inst[State % Int, Shift % ? + Pure]): Int ! Shift % ? + Pure =
      for
        _ <- twice
        v <- s.get
        _ <- s.set(v + 1)
        r <- s.get
        _ <- Shift.abort[Int, Unit, Pure](p0)(r)
      yield r
    // deep: each resumption starts from the state the capture saw, 0 -> 1, twice
    assertEquals(run(Shift.push[Int, Pure](p0)(LexicalState.deep[Int, Int, Shift % ? + Pure](0)(body).map(_._2))), 11)
    // tail: the same — a cell would have answered 12 (1 -> 2 on the second resumption)
    assertEquals(run(Shift.push[Int, Pure](p0)(LexicalState.tail[Int, Int, Shift % ? + Pure](0)(body).map(_._2))), 11)
  }

  test("the same program run TWICE is not a multi-shot: the state is made per run") {
    val once = LexicalState.tail[Int, Int, Shift % ? + Pure](5)(s => s.get.flatMap(v => s.set(v + 1)))
    assertEquals(run(once), (6, 6))
    assertEquals(run(once), (6, 6))
  }

  test("depth: 100 000 operations through one tail instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Shift % ? + Pure], n: Int): Int ! Shift % ? + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(run(LexicalState.tail[Int, Int, Shift % ? + Pure](0)(s => spin(s, 100_000))), (100_000, 100_000))
  }

/** specs/lexical-instances.md stage 2: keyed instances (shift-prompt-key: each instance's prompt a key in the row) */
class TestLexicalStacked extends munit.FunSuite:

  type P = Pure

  object StateClauses:
    /** the answer is a program at the clauses' own row */
    type Ans[G[+_]] = Int => (Int, Int) ! G
    def deep[G[+_]] = new Lexical.Clauses[State % Int, Int, Ans[G], Lexical.Unstacked[G]]:
      def ret(a: Int): Ans[G] ! G = pure((s: Int) => pure((s, a)))
      def op[X](e: State[Int, X], k: X => Ans[G] ! G): Ans[G] ! G = e match
        case State.Get() => pure((s: Int) => k(s).flatMap(f => f(s)))
        case State.Update(g) => pure((s: Int) => { val (b, s1) = g(s); k(b).flatMap(f => f(s1)) })
    val tail = new Lexical.TailClauses[State % Int, Int]:
      def op[X](e: State[Int, X], s: Int): (Int, X) = e match
        case State.Get() => (s, s)
        case State.Update(g) => { val (b, s1) = g(s); (s1, b) }

  test("keyed deep and tail instances, one inside the other: each operation reaches its own") {
    import okay.freer.Row.at
    val r = !.run(Lexical.Stacked.tail[State % Int, Int, Int, P](0)(StateClauses.tail) { a =>
      type G = Shift % a.p.type + P
      Lexical.Stacked.deep[State % Int, Int, StateClauses.Ans[G], G](StateClauses.deep[G]) { b =>
        type H = Shift % b.p.type + G
        for
          x <- a.perform(State.Get[Int, Int]()).at[H]
          y <- b.perform(State.Get[Int, Int]())
          _ <- a.perform(State.Update[Int, Int](_ => (x + 5, x + 5))).at[H]
        yield x + y
      }.flatMap(f => f(10)).map(_._2)
    })
    assertEquals(r, (5, 10))
  }

  // the same clause object at two rows (munit's macro wants literals, so twice): at the installation's own row
  // it compiles, at another it is refused — the twin is what makes the refusal a proof rather than a typo
  test("keyed clauses are typed at the row outside their prompt: clauses over another row are refused at the installation") {
    assertEquals(compileErrors("""
      val c = new okay.freer.Lexical.Clauses[okay.std.State % Int, Int, Int, okay.freer.Lexical.Unstacked[Pure]]:
        def ret(a: Int) = okay.freer.pure[Pure, Int](a)
        def op[X](e: okay.std.State[Int, X], k: X => Int ! Pure) = e match
          case okay.std.State.Get() => k(0)
          case _ => okay.freer.pure[Pure, Int](0)
      okay.freer.Lexical.Stacked.deep[okay.std.State % Int, Int, Int, Pure](c) { b =>
        b.perform(okay.std.State.Get[Int, Int]())
      }"""), "")
    val e = compileErrors("""
      val c = new okay.freer.Lexical.Clauses[okay.std.State % Int, Int, Int, okay.freer.Lexical.Unstacked[okay.std.Reader % Int + Pure]]:
        def ret(a: Int) = okay.freer.pure[okay.std.Reader % Int + Pure, Int](a)
        def op[X](e: okay.std.State[Int, X], k: X => Int ! okay.std.Reader % Int + Pure) = e match
          case okay.std.State.Get() => k(0)
          case _ => okay.freer.pure[okay.std.Reader % Int + Pure, Int](0)
      okay.freer.Lexical.Stacked.deep[okay.std.State % Int, Int, Int, Pure](c) { b =>
        b.perform(okay.std.State.Get[Int, Int]())
      }""")
    assert(e.contains("Required:"), s"compiled, or not a type error: $e")
  }

  test("a keyed instance used AFTER its installation returned does not compile where it is run") {
    val e = compileErrors("""
      var leaked: okay.freer.Lexical.Stacked.Tail[okay.std.State % Int, Int, Int, Pure] | Null = null
      okay.freer.!.run(okay.freer.Lexical.Stacked.tail[okay.std.State % Int, Int, Int, Pure](0)(null) { a =>
        leaked = a
        okay.freer.pure[okay.freer.Shift % a.p.type + Pure, Int](1)
      }.flatMap { _ =>
        val l = leaked.nn
        l.perform(okay.std.State.Get[Int, Int]()).map(v => (v, v))
      })""")
    assert(e.nonEmpty, "an instance used after its installation ran")
    assert(e.contains("l.p"), s"the message does not name the escaped key: $e")
  }

/** specs/lexical-instances.md stage 3: the default, and the manual choice kept */
class TestLexicalDefault extends munit.FunSuite:
  import LexicalState.{get, set}
  import Layered.{reify, reflect}

  def run[A](p: A ! Shift % ? + Pure): A = !.run(Shift.run[A, Pure](p))

  def pick[R](s: Lexical.Inst[State % Int, Shift % ? + Pure])(using Layered.Reflect[List, R]): Int ! Shift % ? + Pure =
    for
      x <- List(1, 2, 3).reflect[R, Pure]
      v <- s.get
      _ <- s.set(v + x)
    yield v

  test("LexicalState(s0) is tail: across a multi-shot capture in a Shift row it answers as deep does") {
    val deep = run(reify[List, (Int, Int), Pure](LexicalState.deep[Int, Int, Shift % ? + Pure](0)(s => pick(s))))
    assertEquals(deep, List((1, 0), (2, 0), (3, 0)))
    assertEquals(run(reify[List, (Int, Int), Pure](LexicalState[Int, Int, Shift % ? + Pure](0)(s => pick(s)))), deep)
  }

  enum Flip[+A]:
    case Coin() extends Flip[Boolean]

  test("Lexical.handle picks by clause kind: general clauses run deep (multi-shot works), tail clauses run tail") {
    val all = new Lexical.Clauses[Flip, Boolean, List[Boolean], Lexical.Unstacked[Shift % ? + Pure]]:
      def ret(a: Boolean): List[Boolean] ! Shift % ? + Pure = okay.freer.pure(List(a))
      def op[X](e: Flip[X], k: X => List[Boolean] ! Shift % ? + Pure): List[Boolean] ! Shift % ? + Pure = e match
        case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
    assertEquals(run(Lexical.handle(all)(f => f.perform(Flip.Coin()))), List(true, false))
    val counter = new Lexical.TailClauses[State % Int, Int]:
      def op[X](e: State[Int, X], s: Int): (Int, X) = e match
        case State.Get() => (s, s)
        case State.Update(g) => { val (b, s1) = g(s); (s1, b) }
    assertEquals(run(Lexical.handle(7)(counter)(s => s.get.flatMap(v => s.set(v * 2)))), (14, 14))
  }

/** lexical-tail-allocs: pay only for what the row can do */
class TestLexicalPayAsYouGo extends munit.FunSuite:
  import LexicalState.{get, set}

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

  test("tail on a row WITHOUT Shift: no guard, no machine — the program is (S, A) ! Writer, and Bisim-equal to State.handle") {
    val t = new TestLexical
    val lex: (Int, Int) ! W = LexicalState.tail[Int, Int, W](1)(counterW)
    assertEquals(Bisim.check(lex, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("the default on the pure row: LexicalState(0) runs with !.run alone — no Shift anywhere") {
    val p: (Int, Int) ! Pure = LexicalState[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3)))
    assertEquals(!.run(p), (15, 15))
  }

  test("deep NEEDS Shift in the row: on a Writer-only row it does not compile") {
    val e = compileErrors("okay.std.LexicalState.deep[Int, Int, okay.std.Writer % String](0)(s => okay.std.LexicalState.get(s))")
    assert(e.contains("Shift"), s"compiled, or refused for another reason: $e")
  }

  test("depth, unguarded: 100 000 operations in constant stack") {
    def spin(s: Lexical.Inst[State % Int, Pure], n: Int): Int ! Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(!.run(LexicalState[Int, Int, Pure](0)(s => spin(s, 100_000))), (100_000, 100_000))
  }

/** lexical-tagged-walk: the optional `walk` strategy */
class TestLexicalWalk extends munit.FunSuite:
  import LexicalState.{get, set}
  import Layered.{reify, reflect}
  import okay.freer.Row.{at, up}

  type W = Writer % String
  given Answers[W] = Answers.writer[String]

  /** the one member every walk of State % Int puts in the row */
  type SI = Instances.Of[State % Int]

  /** the top: every walk's operations answered, or Instances.Survived */
  def done[A, G[+_]](p: A ! SI + G): A ! G = Instances.exhausted[State % Int, A, G](p)

  test("walk is Bisim-equal to State.handle on the Writer row, after runLocal") {
    val t = new TestLexical
    val walked: (Int, Int) ! W = done(LexicalState.walk[Int, Int, W](1) { s =>
      for
        a <- s.get
        _ <- Writer.tell(s"a=$a").at[SI + W]
        _ <- s.set(a + 10)
        b <- s.get
        _ <- Writer.tell(s"b=$b").at[SI + W]
      yield a + b
    })
    assertEquals(Bisim.check(walked, State.handle[Int](1)(t.counterRow)), Verdict.Same(1, 0))
  }

  test("walk on the pure row: runLocal and !.run, no Shift anywhere") {
    assertEquals(!.run(done(LexicalState.walk[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3))))), (15, 15))
  }

  test("two walk instances of one effect: the outer's get passes the inner walk and reaches its own") {
    val r = !.run(done(LexicalState.walk[Int, Int, Pure](0) { outer =>
      LexicalState.walk[Int, Int, Pure](10) { inner =>
        outer.get.flatMap(o => inner.get.map(i => o * 100 + i))
      }.map(_._2)
    }))
    assertEquals(r, (0, 10))
  }

  test("an instance used after its walk returned escapes to runLocal, which throws LocalEscaped") {
    var leaked: Lexical.Inst[State % Int, SI + Pure] | Null = null
    val p = LexicalState.walk[Int, Int, Pure](0)(s => { leaked = s; s.get })
      .flatMap(_ => leaked.nn.get.map(v => (v, v)))
    intercept[Instances.Survived](!.run(done(p)))
  }

  test("multi-shot ACROSS the walk (List outside): each branch resumes the walk at its captured state — deep's answer") {
    def pick(s: Lexical.Inst[State % Int, SI + Shift % ? + Pure])(using Layered.Reflect[List, (Int, Int)]): Int ! SI + Shift % ? + Pure =
      for
        x <- List(1, 2, 3).reflect[(Int, Int), SI + Pure]
        v <- s.get
        _ <- s.set(v + x)
      yield v
    val r = !.run(done(Shift.run[List[(Int, Int)], SI + Pure](
      reify[List, (Int, Int), SI + Pure](LexicalState.walk[Int, Int, Shift % ? + Pure](0)(s => pick(s))))))
    assertEquals(r, List((1, 0), (2, 0), (3, 0)))
  }

  test("multi-shot INSIDE the walk with the machine OUTSIDE it: since handle-frames-loops the walk is a frame of that machine — deep's answer") {
    // it ESCAPED, loudly (Instances.Survived), while the walk was a fold the machine could not see into: the
    // operation inside the delimiter went past the walk. Now the machine runs the walk as a frame below the
    // delimiter, and the branches thread the walk's state as with the machine inside
    def pick(s: Lexical.Inst[State % Int, SI + Shift % ? + Pure])(using Layered.Reflect[List, Int]): Int ! SI + Shift % ? + Pure =
      for
        x <- List(1, 2, 3).reflect[Int, SI + Pure]
        v <- s.get
        _ <- s.set(v + x)
      yield v
    val r = !.run(done(Shift.run[(Int, List[Int]), SI + Pure](
      LexicalState.walk[Int, List[Int], Shift % ? + Pure](0)(s => reify[List, Int, SI + Pure](pick(s))))))
    assertEquals(r, (6, List(0, 1, 3)))
  }

  test("multi-shot INSIDE the walk with the machine INSIDE it: the walk threads the state through the branches — deep's answer") {
    def pick(s: Lexical.Inst[State % Int, SI + Pure])(using Layered.Reflect[List, Int]): Int ! Shift % ? + SI + Pure =
      for
        x <- List(1, 2, 3).reflect[Int, SI + Pure]
        v <- s.get.up[Shift % ? + SI + Pure]
        _ <- s.set(v + x).up[Shift % ? + SI + Pure]
      yield v
    val r = !.run(done(LexicalState.walk[Int, List[Int], Pure](0)(s =>
      Shift.run[List[Int], SI + Pure](reify[List, Int, SI + Pure](pick(s))))))
    assertEquals(r, (6, List(0, 1, 3)))
  }

  test("depth: 100 000 operations through one walk, in constant stack") {
    def spin(s: Lexical.Inst[State % Int, SI + Pure], n: Int): Int ! SI + Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(!.run(done(LexicalState.walk[Int, Int, Pure](0)(s => spin(s, 100_000)))), (100_000, 100_000))
  }
