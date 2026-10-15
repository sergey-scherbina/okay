package okay.std.tests

import okay.*
import okay.std.*
import LexicalState.{get, set}

/**
 * specs/lexical-instances.md on the kernel: handler instances as places by key (`Lexical`, `In.At`), the strategy
 * the handler form's — `deep` the kernel's `Deep`, `tail` its `Stateful`.
 */
class TestLexical extends munit.FunSuite:

  // ------------------------------------------------ two of a kind

  test("TWO State[Int] instances in one program, each operation reaching its own — deep") {
    val prog = LexicalState.deep[Int, (Int, Int), Pure](0) { a =>
      LexicalState.deep[Int, Int, Pure](10) { b =>
        for
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    // a: 0 -> 10; b: 10 -> 20; body answers 10; a is read after b's handler returned
    assertEquals(prog.run, (10, (10, 10)))
  }

  // ------------------------------------------------ no accidental handling

  test("NO ACCIDENTAL HANDLING: an inner State[Int] instance does not catch the outer's get") {
    // an instance NAMES its installation (a key), and the operation passes the inner handler of the same effect
    val lex = LexicalState.deep[Int, Int, Pure](0) { outer =>
      LexicalState.deep[Int, Int, Pure](10) { inner => outer.get.flatMap(o => inner.get.map(i => o * 100 + i)) }
        .map(_._2)
    }.run
    assertEquals(lex, (0, 10), "outer answered 0, inner answered 10")
  }

  test("an instance among the program's other effects: the Writer's handler outside, the instance's ops past it") {
    type W = Writer % String
    val counter: Lexical.Inst[State % Int] => Int ! W = s =>
      for
        a <- s.get
        _ <- Writer.tell(s"a=$a")
        _ <- s.set(a + 10)
        b <- s.get
        _ <- Writer.tell(s"b=$b")
      yield a + b
    assertEquals(LexicalState.deep[Int, (Seq[String], Int), Pure](1)(s => Writer.run(counter(s))).run,
      (11, (Seq("a=1", "b=11"), 12)))
    assertEquals(Writer.run(LexicalState[Int, Int, W](1)(counter)).run, (Seq("a=1", "b=11"), (11, 12)))
  }

  // ------------------------------------------------ what `tail` cannot run

  enum Flip[X]:
    case Coin() extends Flip[Boolean]

  /** every answer: the rest resumed once per side */
  val all: Handler[Flip, List] = Handler.deep[Flip, List]([a] => (x: a) => List(x)) {
    [a, X] => (e: Flip[X], k: X => List[a] ! Pure) => e match
      case Flip.Coin() => k(true).flatMap(xs => k(false).map(xs ++ _))
  }

  test("a NON-tail-resumptive handler, lexically: every answer of two coin flips, k called twice") {
    val r = Lexical.handle(all) { f =>
      for x <- f.perform(Flip.Coin()); y <- f.perform(Flip.Coin()) yield (x, y)
    }.run
    assertEquals(r, List((true, true), (true, false), (false, true), (false, false)))
  }

  test("depth: 10 000 get/set through one deep instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int], n: Int): Int ! Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(LexicalState.deep[Int, Int, Pure](0)(s => spin(s, 10_000)).run, (10_000, 10_000))
  }

/** specs/lexical-instances.md stage 1: the `tail` strategy (the kernel's `Stateful`) */
class TestLexicalTail extends munit.FunSuite:

  test("strategies MIX in one program: a deep outer and a tail inner State[Int], the same answer as two deeps") {
    val prog = LexicalState.deep[Int, (Int, Int), Pure](0) { a =>
      LexicalState.tail[Int, Int, Pure](10) { b =>
        for
          x <- a.get
          y <- b.get
          _ <- a.set(x + y)
          _ <- b.set(y * 2)
        yield x + y
      }.map(_._2).flatMap(r => a.get.map(sa => (r, sa)))
    }
    assertEquals(prog.run, (10, (10, 10)))
  }

  /** the body both multi-shot tests run: pick one of three, read and bump the state */
  def pick(s: Lexical.Inst[State % Int]): Int ! Choose =
    for
      x <- effect(Choose(Seq(1, 2, 3)))
      v <- s.get.at[Choose]
      _ <- s.set(v + x).at[Choose]
    yield v

  test("multi-shot INSIDE the installation: tail threads the state through the branches exactly as deep does") {
    val deep = LexicalState.deep[Int, Seq[Int], Pure](0)(s => runChoice(pick(s))).run
    val tail = LexicalState.tail[Int, Seq[Int], Pure](0)(s => runChoice(pick(s))).run
    assertEquals(deep, (6, Seq(0, 1, 3)))
    assertEquals(tail, deep)
  }

  test("multi-shot ACROSS the installation: a state per branch, tail as deep") {
    val deep = runChoice(LexicalState.deep[Int, Int, Choose](0)(s => pick(s))).run
    assertEquals(deep, Seq((1, 0), (2, 0), (3, 0)))
    val tail = runChoice(LexicalState.tail[Int, Int, Choose](0)(s => pick(s))).run
    assertEquals(tail, deep)
  }

  /** resumes the rest twice, the answers of both */
  enum Twice[X]:
    case Go() extends Twice[Unit]
  val twice: Handler[Twice, List] = Handler.deep[Twice, List]([a] => (x: a) => List(x))(
    [a, X] => (e: Twice[X], k: X => List[a] ! Pure) => e match
      case Twice.Go() => k(()).flatMap(xs => k(()).map(xs ++ _)))

  test("ACROSS, leaving by abort: each resumption starts from the state the capture saw, never the other's") {
    // lexical-tail-guard-abort found the shape: a resumption that leaves the body by an abort to an outer scope never
    // returns through the installation, so a cell written by the first resumption would be read by the second
    type R = Escape
    def body(t: Lexical.Inst[Twice], p: Escape.Prompt[Int])(s: Lexical.Inst[State % Int]): Int ! R =
      for
        _ <- t.perform(Twice.Go())
        v <- s.get
        _ <- s.set(v + 1)
        r <- s.get
        _ <- Escape.escape[Unit, Int](p)(r)
      yield r
    def run(install: (Lexical.Inst[State % Int] => Int ! R) => (Int, Int) ! R): List[Int] =
      Escape.run(Lexical.handle(twice)(t => Escape.prompt[Int, R](p => install(body(t, p)).map(_._2)))).run
    // each resumption 0 -> 1: a cell would have answered 2 the second time
    assertEquals(run(LexicalState.deep[Int, Int, R](0)), List(1, 1))
    assertEquals(run(LexicalState.tail[Int, Int, R](0)), List(1, 1))
  }

  test("the same program run TWICE is not a multi-shot: the state is made per run") {
    val once = LexicalState.tail[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v + 1)))
    assertEquals(once.run, (6, 6))
    assertEquals(once.run, (6, 6))
  }

  test("depth: 100 000 operations through one tail instance, in constant stack") {
    def spin(s: Lexical.Inst[State % Int], n: Int): Int ! Pure =
      if n == 0 then s.get else s.get.flatMap(v => s.set(v + 1)).flatMap(_ => spin(s, n - 1))
    assertEquals(LexicalState.tail[Int, Int, Pure](0)(s => spin(s, 100_000)).run, (100_000, 100_000))
  }

/** the default, any handler form, and an instance kept past its installation */
class TestLexicalDefault extends munit.FunSuite:

  test("LexicalState(s0) is tail, on the pure row: run alone") {
    val p: (Int, Int) ! Pure = LexicalState[Int, Int, Pure](5)(s => s.get.flatMap(v => s.set(v * 3)))
    assertEquals(p.run, (15, 15))
  }

  test("Lexical.handle installs any handler form: a state (Stateful) and an answering one, nested, each its own") {
    val counter = Handler.state[State % Int, Int](7)([X] => (s: Int, e: State[Int, X]) => e match
      case State.Get() => (s, s)
      case State.Update(g) => { val (b, s1) = g(s); (s1, b) })
    val ask = Handler.answering[Reader % Int]([X] => (e: Reader[Int, X]) => e match
      case Reader.Ask() => 100
      case Reader.Asks(f) => f(100))
    val r = Lexical.handle(counter) { s =>
      Lexical.handle(ask)(env => s.get.flatMap(v => env.perform(Reader.Ask[Int, Int]()).flatMap(e => s.set(v * 2 + e))))
    }.run
    assertEquals(r, (114, 114))
  }

  test("keyed instances, one inside the other: the outer's operation passes an inner handler of its effect in the row") {
    // the inner installation a row handler of State % Int (`State.handle`): the outer instance's ops are not its
    val r = LexicalState.tail[Int, (Int, Int), Pure](0) { a =>
      State.handle(10)(for
        x <- a.get
        y <- State.get[Int]
        _ <- a.set(x + 5)
      yield x + y)
    }.run
    assertEquals(r, (5, (10, 10)))
  }

  test("an instance used AFTER its installation returned: the kernel's error where it is performed, no answer") {
    var leaked: Lexical.Inst[State % Int] | Null = null
    val p = LexicalState[Int, Int, Pure](0)(s => { leaked = s; s.get })
      .flatMap(_ => leaked.nn.get.map(v => (v, v)))
    val e = intercept[IllegalStateException](p.run)
    assert(e.getMessage.contains("no handler of it in force"), e.getMessage)
  }
