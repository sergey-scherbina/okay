package okay

import okay.Direct.*

/** The flat block over Monadic: specs/direct-macro.md */
class TestDirect extends munit.FunSuite {

  test("direct-try: a strict monad catches everything — construction IS the run") {
    def boom(n: Int): Int = throw new IllegalStateException(s"boom $n")
    val r = direct[Option] {
      try
        val x = Option(3).reflect
        boom(x)
      catch case _: IllegalStateException => -1
    }
    assertEquals(r, Some(-1))
    // the no-throw path is untouched
    assertEquals(direct[Option] {
      try Option(3).reflect + 1 catch case _: IllegalStateException => -1
    }, Some(4))
  }

  test("direct-try on a Free row: a pure segment throwing AFTER a mark is caught at run") {
    type W = [A] =>> A ! Writer % String
    val prog: Int ! Writer % String = direct[W] {
      try
        val a = !okay.effect[Writer % String, Unit](Writer("before"))
        if true then throw new IllegalStateException("mid-stream") else 0
      catch case _: IllegalStateException => 7
    }
    val (logged, n) = Writer.run[String, Int, Pure](prog).runWith
    assertEquals(n, 7)
    assertEquals(logged.toList, List("before"), "the effect before the throw happened")
    // an exception the cases do not match RETHROWS
    val rethrow: Int ! Writer % String = direct[W] {
      try
        val _ = !okay.effect[Writer % String, Unit](Writer("x"))
        throw new RuntimeException("unmatched")
      catch case _: IllegalStateException => 0
    }
    intercept[RuntimeException](Writer.run[String, Int, Pure](rethrow).runWith)
  }

  test("direct-try v1 edges are refused NAMED: finalizer; a mark in a catch body") {
    val e1 = compileErrors(
      "okay.Direct.direct[Option] { try Option(1).reflect catch { case _: Exception => 0 } finally () }")
    assert(e1.contains("finalizer"), e1)
    val e2 = compileErrors(
      "okay.Direct.direct[Option] { try Option(1).reflect catch { case _: Exception => Option(0).reflect } }")
    assert(e2.contains("catch body"), e2)
  }


  test("the ! mark: one glyph, prefix, on the rows where ? is ambiguous") {
    // a Free-row program collapses under its own type's symbol
    val prog: Int ! Writer % String = direct[[A] =>> A ! Writer % String] {
      val a = !okay.effect[Writer % String, Unit](Writer("hi"))
      21 + 21
    }
    val ((logged, n)) = Writer.run[String, Int, Pure](prog).runWith
    assertEquals(n, 42)
    assertEquals(logged.toList, List("hi"))
    // outside a block the mark stays a loud failure, like its kin
    intercept[IllegalStateException](!okay.pure[Pure, Int](1))
  }


  given Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)
    extension [A](m: Option[A])
      override def flatMap[B](f: A => Option[B]): Option[B] = m.flatMap(f)

  given Monad[List] with
    override def pure[A](a: A): List[A] = List(a)
    extension [A](m: List[A])
      override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)

  given [E]: Monad[[A] =>> Either[E, A]] with
    override def pure[A](a: A): Either[E, A] = Right(a)
    extension [A](m: Either[E, A])
      override def flatMap[B](f: A => Either[E, B]): Either[E, B] = m.flatMap(f)

  test("no marks: direct is pure") {
    assertEquals(direct[Option] { 42 }, Some(42))
  }

  test("Either: the error channel short-circuits the flat block") {
    def div(a: Int, b: Int): Either[String, Int] =
      if b == 0 then Left("div0") else Right(a / b)
    def calc(a: Int, b: Int, c: Int): Either[String, Int] =
      direct[[A] =>> Either[String, A]] {
        val x = div(a, b).reflect
        div(x, c).reflect + 1
      }
    assertEquals(calc(12, 3, 2), Right(3))
    assertEquals(calc(12, 0, 2), Left("div0"))
    assertEquals(calc(12, 3, 0), Left("div0"))
  }

  test("vals: the flat block binds in order") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      direct[Option] {
        val x = mx.reflect
        val y = my.reflect
        x + y
      }
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(Some(2), None), None)
    assertEquals(add(None, Some(3)), None)
  }

  test("short-circuit: after a None nothing runs") {
    var ran = false
    val r = direct[Option] {
      val _ = (None: Option[Int]).reflect
      ran = true
      ()
    }
    assertEquals(r, None)
    assert(!ran)
  }

  test("subexpression marks hoist left to right") {
    val order = collection.mutable.ListBuffer[String]()
    def eff(tag: String, v: Int): Option[Int] = { order += tag; Some(v) }
    val r = direct[Option] { eff("a", 1).reflect + eff("b", 2).reflect }
    assertEquals(r, Some(3))
    assertEquals(order.toList, List("a", "b"))
  }

  test("if: marks in condition and branches, only the taken branch runs") {
    var b = 0
    def branch(v: Int): Option[Int] = { b += 1; Some(v) }
    def pick(c: Option[Boolean]): Option[Int] =
      direct[Option] { if c.reflect then branch(1).reflect else branch(2).reflect }
    assertEquals(pick(Some(true)), Some(1))
    assertEquals(b, 1)
    assertEquals(pick(Some(false)), Some(2))
    assertEquals(b, 2)
    assertEquals(pick(None), None)
    assertEquals(b, 2)
  }

  test("match: marks in scrutinee and case bodies") {
    def f(m: Option[Int]): Option[String] =
      direct[Option] {
        m.reflect match
          case 0 => "zero"
          case n => Some(s"n=$n").reflect
      }
    assertEquals(f(Some(0)), Some("zero"))
    assertEquals(f(Some(7)), Some("n=7"))
    assertEquals(f(None), None)
  }

  test("multi-shot: the rest of the block re-runs per element") {
    var hits = 0
    val r = direct[List] {
      val x = List(1, 2, 3).reflect
      val y = List(10, 20).reflect
      hits += 1
      x * y
    }
    assertEquals(r, List(10, 20, 20, 40, 30, 60))
    assertEquals(hits, 6)
  }

  test("effects: a direct block over ! answers as the monadic program") {
    type W = Writer % String
    val prog: Int ! W = direct {
      Writer.tell("a").reflect
      val x = Free.pure[W, Int](21).reflect
      Writer.tell("b").reflect
      x * 2
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](prog))
    assertEquals(ws, Seq("a", "b"))
    assertEquals(a, 42)
  }

  test("effects: a two-effect row reflects both operations in one block") {
    type F = Reader % Int + Writer % String
    val prog: Int ! F = direct {
      val env = effect[F, Int](Reader.Ask()).reflect
      effect[F, Unit](Writer(s"env=$env")).reflect
      env + 1
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(prog)))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("a mark under a lambda is a compile error") {
    val errors = compileErrors(
      "okay.Direct.direct[List] { List(1).filter(i => List(i > 0).reflect) }(using summon[Monad[List]]) ")
    assert(errors.contains("lambda"), errors)
  }

  test("a mark under a by-name argument is a compile error") {
    // (try around marks GRADUATED: direct-try rewrote it — see the
    // direct-try tests above; the finalizer and catch-mark edges
    // remain refused there, named)
    val bn = compileErrors(
      "okay.Direct.direct[Option] { Option(1).getOrElse(Option(2).reflect) }(using summon[Monad[Option]]) ")
    assert(bn.contains("by-name"), bn)
  }

  test("one mark, two spellings: .reflect and .!? on values and operations") {
    type F = Reader % Int + Writer % String
    val prog: Int ! F = direct {
      val env = Reader.Ask[Int, Int]().!?
      Writer(s"env=$env").reflect
      env + 1
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(prog)))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("a mark that is neither F nor a row operation is refused by type") {
    val e = compileErrors(
      "okay.Direct.direct[Option] { List(1).reflect }(using summon[Monad[Option]]) ")
    assert(e.contains("neither"), e)
  }

  test("&& and || keep the short-circuit (desugared to their If)") {
    var n = 0
    def eff(b: Boolean): Option[Boolean] = { n += 1; Some(b) }
    assertEquals(direct[Option] { eff(false).reflect && eff(true).reflect }, Some(false))
    assertEquals(n, 1)
    assertEquals(direct[Option] { eff(true).reflect || eff(false).reflect }, Some(true))
    assertEquals(n, 2)
  }

  test("for-do: an effect per element, in order") {
    val order = collection.mutable.ListBuffer[Int]()
    def eff(i: Int): Option[Int] = { order += i; Some(i) }
    val r = direct[Option] {
      for i <- List(1, 2, 3) do eff(i).reflect
      order.sum
    }
    assertEquals(r, Some(6))
    assertEquals(order.toList, List(1, 2, 3))
  }

  test("for-do over an Array receiver (the split case)") {
    type W = Writer % String
    val prog: Unit ! W = direct {
      for t <- "a b c".split(' ') do Writer(t + " ").reflect
    }
    val (ws, _) = !.run(Writer.run[String, Unit, okay.Pure](prog))
    assertEquals(ws, Seq("a ", "b ", "c "))
  }

  test("for-do: a None mid-loop short-circuits the rest") {
    var hits = 0
    def eff(i: Int): Option[Int] =
      { hits += 1; if i == 2 then None else Some(i) }
    val r = direct[Option] {
      for i <- List(1, 2, 3) do eff(i).reflect
      99
    }
    assertEquals(r, None)
    assertEquals(hits, 2, "the loop stopped at the None")
  }

  test("for-yield: the traverse shape, results in order") {
    def look(i: Int): Option[Int] = Some(i * 10)
    val r: Option[List[Int]] = direct[Option] {
      for i <- List(1, 2, 3) yield look(i).reflect
    }
    assertEquals(r, Some(List(10, 20, 30)))
  }

  test("while: effectful condition re-evaluates per iteration") {
    var n = 0
    def more: Option[Boolean] = { n += 1; Some(n < 4) }
    val r = direct[Option] {
      while more.reflect do ()
      n
    }
    assertEquals(r, Some(4))
  }

  test("multi-shot inside a loop body: immutable re-entry") {
    // a List reflect INSIDE the loop body re-runs the REST of the
    // loop per element — the emitted loop recurses over an immutable
    // List, so re-entry cannot corrupt iteration state
    given Monad[List] with
      override def pure[A](a: A): List[A] = List(a)
      extension [A](m: List[A])
        override def flatMap[B](f: A => List[B]): List[B] = m.flatMap(f)
    var sum = 0
    val r = direct[List] {
      for i <- List(1, 2) do { sum += List(10, 20).reflect * i; () }
      sum
    }
    assertEquals(r.length, 4, "2 elements x 2 continuations")
  }

  test("nested for-do loops") {
    val pairs = collection.mutable.ListBuffer[(Int, Int)]()
    def eff(p: (Int, Int)): Option[Unit] = { pairs += p; Some(()) }
    val r = direct[Option] {
      for i <- List(1, 2) do
        for j <- List(10, 20) do eff((i, j)).reflect
      pairs.size
    }
    assertEquals(r, Some(4))
    assertEquals(pairs.toList, List((1, 10), (1, 20), (2, 10), (2, 20)))
  }

  test("a BARE op as the for-do body runs — statement semantics reach loop bodies") {
    type W = Writer % String
    val prog: Unit ! W = direct {
      for t <- "a b c".split(' ') do Writer(t)   // no mark at all
    }
    val (ws, _) = !.run(Writer.run[String, Unit, okay.Pure](prog))
    assertEquals(ws, Seq("a", "b", "c"))
  }

  test("a BARE op as the while body runs too") {
    type W = Writer % String
    var n = 0
    val prog: Int ! W = direct {
      while { n += 1; pure[W, Boolean](n < 3).reflect } do Writer(s"tick$n")
      n
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](prog))
    assertEquals(ws, Seq("tick1", "tick2"))
    assertEquals(a, 3)
  }

  test("a non-whitelisted HOF with a mark keeps the refusal") {
    val e = compileErrors(
      "okay.Direct.direct[Option] { List(1).exists(i => Option(i > 0).reflect) }(using summon[Monad[Option]]) ")
    assert(e.contains("lambda"), e)
  }

  test("a mark outside any direct block throws by design") {
    intercept[IllegalStateException] { Option(1).reflect }
  }
}
