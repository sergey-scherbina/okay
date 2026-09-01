package okay

import okay.Direct.*

/** The flat block over Monadic: specs/direct-macro.md */
class TestDirect extends munit.FunSuite {

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
        val x = div(a, b).?
        div(x, c).? + 1
      }
    assertEquals(calc(12, 3, 2), Right(3))
    assertEquals(calc(12, 0, 2), Left("div0"))
    assertEquals(calc(12, 3, 0), Left("div0"))
  }

  test("vals: the flat block binds in order") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      direct[Option] {
        val x = mx.?
        val y = my.?
        x + y
      }
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(Some(2), None), None)
    assertEquals(add(None, Some(3)), None)
  }

  test("short-circuit: after a None nothing runs") {
    var ran = false
    val r = direct[Option] {
      val _ = (None: Option[Int]).?
      ran = true
      ()
    }
    assertEquals(r, None)
    assert(!ran)
  }

  test("subexpression marks hoist left to right") {
    val order = collection.mutable.ListBuffer[String]()
    def eff(tag: String, v: Int): Option[Int] = { order += tag; Some(v) }
    val r = direct[Option] { eff("a", 1).? + eff("b", 2).? }
    assertEquals(r, Some(3))
    assertEquals(order.toList, List("a", "b"))
  }

  test("if: marks in condition and branches, only the taken branch runs") {
    var b = 0
    def branch(v: Int): Option[Int] = { b += 1; Some(v) }
    def pick(c: Option[Boolean]): Option[Int] =
      direct[Option] { if c.? then branch(1).? else branch(2).? }
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
        m.? match
          case 0 => "zero"
          case n => Some(s"n=$n").?
      }
    assertEquals(f(Some(0)), Some("zero"))
    assertEquals(f(Some(7)), Some("n=7"))
    assertEquals(f(None), None)
  }

  test("multi-shot: the rest of the block re-runs per element") {
    var hits = 0
    val r = direct[List] {
      val x = List(1, 2, 3).?
      val y = List(10, 20).?
      hits += 1
      x * y
    }
    assertEquals(r, List(10, 20, 20, 40, 30, 60))
    assertEquals(hits, 6)
  }

  test("effects: a direct block over ! answers as the monadic program") {
    type W = Writer % String
    val prog: Int ! W = direct {
      Writer.tell("a").?
      val x = Free.pure[W, Int](21).?
      Writer.tell("b").?
      x * 2
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](prog))
    assertEquals(ws, Seq("a", "b"))
    assertEquals(a, 42)
  }

  test("effects: a two-effect row reflects both operations in one block") {
    type F = Reader % Int + Writer % String
    val prog: Int ! F = direct {
      val env = effect[F, Int](Reader.Ask()).?
      effect[F, Unit](Writer(s"env=$env")).?
      env + 1
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(prog)))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("a mark under a lambda is a compile error") {
    val errors = compileErrors(
      "okay.Direct.direct[List] { List(1).filter(i => List(i > 0).?) }(using summon[Monad[List]]) ")
    assert(errors.contains("lambda"), errors)
  }

  test("try around marks is a v2 compile error; by-name too") {
    val tr = compileErrors(
      "okay.Direct.direct[List] { try List(1).? catch { case _: Exception => 0 } }(using summon[Monad[List]]) ")
    assert(tr.contains("try"), tr)
    val bn = compileErrors(
      "okay.Direct.direct[Option] { Option(1).getOrElse(Option(2).?) }(using summon[Monad[Option]]) ")
    assert(bn.contains("by-name"), bn)
  }

  test("one mark: .? on a raw operation lifts it into the block's row") {
    type F = Reader % Int + Writer % String
    val prog: Int ! F = direct {
      val env = Reader.Ask[Int, Int]().?
      Writer(s"env=$env").?
      env + 1
    }
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](
      Reader.run[Int, Int, Writer % String](41)(prog)))
    assertEquals(ws, Seq("env=41"))
    assertEquals(a, 42)
  }

  test("a mark that is neither F nor a row operation is refused by type") {
    val e = compileErrors(
      "okay.Direct.direct[Option] { List(1).? }(using summon[Monad[Option]]) ")
    assert(e.contains("neither"), e)
  }

  test("&& and || keep the short-circuit (desugared to their If)") {
    var n = 0
    def eff(b: Boolean): Option[Boolean] = { n += 1; Some(b) }
    assertEquals(direct[Option] { eff(false).? && eff(true).? }, Some(false))
    assertEquals(n, 1)
    assertEquals(direct[Option] { eff(true).? || eff(false).? }, Some(true))
    assertEquals(n, 2)
  }

  test("for-do: an effect per element, in order") {
    val order = collection.mutable.ListBuffer[Int]()
    def eff(i: Int): Option[Int] = { order += i; Some(i) }
    val r = direct[Option] {
      for i <- List(1, 2, 3) do eff(i).?
      order.sum
    }
    assertEquals(r, Some(6))
    assertEquals(order.toList, List(1, 2, 3))
  }

  test("for-do over an Array receiver (the split case)") {
    type W = Writer % String
    val prog: Unit ! W = direct {
      for t <- "a b c".split(' ') do Writer(t + " ").?
    }
    val (ws, _) = !.run(Writer.run[String, Unit, okay.Pure](prog))
    assertEquals(ws, Seq("a ", "b ", "c "))
  }

  test("for-do: a None mid-loop short-circuits the rest") {
    var hits = 0
    def eff(i: Int): Option[Int] =
      { hits += 1; if i == 2 then None else Some(i) }
    val r = direct[Option] {
      for i <- List(1, 2, 3) do eff(i).?
      99
    }
    assertEquals(r, None)
    assertEquals(hits, 2, "the loop stopped at the None")
  }

  test("for-yield: the traverse shape, results in order") {
    def look(i: Int): Option[Int] = Some(i * 10)
    val r: Option[List[Int]] = direct[Option] {
      for i <- List(1, 2, 3) yield look(i).?
    }
    assertEquals(r, Some(List(10, 20, 30)))
  }

  test("while: effectful condition re-evaluates per iteration") {
    var n = 0
    def more: Option[Boolean] = { n += 1; Some(n < 4) }
    val r = direct[Option] {
      while more.? do ()
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
      for i <- List(1, 2) do { sum += List(10, 20).? * i; () }
      sum
    }
    assertEquals(r.length, 4, "2 elements x 2 continuations")
  }

  test("nested for-do loops") {
    val pairs = collection.mutable.ListBuffer[(Int, Int)]()
    def eff(p: (Int, Int)): Option[Unit] = { pairs += p; Some(()) }
    val r = direct[Option] {
      for i <- List(1, 2) do
        for j <- List(10, 20) do eff((i, j)).?
      pairs.size
    }
    assertEquals(r, Some(4))
    assertEquals(pairs.toList, List((1, 10), (1, 20), (2, 10), (2, 20)))
  }

  test("a non-whitelisted HOF with a mark keeps the refusal") {
    val e = compileErrors(
      "okay.Direct.direct[Option] { List(1).exists(i => Option(i > 0).?) }(using summon[Monad[Option]]) ")
    assert(e.contains("lambda"), e)
  }

  test("a mark outside any direct block throws by design") {
    intercept[IllegalStateException] { Option(1).? }
  }
}
