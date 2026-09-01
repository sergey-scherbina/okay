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
      "okay.Direct.direct[List] { List(1).map(i => List(i).?) }(using summon[Monad[List]]) ")
    assert(errors.contains("lambda"), errors)
  }

  test("while and try around marks are v2 compile errors; by-name too") {
    val w = compileErrors(
      "okay.Direct.direct[List] { while (List(true).?) {}; 1 }(using summon[Monad[List]]) ")
    assert(w.contains("while"), w)
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

  test("a mark outside any direct block throws by design") {
    intercept[IllegalStateException] { Option(1).? }
  }
}
