package okay

import okay.Monadic.*

/** Filinski's monadic reflection over Cont: specs/monadic-reflection.md */
class TestMonadic extends munit.FunSuite {

  // stdlib instances live HERE, not in the core (see the spec's
  // Decisions): the test is their only consumer
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

  test("round trip: reify(reflect(m)) == m") {
    assertEquals(reify(reflect(Option(42))), Option(42))
    assertEquals(reify(reflect(None: Option[Int])), None)
    assertEquals(reify(reflect(List(1, 2, 3))), List(1, 2, 3))
    assertEquals(reify(reflect(Right(1): Either[String, Int])), Right(1): Either[String, Int])
  }

  test("direct style: reflected values are plain values in one block") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      reify:
        for
          x <- reflect(mx)
          y <- reflect(my)
        yield x + y
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(Some(2), None), None)
    assertEquals(add(None, Some(3)), None)
  }

  test("short-circuit: a reflected None drops the rest of the block") {
    var ran = false
    val r: Option[Unit] = reify:
      for
        _ <- reflect(None: Option[Int])
        _ <- reflect { ran = true; Option(1) }
      yield ()
    assertEquals(r, None)
    assert(!ran, "the continuation after a None must never run")
  }

  test("error channel: Either reflects with its Left intact") {
    def div(a: Int, b: Int): Either[String, Int] =
      if b == 0 then Left("div0") else Right(a / b)
    def calc(a: Int, b: Int, c: Int): Either[String, Int] =
      reify:
        for
          x <- reflect(div(a, b))
          y <- reflect(div(x, c))
        yield y + 1
    assertEquals(calc(12, 3, 2), Right(3))
    assertEquals(calc(12, 0, 2), Left("div0"))
    assertEquals(calc(12, 3, 0), Left("div0"))
  }

  test("multi-shot: a reflected List re-runs the continuation per element") {
    var hits = 0
    val r: List[Int] = reify:
      for
        x <- reflect(List(1, 2, 3))
        y <- reflect(List(10, 20))
      yield { hits += 1; x * y }
    assertEquals(r, List(10, 20, 20, 40, 30, 60))
    assertEquals(hits, 6, "3 × 2 continuations, each run to the end")
  }

  test("postfix forms: m.reflect and m.? are the same μ") {
    def add(mx: Option[Int], my: Option[Int]): Option[Int] =
      reify:
        for
          x <- mx.reflect
          y <- my.?
        yield x + y
    assertEquals(add(Some(2), Some(3)), Some(5))
    assertEquals(add(None, Some(3)), None)
  }

  test("plain control flow between reflects") {
    def clamp(m: Option[Int]): Option[String] =
      reify:
        for x <- reflect(m)
        yield
          val y = if x > 9 then 9 else x
          "*" * y
    assertEquals(clamp(Some(3)), Some("***"))
    assertEquals(clamp(Some(100)), Some("*" * 9))
    assertEquals(clamp(None), None)
  }

  test("a strict monad reflects a modest chain: 1_000 binds") {
    // a strict flatMap invokes the continuation in place, so each
    // reflect costs a stack frame — the budget is the MONAD's, not
    // Cont's (see the spec's Decisions); trampolined depth is below
    val n = 1_000
    val r: Option[Int] = reify:
      (1 to n).foldLeft(Cont.Pure(0): Cont[Int, Option[Int], Option[Int]]) {
        (acc, _) => acc.flatMap(x => reflect(Option(x + 1)))
      }
    assertEquals(r, Some(n))
  }

  test("a trampolined monad reflects stack-safely: 100_000 binds") {
    val n = 100_000
    val prog: Int ! okay.Pure = reify:
      (1 to n).foldLeft(Cont.Pure(0): Cont[Int, Int ! okay.Pure, Int ! okay.Pure]) {
        (acc, _) => acc.flatMap(x => reflect(Free.pure[okay.Pure, Int](x + 1)))
      }
    assertEquals(!.run(prog), n)
  }

  test("okay's own programs reflect: Writer effects survive the round trip") {
    type W = Writer % String
    val prog: Int ! W = reify:
      for
        _ <- Writer.tell("a").reflect
        x <- Free.pure[W, Int](21).reflect
        _ <- Writer.tell("b").reflect
      yield x * 2
    val (ws, a) = !.run(Writer.run[String, Int, okay.Pure](prog))
    assertEquals(ws, Seq("a", "b"))
    assertEquals(a, 42)
  }
}
