package okay.std.tests

import okay.*
import okay.given
import scala.language.implicitConversions
import okay.std.*


/**
 * docs/effects-and-continuations.md, VERBATIM: every example line as the page prints it, answer comment
 * included, then asserted. The direct-style section is okay-direct's TestDocExamplesLevel1Direct.
 */
class TestDocExamplesLevel1 extends munit.FunSuite:

  test("effects: perform, handle, run") {
    val counter: Int ! State % Int =
      for
        n <- State.get[Int]
        _ <- State.set(n + 1)
      yield n * 10

    val a = counter.handle(State(5)).run   // (6, 50)

    val checked: Int ! State % Int + Throws % String =
      for
        n <- State.get[Int].at[State % Int + Throws % String]
        r <- (if n > 3 then raise[String, Int]("too big") else pure(n))
      yield r

    val b = checked.handle(State(5)).handle(Throws.either).run   // Left("too big")
    val c = checked.handle(Throws.either).handle(State(1)).run   // (1, Right(1))
    assertEquals(a, (6, 50))
    assertEquals(b, Left("too big"))
    assertEquals(c, (1, Right(1)))
  }

  test("tutorial §1, in level 1's words") {
    val prog: Int ! State % Int =
      for
        x <- State.get[Int]
        _ <- State.set(x + 40)
        y <- State.get[Int]
      yield y + 2

    val ran = prog.handle(State(0)).run   // (40, 42) — the final state and the answer

    type F = State % Int + Throws % String
    def risky(n: Int): Int ! F =
      if n < 0 then raise[String, Int]("negative").at[F] else effect[State % Int, Int](State.Update[Int, Int](_ => (n, n))).at[F]

    val both = risky(5).handle(State(0)).handle(Throws.either).run   // handle State, then Throws: Right((5, 5))
    assertEquals(ran, (40, 42))
    assertEquals(both, Right((5, 5)))
  }

  test("perform: an operation as a program, either spelling") {
    val asked: Int ! State % Int = effect(State.Get[Int, Int]())
    assertEquals(asked.handle(State(3)).run, (3, 3))
  }

  test("docs/modules/okay-freer.md: using it") {
    val p: Int ! State % Int = State.get[Int].map(_ + 1)
    val j = p.handle(State(41)).run   // (41, 42)
    assertEquals(j, (41, 42))
  }
