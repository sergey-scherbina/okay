package scala2probe

import okay.scala2._

/**
 * okay's vocabulary through the facade: the program lines AND the
 * runner below are IDENTICAL to okay2's `TestFacadeVocabulary`, compiled
 * there against okay2 and here against okay-scala2 + the prelude
 * (scala2-roads, then scala2-facade-okay-names, 2026-09-24).
 */
class TestOkayVocabularyFromScala2 extends munit.FunSuite {

  def step(limit: Int): Int ! (Reader[Int] + State[Int] + Throws[String]) = for {
    max <- Reader.ask[Int]
    n0 <- State.get[Int]
    n <- State.set(n0 + 1)
    r <- if (n > max) Throws.raise[String, Int]("over " + max) else State.get[Int]
  } yield r + limit

  val twice: Int ! (Reader[Int] + State[Int] + Throws[String]) = step(0).flatMap(_ => step(100))

  val searched: Int ! (Choose + Writer[String]) = for {
    x <- Choose.choose(1, 2, 3)
    _ <- Writer.tell("saw " + x)
  } yield x * 10

  test("State.set/handle, Throws.runEither, Reader.run: okay's names, the facade's runner") {
    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(5)(twice)))), (2, Right(102)))
    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(1)(twice)))), (2, Left("over 1")))
  }

  test("Choose.choose/runChoice and Writer.collect") {
    assertEquals(!.run(Writer.collect(Choose.runChoice(searched))), (Vector("saw 1", "saw 2", "saw 3"), Seq(10, 20, 30)))
  }

  test("Async(a) is okay2's spelling of a suspended computation") {
    var ran = 0
    val p = Async { ran += 1; ran }.map(_ * 2)
    assertEquals(ran, 0)
    assertEquals(p.runWith, 2)
  }
}
