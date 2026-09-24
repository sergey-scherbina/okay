package okay2

/**
 * The program lines below are IDENTICAL to the facade probe's
 * `TestOkayVocabularyFromScala2`, runner included: okay's vocabulary,
 * compiled here against okay2 and there against okay-scala2 with its
 * 2.13 prelude (scala2-roads, scala2-facade-okay-names, 2026-09-24).
 * Nothing was added to okay2 for this: the facade took okay's names.
 */
class TestFacadeVocabulary extends munit.FunSuite {

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

  test("State.set/handle, Throws.runEither, Reader.run: the same lines as through the facade") {
    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(5)(twice)))), (2, Right(102)))
    assertEquals(!.run(State.handle(0)(Throws.runEither(Reader.run(1)(twice)))), (2, Left("over 1")))
  }

  test("Choose.choose/runChoice and Writer.collect") {
    assertEquals(!.run(Writer.collect(Choose.runChoice(searched))), (Vector("saw 1", "saw 2", "saw 3"), Seq(10, 20, 30)))
  }
}
