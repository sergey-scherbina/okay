package okay.frege

import okay.{!, %, +, Choose, Reader, State, Writer, pure, runChoice, through}
import okay.frege.{Programs as P}
import frege.run8.Thunk

/**
 * The Scala examples docs/modules/okay-frege.md and docs/guide.md §5
 * print, VERBATIM (the-record-outlives-the-truth); the Frege side they
 * show is src/test/frege/okay/frege/Programs.fr itself, compiled.
 */
class TestDocExamplesFrege extends munit.FunSuite:

  def numbers(xs: Long*): Unit ! Writer % Long =
    xs.foldRight(pure[Writer % Long, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  def told[O](p: Unit ! Writer % O): Seq[O] = !.run(Writer.run(p))._1

  test("okay-frege.md: a Frege stage in an okay pipeline") {
    val sums = through(numbers(1, 2, 3, 4))(Frege.stage[Long, java.lang.Long](P.runningSum(Thunk.`lazy`(0L)).call()))
    // Writer.run(sums) — (Seq(1, 3, 6, 10), ())
    assertEquals(told(sums).map(_.longValue), Seq(1L, 3L, 6L, 10L))
  }

  test("okay-frege.md: okay's effects performed from Frege, one row built with |") {
    val prog = Frege.run[Reader % Long + State % Long, java.lang.Long](P.readerState.call())(
      using summon, Frege.Row.of[Reader % Long] | Frege.Row.of[State % Long])
    val answer = !.run(State.handle(5L)(Reader.run(7L)(prog)))   // (6, 7006)
    assertEquals((answer._1, answer._2.longValue), (6L, 7006L))
  }

  test("okay-frege.md / guide §5: multi-shot — Choose resumes the Frege continuation per branch") {
    val all = !.run(runChoice(Frege.run[Choose, java.lang.Long](P.choosing.call())))   // 11, 12, 21, 22
    assertEquals(all.map(_.longValue).sorted, Seq(11L, 12L, 21L, 22L))
  }
