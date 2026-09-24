package okay.frege

import okay.{!, %, +, Choose, Chunks, Reader, State, Writer, effect, pure, runChoice, through}
import okay.frege.{Programs as P}
import frege.run8.Thunk
import java.util.concurrent.atomic.AtomicInteger

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

  def emitIn[I, G[+_]](xs: Seq[I]): Unit ! Writer % I + G =
    xs.foldRight(pure[Writer % I + G, Unit](()))((x, p) => effect[Writer % I + G, Unit](Writer(x)).flatMap(_ => p))

  def countedNats(produced: AtomicInteger, size: Int): Chunks[java.lang.Long] =
    Chunks.map(Chunks.nats[Long](size))(x => { produced.incrementAndGet(): Unit; Long.box(x) })

  test("okay-frege.md: a Frege stage that performs") {
    val stage = Frege.stageWith[Long, java.lang.Long, Reader % Long](P.aboveThreshold.call())
    val out = through[Long, java.lang.Long, Reader % Long, Unit, Unit](emitIn[Long, Reader % Long](List(3L, 9L, 5L, 12L, 7L)))(stage)
    // Reader.run(6L)(Writer.run(out)) — told 9, 12, 7
    assertEquals(!.run(Reader.run(6L)(Writer.run(out)))._1.map(_.longValue), Seq(9L, 12L, 7L))
  }

  test("okay-frege.md: lists, both ways, lazily") {
    // an INFINITE Frege list, read partially by okay
    val firstFive = Chunks.take(Frege.chunks[java.lang.Long](P.squares.call()))(5)      // 1, 4, 9, 16, 25
    assertEquals(Chunks.foldLeft(firstFive)(Vector.empty[Long])(_ :+ _.longValue), Vector(1L, 4L, 9L, 16L, 25L))
    val produced = AtomicInteger()
    // an INFINITE okay source, handed to a Frege function that takes 10
    val sum = P.sumFirst(10, Frege.list(countedNats(produced, 16)))              // 45, and okay produced ≤ 16
    assertEquals(sum, 45L)
    assert(produced.get <= 16)
  }
