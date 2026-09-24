package okay.frege

import okay.{!, %, +, Async, Chunks, Reader, Writer, effect, pure, through}
import okay.given
import okay.frege.{Programs as P}
import java.util.concurrent.atomic.AtomicInteger

/**
 * okay-frege stage 2 (specs/frege.md): a Frege stage that also performs,
 * `Async` from Frege, and data both ways — Frege lists as okay `Chunks`
 * and okay `Chunks` as Frege lists, lazily, with the laziness COUNTED on
 * the okay side, where it can be.
 */
class TestFregeStage2 extends munit.FunSuite {

  def emit[I](xs: Seq[I]): Unit ! Writer % I =
    xs.foldRight(pure[Writer % I, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  /** a producer already in the stage's extra row, as okay-stream's
   * effectful `through` takes it */
  def emitIn[I, G[+_]](xs: Seq[I]): Unit ! Writer % I + G =
    xs.foldRight(pure[Writer % I + G, Unit](()))((x, p) => effect[Writer % I + G, Unit](Writer(x)).flatMap(_ => p))

  // --------------------------------------------------- effectful stage

  test("a Frege stage that performs: a filter asking its threshold from Reader") {
    val stage = Frege.stageWith[Long, java.lang.Long, Reader % Long](P.aboveThreshold.call())
    val out = through[Long, java.lang.Long, Reader % Long, Unit, Unit](emitIn[Long, Reader % Long](List(3L, 9L, 5L, 12L, 7L)))(stage)
    val told = !.run(Reader.run(6L)(Writer.run(out)))._1
    assertEquals(told.map(_.longValue), Seq(9L, 12L, 7L))
  }

  test("a plain stage refuses perform by name, and says where the road is") {
    val e = intercept[IllegalArgumentException](
      !.run(Writer.run(through(emit(List(1L)))(Frege.stage[Long, java.lang.Long](P.aboveThreshold.call())))))
    assert(e.getMessage.contains("stageWith[I, O, F] adds F"), e.getMessage)
  }

  // ------------------------------------------------------------ Async

  test("Async from Frege: two 20 ms sleeps take at least 40 ms and answer 40") {
    val t0 = System.nanoTime()
    val slept = Async.run(Frege.run[Async, java.lang.Long](P.sleepTwice.call())).runWith
    val ms = (System.nanoTime() - t0) / 1_000_000
    assertEquals(slept.longValue, 40L)
    assert(ms >= 40, s"two 20 ms sleeps took $ms ms")
  }

  // ------------------------------------------------------------- lists

  test("a Frege INFINITE list read partially as okay Chunks") {
    val firstFive = Chunks.foldLeft(Chunks.take(Frege.chunks[java.lang.Long](P.squares.call()))(5))(Vector.empty[Long])(_ :+ _.longValue)
    assertEquals(firstFive, Vector(1L, 4L, 9L, 16L, 25L))
  }

  test("the same Frege list read twice as Chunks gives the same elements (a list is a value)") {
    val c = Chunks.take(Frege.chunks[java.lang.Long](P.squares.call(), size = 3))(7)
    val read = () => Chunks.foldLeft(c)(Vector.empty[Long])(_ :+ _.longValue)
    assertEquals(read(), read())
  }

  /** okay's naturals, counting every element okay PRODUCES */
  def countedNats(produced: AtomicInteger, size: Int): Chunks[java.lang.Long] =
    Chunks.map(Chunks.nats[Long](size))(x => { produced.incrementAndGet(): Unit; Long.box(x) })

  test("an INFINITE okay source as a Frege list: Frege takes 10, okay produces one chunk") {
    val produced = AtomicInteger()
    val sum = P.sumFirst(10, Frege.list(countedNats(produced, 16)))
    assertEquals(sum, (0L until 10L).sum)
    assert(produced.get <= 16, s"okay produced ${produced.get} elements for a Frege take of 10 (chunk 16)")
  }

  test("Frege filters an okay source lazily, and okay reads the result back as Chunks") {
    val produced = AtomicInteger()
    val evens = P.evensOf(Frege.list(countedNats(produced, 8)))
    val firstFour = Chunks.foldLeft(Chunks.take(Frege.chunks[java.lang.Long](evens, size = 4))(4))(Vector.empty[Long])(_ :+ _.longValue)
    assertEquals(firstFour, Vector(0L, 2L, 4L, 6L))
    assert(produced.get <= 16, s"okay produced ${produced.get} elements for four evens (chunk 8)")
  }

  test("an element of the wrong type in a Frege list is refused by name") {
    val e = intercept[IllegalArgumentException](
      Chunks.foldLeft(Frege.chunks[String](P.squares.call()))(0)((n, _) => n + 1))
    assert(e.getMessage.contains("java.lang.Long"), e.getMessage)
  }

  // ------------------------------------------------------------- Maybe

  test("Maybe <-> Option, both ways") {
    assertEquals(Frege.option[java.lang.Long](Frege.maybe(Some(Long.box(7L)))).map(_.longValue), Some(7L))
    assertEquals(Frege.option[java.lang.Long](Frege.maybe(None)), None)
  }
}
