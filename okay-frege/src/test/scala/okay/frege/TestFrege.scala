package okay.frege

import okay.{!, %, +, Choose, Reader, Stage, State, Throws, Writer, pure, runChoice, runEither, through}
import okay.given
import frege.run8.Thunk
import java.util.concurrent.atomic.AtomicInteger
import okay.frege.{Programs as P}

/**
 * Frege programs written in `okay.frege.Prog` run as okay programs
 * (specs/frege.md). The Frege side is src/test/frege/okay/frege/
 * Programs.fr, compiled by project/Frege.scala before this suite; the
 * `Prog` library itself is src/main/frege. Every loop is bounded, so a
 * broken driver FAILS rather than hangs.
 */
class TestFrege extends munit.FunSuite {

  def emit[I](xs: Seq[I]): Unit ! Writer % I =
    xs.foldRight(pure[Writer % I, Unit](()))((x, p) => Writer.tell(x).flatMap(_ => p))

  def own[I, O, A](src: Unit ! Writer % I, s: Stage[I, O, A]): List[O] =
    !.run(Writer.run(through(src)(s)))._1.toList

  val inputs: List[List[Long]] = List(Nil, List(7L), (1L to 10L).toList, (1L to 1000L).toList)

  // ---------------------------------------------------------------- stage

  test("law: a Frege runningSum stage tells what the same stage written in okay tells") {
    val okaySide = Stage.mapAccumulate[Long, Long, Long](0L)((s, i) => (s + i, s + i))
    for xs <- inputs do
      val frege = Frege.stage[Long, java.lang.Long](P.runningSum(Thunk.`lazy`(0L)).call())
      assertEquals(own(emit(xs), frege).map(_.longValue), own(emit(xs), okaySide), s"over ${xs.size}")
  }

  test("tells before the first await and after the last (empty input too)") {
    assertEquals(own(emit(List[Long]()), Frege.stage[Long, String](P.framed(0).call())), List("head", "n=0"))
    assertEquals(own(emit(List(4L, 5L)), Frege.stage[Long, String](P.framed(0).call())), List("head", "4", "5", "n=2"))
  }

  /** 0 until n told, counting what the stage actually pulled */
  def nat(n: Long, pulled: AtomicInteger): Unit ! Writer % Long =
    def go(i: Long): Unit ! Writer % Long =
      if i >= n then pure(())
      else Writer.tell(i).flatMap(_ => { pulled.incrementAndGet(): Unit; go(i + 1) })
    go(0)

  test("a Frege stage that returns early ends the stage: upstream is pulled no further") {
    val pulled = AtomicInteger()
    val out = own(nat(1000, pulled), Frege.stage[Long, java.lang.Long](P.firstThree(0).call()))
    assertEquals(out.map(_.longValue), List(0L, 1L, 2L))
    assert(pulled.get <= 3, s"pulled ${pulled.get} past a returned Frege stage")
  }

  test("a hundred thousand Frege steps on the default stack") {
    val out = own(emit(List[Long]()), Frege.stage[Long, java.lang.Long](P.countTo(0L, 100000L).call()))
    assertEquals((out.size, out.last.longValue), (100000, 99999L))
  }

  test("existing Frege IO, lifted: told in a stage, answered in a run") {
    assertEquals(own(emit(List[Long]()), Frege.stage[Long, String](P.usesIO.call())), List("hello, okay"))
    assertEquals(!.run(Reader.run(0L)(Frege.run[Reader % Long, String](P.liftedAnswer.call()))), "hello, okay!")
  }

  // ------------------------------------------------------------------ run

  test("Reader and State performed from Frege, in the order Frege's >>= asks") {
    val prog = Frege.run[Reader % Long + State % Long, java.lang.Long](P.readerState.call())(
      using summon, Frege.Row.of[Reader % Long] | Frege.Row.of[State % Long])
    val (s, a) = !.run(State.handle(5L)(Reader.run(7L)(prog)))
    assertEquals((s, a.longValue), (6L, 7006L))
  }

  test("a Throws raised from Frege reaches runEither as a Left") {
    assertEquals(!.run(runEither(Frege.run[Throws % String, java.lang.Long](P.raising.call()))), Left("boom from frege"))
  }

  test("MULTI-SHOT: Choose resumes the Frege continuation once per branch") {
    val all = !.run(runChoice(Frege.run[Choose, java.lang.Long](P.choosing.call())))
    assertEquals(all.map(_.longValue).sorted, Seq(11L, 12L, 21L, 22L))
  }

  test("an operation outside the row is refused by name") {
    val e = intercept[IllegalArgumentException](!.run(Reader.run(1L)(Frege.run[Reader % Long, java.lang.Long](P.raising.call()))))
    assert(e.getMessage.contains("not an operation of this program's row"), e.getMessage)
  }

  test("await or tell outside a stage, and perform inside one, are refused by name") {
    val e1 = intercept[IllegalStateException](!.run(Reader.run(1L)(Frege.run[Reader % Long, String](P.usesIO.call()))))
    assert(e1.getMessage.contains("tell outside a stage"), e1.getMessage)
    // a plain stage is stageWith at the empty row: perform is an
    // operation outside that row, refused by name with the road to take
    val e2 = intercept[IllegalArgumentException](own(emit(List(1L)), Frege.stage[Long, java.lang.Long](P.readerState.call())))
    assert(e2.getMessage.contains("stageWith[I, O, F] adds F"), e2.getMessage)
  }

  test("a Frege error surfaces at the step that forced it, with Frege's message") {
    val e = intercept[Throwable](!.run(Reader.run(1L)(Frege.run[Reader % Long, java.lang.Long](P.failing.call()))))
    assert(String.valueOf(e.getMessage).contains("frege says no") || String.valueOf(e.getCause).contains("frege says no"), String.valueOf(e))
  }

  test("PRICE: a Frege step through the driver, recorded not asserted") {
    val n = 200000L
    own(emit(List[Long]()), Frege.stage[Long, java.lang.Long](P.countTo(0L, 20000L).call())): Unit   // warm
    val t0 = System.nanoTime()
    val out = own(emit(List[Long]()), Frege.stage[Long, java.lang.Long](P.countTo(0L, n).call()))
    println(f"PRICE okay.frege Prog step: ${(System.nanoTime() - t0) / 1000.0 / n}%.3f us per tell over $n")
    assertEquals(out.size, n.toInt)
  }
}
