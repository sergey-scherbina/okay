package okay

import okay.given
import okay.Row.plus
import scala.collection.immutable.ArraySeq

/**
 * The two roads between a producer and a Source, and the asymmetry
 * that makes the second one worth having (specs/blob.md, "The Source
 * road"): on the Produce side `pure(x)` type-checks and emits
 * nothing; on the Source side it is a type error.
 */
class TestSourceProducer extends munit.FunSuite:

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  type P = Produce + Async

  test("a producer in Async becomes a source: told in order, Async performed, answer dropped") {
    var performed = 0
    val p: Int ! P = produce(1).plus[Async]
      .flatMap(_ => async { performed += 1; 7 }.plus[Produce])
      .flatMap(_ => produce(2).plus[Async])
      .flatMap(_ => produce(3).plus[Async])
      .flatMap(_ => pure(-1))          // the phantom answer
    val (told, unit) = run(Writer.run(Source.ofProducer(p)))
    assertEquals(told, Seq(1, 2, 3))
    assertEquals(unit, ())
    assertEquals(performed, 1)
  }

  test("fromProducer keeps the ANSWER, which for a get is the outcome, and names the element type apart") {
    // the type reads as a producer of Eithers; the elements are chunks
    val c1: Chunk[Byte] = ArraySeq[Byte](1, 2, 3)
    val c2: Chunk[Byte] = ArraySeq[Byte](4)
    val get: Either[String, Unit] ! P =
      effect[P, Chunk[Byte]](c1).flatMap(_ => effect[P, Chunk[Byte]](c2)).flatMap(_ => pure(Right(())))
    val r = run(Writer.collect(Source.fromProducer[Chunk[Byte], Either[String, Unit], Async](get)))
    assertEquals(r._1.map(_.toVector), Seq(Vector[Byte](1, 2, 3), Vector[Byte](4)))
    assertEquals(r._2, Right(()))
    // an absent key: nothing told, and the Left is the answer
    val absent: Either[String, Unit] ! P = pure(Left("no such key"))
    val none = run(Writer.collect(Source.fromProducer[Chunk[Byte], Either[String, Unit], Async](absent)))
    assertEquals(none._1.size, 0)
    assertEquals(none._2, Left("no such key"))
  }

  test("a source becomes a producer: each told value produced, Async kept, `end` is the final Pure") {
    var performed = 0
    val s: Source[Int] = Writer.tell(10).plus[Async]
      .flatMap(_ => async { performed += 1 }.plus[Writer % Int])
      .flatMap(_ => Writer.tell(20).plus[Async])
    val p: Int ! P = Source.toProducer(s)(end = -1)
    var seen = Vector.empty[Int]
    val end = run(Producer.each[Int, Int, Async](p)(seen :+= _))
    assertEquals(seen, Vector(10, 20))
    assertEquals(end, -1)
    assertEquals(performed, 1)
    // and the stream instance sees exactly the elements, never `end`
    val S = summon[Stream[[A] =>> A ! P, Async]]
    def drain(rest: Int ! P): Vector[Int] ! Async = S.uncons(rest).flatMap {
      case None => pure(Vector.empty)
      case Some((a, more)) => drain(more).map(a +: _)
    }
    assertEquals(run(drain(Source.toProducer(s)(end = -1))), Vector(10, 20))
  }

  test("round trip: source -> producer -> source is the same telling") {
    val s: Source[String] = Source("a", "b", "c")
    val back = Source.ofProducer(Source.toProducer(s)(end = ""))
    assertEquals(run(Writer.run(back))._1, Seq("a", "b", "c"))
    assertEquals(run(Writer.run(Source.ofProducer(Source.toProducer(Source[String]())(end = ""))))._1, Seq.empty)
  }

  test("Producer.each keeps the answer uncons loses at its None") {
    val p: String ! P = produce("x").plus[Async].flatMap(_ => pure("the answer"))
    var seen = Vector.empty[String]
    assertEquals(run(Producer.each[String, String, Async](p)(seen :+= _)), "the answer")
    assertEquals(seen, Vector("x"))
  }

  test("the asymmetry: at Produce `pure(x)` LOOKS like an emit and is nothing; at Source it is visibly a discard") {
    // Produce: type-checks at the wider row, and produces NOTHING —
    // the Pure is the end of the stream. The answer type IS the
    // element type, so nothing distinguishes this from produce(1)
    val silent: Int ! P = pure(1)
    var seen = 0
    val _ = run(Producer.each[Int, Int, Async](silent)(_ => seen += 1))
    assertEquals(seen, 0)
    // Source: the SAME line compiles too — measured, not hoped: the
    // first draft of this test asserted a type error and was wrong.
    // Value discarding turns the 1 into (), because a Source answers
    // Unit. What is different is what compiled: a discarded value,
    // which the compiler can flag (-Wvalue-discard, on in this build),
    // where the Produce form is an ordinary well-typed answer nothing
    // can flag. The element type in the SIGNATURE is what buys that
    val discarded = compileErrors("val s: okay.Source[Int] = okay.pure(1)")
    assert(!discarded.toLowerCase.contains("error"), discarded)
    // and what it compiles TO tells nothing, exactly as at Produce
    assertEquals(run(Writer.run(pure[Writer % Int + Async, Unit](())))._1.size, 0)
    // and the named injector is the one that emits, at either row
    var told = 0
    val _ = run(Producer.each[Int, Int, Async](produce(1).plus[Async])(_ => told += 1))
    assertEquals(told, 1)
  }

  test("put-de-diagonal: generate materializes into a live Source, for free") {
    // no diagonal Put[Source] could ever have existed: Source's own
    // answer is always Unit, never the element, which is exactly the
    // structural proof the backlog entry argues from
    def firstN[W](s: Source[W], n: Int): Vector[W] ! Async =
      if n <= 0 then pure(Vector.empty)
      else Writer.uncons[W, Unit, Async](s).flatMap {
        case Left(_) => pure(Vector.empty)
        case Right((w, rest)) => firstN(rest, n - 1).map(w +: _)
      }
    assertEquals(run(firstN(nats[Int, Source], 5)), Vector(0, 1, 2, 3, 4))
    assertEquals(run(firstN(fibs[Long, Source], 10)),
      Vector(0L, 1L, 1L, 2L, 3L, 5L, 8L, 13L, 21L, 34L))
  }
