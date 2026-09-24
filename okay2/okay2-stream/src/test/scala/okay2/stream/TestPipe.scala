package okay2.stream

import okay2._
import okay2.Later.later
import okay2.Stream.{FeedOps, FeedInOps}
import okay2.stream.Pipe._

/** Coroutine pipelines: tell meets await, one element at a time. */
class TestPipe extends munit.FunSuite {

  def count(n: Int): Nothing ! Writer[Int] = Writer.tell(n).flatMap(_ => count(n + 1))

  def sums(n: Int, acc: Int): Int ! Take[Int] =
    if (n == 0) pure(acc)
    else Take.await[Int].flatMap {
      case Some(x) => sums(n - 1, acc + x)
      case None => pure(acc)
    }

  def told(xs: Int*): Int ! Writer[Int] =
    xs.foldLeft(pure[Writer[Int], Int](0))((m, i) => m.flatMap(_ => Writer.tell(i).map(_ => i)))

  test("the consumer drives: a finite consumer ends an infinite producer") {
    assertEquals(pipe(count(0))(sums(5, 0)), 0 + 1 + 2 + 3 + 4)
  }

  test("a producer that ends first answers None to every further await") {
    val short: Int ! Writer[Int] = Writer.tell(10).flatMap(_ => Writer.tell(20)).map(_ => 0)
    assertEquals(pipe(short)(sums(5, 0)), 30)
    val nones: Int ! Take[Int] =
      Take.await[Int].flatMap(_ => Take.await[Int]).flatMap(_ => Take.await[Int].map(_.fold(-1)(identity)))
    assertEquals(pipe(short)(nones), -1)
  }

  test("stages: a producer through transducers, demand-driven") {
    def double: Stage[Int, Int, Unit] =
      Stage.await[Int, Int].flatMap {
        case Some(x) => Stage.tell[Int, Int](x * 2).flatMap(_ => double)
        case None => pure(())
      }
    // infinite producer, two stages, finite consumer: still lazy
    assertEquals(pipe(into(count(0))(double))(sums(3, 0)), 0 + 2 + 4)
    // through is associative on behavior
    val s1 = into(into(count(0))(double))(Stage.id[Int])
    val s2 = into(count(0))(through(double)(Stage.id[Int]))
    assertEquals(s1.toLazyList.take(5).toList, s2.toLazyList.take(5).toList)
  }

  test("transduce: state, conditional emission, and the flush") {
    val evens: Stage[Int, Int, Int] = Stage.transduce[Int, Int, Int](0)((sum, i) => {
      val s2 = sum + i
      if (i % 2 == 0) Stage.tell[Int, Int](s2).map(_ => s2) else pure(s2)
    }, s => Stage.tell[Int, Int](-s).map(_ => s))

    val (out, answer) = Effects.run(Writer.run(into(told(1, 2, 3, 4, 5, 6))(evens)))
    assertEquals(out, Seq(3, 10, 21, -21)) // 1+2, +3+4, +5+6, then the flush
    assertEquals(answer, 21)
  }

  test("mapAccumulate: fs2's 1:1 special case") {
    val runningTotal: Stage[Int, Int, Int] = Stage.mapAccumulate[Int, Int, Int](0)((sum, i) => (sum + i, sum + i))
    assertEquals(Effects.run(Writer.run(into(told(1, 2, 3, 4))(runningTotal)))._1, Seq(1, 3, 6, 10))
  }

  test("chunked/unchunk stages: batching with a flush, then flattening back") {
    val ten: Int ! Writer[Int] = told(0 to 10: _*)
    val chunks = into(ten)(Stage.chunked[Int](4)).toLazyList.toList
    assertEquals(chunks.map(_.length), List(4, 4, 3))
    assertEquals(chunks.flatten, (0 to 10).toList)
    val back = into(into(ten)(Stage.chunked[Int](4)))(Stage.unchunk[Int])
    assertEquals(back.toLazyList.toList, (0 to 10).toList)
  }

  test("a stage that accumulates past the pull budget does not overflow the stack") {
    val many: Int ! Writer[Int] = told(0 until 20000: _*)
    val chunks = into(many)(Stage.chunked[Int](4096)).toLazyList.toList
    assertEquals(chunks.map(_.length), List(4096, 4096, 4096, 4096, 3616))
  }

  test("an effectful producer pipes into a program of its effects") {
    type F = Writer[Int] + Later
    var performed = 0
    def ticks(n: Int): Unit ! F =
      later { performed += 1 }.at[F].flatMap(_ => Writer.tell(n).at[F].flatMap(_ => ticks(n + 1)))
    val result: Int ! Later = pipeIn[Int, Unit, Int, Later](ticks(7))(sums(3, 0))
    assertEquals(performed, 0)
    assertEquals(result.runWith, 7 + 8 + 9)
    assertEquals(performed, 3)
  }

  test("effectful stages: G ops forward through composition, lazily") {
    type Row = Take[Int] + (Writer[Int] + Later)
    var effects = 0
    def double: Unit ! Row =
      Take.await[Int].at[Row].flatMap {
        case Some(x) => later { effects += 1 }.at[Row].flatMap(_ => Writer.tell(x * 2).at[Row].flatMap(_ => double))
        case None => pure(())
      }
    def incPure: Stage[Int, Int, Unit] =
      Stage.await[Int, Int].flatMap {
        case Some(x) => Stage.tell[Int, Int](x + 1).flatMap(_ => incPure)
        case None => pure(())
      }
    val inc: Unit ! Row = incPure.at[Row]

    type Src = Writer[Int] + Later
    var told = 0
    def src(n: Int): Unit ! Src =
      if (n == 0) pure(())
      else later { told += 1 }.at[Src].flatMap(_ => Writer.tell(n).at[Src].flatMap(_ => src(n - 1)))

    val composed = throughIn[Int, Int, Int, Later, Unit, Unit](double)(inc)
    val out = intoIn[Int, Int, Later, Unit, Unit](src(3))(composed)
    assertEquals(effects, 0)
    assertEquals(told, 0)
    assertEquals(out.toLazyList.toList, List(7, 5, 3))
    assertEquals(effects, 3)
    assertEquals(told, 3)

    effects = 0; told = 0
    val left = intoIn[Int, Int, Later, Unit, Unit](intoIn[Int, Int, Later, Unit, Unit](src(3))(double))(inc)
    assertEquals(left.toLazyList.toList, List(7, 5, 3))
  }

  test("a program built by through/into/pipeIn starts its stage when RUN, once per run") {
    var starts = 0
    def counting: Stage[Int, Int, Unit] = Free.delay { () => starts += 1; Stage.id[Int] }
    val s1 = through(counting)(Stage.id[Int])
    assertEquals(starts, 0, "through(stage)(stage) builds, runs nothing")
    val p2 = into(told(1, 2, 3))(s1)
    assertEquals(starts, 0, "into(producer)(stage) builds, runs nothing")
    assertEquals(Effects.run(Writer.run(p2))._1, Seq(1, 2, 3))
    assertEquals(Effects.run(Writer.run(p2))._1, Seq(1, 2, 3))
    assertEquals(starts, 2, "one start per run")

    starts = 0
    type Row = Take[Int] + (Writer[Int] + Later)
    val countingG: Unit ! Row = Free.delay { () => starts += 1; Stage.id[Int].at[Row] }
    val idG: Unit ! Row = Stage.id[Int].at[Row]
    val s3 = throughIn[Int, Int, Int, Later, Unit, Unit](countingG)(idG)
    assertEquals(starts, 0)
    val toldG: Int ! (Writer[Int] + Later) = told(1, 2, 3).plus[Later]
    val p4 = intoIn[Int, Int, Later, Int, Unit](toldG)(s3)
    assertEquals(starts, 0)
    assertEquals(Writer.run(p4).runWith._1, Seq(1, 2, 3))
    assertEquals(Writer.run(p4).runWith._1, Seq(1, 2, 3))
    assertEquals(starts, 2)

    starts = 0
    val consumer: Int ! Take[Int] = Free.delay { () => starts += 1; sums(3, 0) }
    val p5 = pipeIn[Int, Int, Int, Later](toldG)(consumer)
    assertEquals(starts, 0)
    assertEquals(p5.runWith, 6)
    assertEquals(p5.runWith, 6)
    assertEquals(starts, 2)
  }

  test("Lines: bytes in, lines out, a multi-byte character split across chunks") {
    val text = "héllo\r\nwörld\nlast"
    val bytes = text.getBytes(java.nio.charset.StandardCharsets.UTF_8)
    // split inside the two-byte 'é' (bytes 1-2) and inside 'ö'
    val parts = List(bytes.slice(0, 2), bytes.slice(2, 10), bytes.slice(10, bytes.length))
    val fed: Unit ! Writer[Chunk[Byte]] = parts.foldLeft(pure[Writer[Chunk[Byte]], Unit](()))((p, c) =>
      p.flatMap(_ => Writer.tell(scala.collection.immutable.ArraySeq.unsafeWrapArray(c): Chunk[Byte])))
    assertEquals(into(fed)(Lines.stage).toLazyList.toList, List("héllo", "wörld", "last"))
  }
}

/** Take.each: the consumer side of an iteratee as a source */
class TestTakeEach extends munit.FunSuite {

  def told(xs: Int*): Unit ! Writer[Int] =
    xs.foldLeft(pure[Writer[Int], Unit](()))((p, x) => p.flatMap(_ => Writer.tell(x)))

  test("Take.each.loop is the consumer loop: every element the producer tells, then the end") {
    val seen = scala.collection.mutable.Buffer[Int]()
    Pipe.pipe(told(1, 2, 3))(Take.each[Int].loop(seen += _ * 2))
    assertEquals(seen.toList, List(2, 4, 6))
    seen.clear()
    Pipe.pipe(told())(Take.each[Int].loop(seen += _))
    assertEquals(seen.toList, Nil)
  }

  test("a finite consumer ends an infinite producer — the loop pulls only what it reads") {
    def naturals(n: Int): Unit ! Writer[Int] = Writer.tell(n).flatMap(_ => naturals(n + 1))
    val firstThree: List[Int] ! Take[Int] =
      Take.each[Int].step.flatMap {
        case Some((a, rest)) => rest.step.flatMap {
          case Some((b, rest2)) => rest2.step.map(_.map { case (c, _) => List(a, b, c) }.getOrElse(List(a, b)))
          case None => pure(List(a))
        }
        case None => pure(Nil)
      }
    assertEquals(Pipe.pipe(naturals(0))(firstThree), List(0, 1, 2))
  }
}
