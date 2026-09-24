package okay

import scala.collection.immutable.ArraySeq

/**
 * specs/fold-until.md on the stream carriers: `Chunks.foldUntil`,
 * `Writer.foldUntil`, `Source.runFoldUntil` agree with the pure road,
 * and each STOPS — no chunk pulled after the satisfying one, no Async
 * operation performed after the stop, and the producer not resumed
 * past the satisfying tell.
 */
class TestFoldUntilStreams extends munit.FunSuite:

  type R = Writer % Int + Async

  /** 1, perform, 2, perform, 3, perform, ... — an Async op after every tell */
  private def counted(n: Int, performed: () => Unit): Source[Int] =
    def go(i: Int): Source[Int] =
      if i > n then okay.pure(())
      else okay.effect[R, Unit](Writer(i))
        .flatMap(_ => okay.effect[R, Unit](Async.Run(() => performed())))
        .flatMap(_ => go(i + 1))
    okay.pure[R, Unit](()).flatMap(_ => go(1))

  private def chunked(xs: List[Int], size: Int): Chunks[Int] =
    def go(rest: List[Int]): Chunks[Int] =
      if rest.isEmpty then okay.pure(())
      else Writer.tell(ArraySeq.from(rest.take(size))).flatMap(_ => go(rest.drop(size)))
    go(xs)

  test("the four carriers agree with the pure road on every instance") {
    val xs = (1 to 20).toList
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit =
      val expected = Stream.foldUntil(xs)(using fo)
      assertEquals(Chunks.foldUntil(chunked(xs, 4))(using fo), expected, s"Chunks $name")
      assertEquals(Writer.foldUntil[Int, S, Unit, X, Async](counted(20, () => ()))(using summon)(using summon, fo).runWith, expected, s"Writer $name")
      assertEquals(Source.of(xs).runFoldUntil(using fo).runWith, expected, s"Source $name")
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.take(100), "take(100)")
    check(FoldUntil.find[Int](_ > 7), "find")
    check(FoldUntil.find[Int](_ > 70), "find-none")
    check(FoldUntil.exists[Int](_ == 11), "exists")
    check(FoldUntil.forall[Int](_ < 5), "forall")
    check(FoldUntil.headOption, "headOption")
    check(FoldUntil.until[Int, Int, Int](0)((s, a) => if s + a > 30 then Right(s) else Left(s + a))(identity), "until")
  }

  test("Chunks.foldUntil's unboxed arms agree with the generic one and stop at the chunk") {
    var pulls = 0
    val chunked = Chunks.generateWith(0) { i => pulls += 1; (ArraySeq.range(i, i + 4), i + 4) }
    val sumL = FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ > 50)(identity)   // 0+..+10 = 55, in the 3rd chunk
    assertEquals(Chunks.foldUntil(chunked)(using sumL), 55L)
    assertEquals(pulls, 3)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil[Int, Long, Long](0L)((s, a) => s + a)(_ > 50)(identity)), 55L)
    assertEquals(pulls, 3)
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.int[Int, Int](0)((s, a) => s + a)(_ > 50)(identity)), 55)
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.double[Int, Double](0.0)((s, a) => s + a)(_ > 50)(identity)), 55.0)
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.exists[Int](_ == 9)), true)
  }

  test("Chunks.foldUntil pulls no chunk after the one that satisfied it") {
    var pulls = 0
    val chunked = Chunks.generateWith(0) { i => pulls += 1; (ArraySeq.range(i, i + 4), i + 4) }
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.take[Int](3)), Vector(0, 1, 2))
    assertEquals(pulls, 1)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.find[Int](_ == 5)), Some(5))
    assertEquals(pulls, 2)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(using FoldUntil.take[Int](0)), Vector.empty[Int])
    assertEquals(pulls, 0)
  }

  test("Source.runFoldUntil performs the Async op before the stop and not the one after it") {
    var performed = 0
    // take(3): tells 1, 2, 3; the op after 3 is never reached, so 2 performed
    assertEquals(counted(1000, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](3)).runWith, Vector(1, 2, 3))
    assertEquals(performed, 2)
    performed = 0
    assertEquals(counted(1000, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](0)).runWith, Vector.empty[Int])
    assertEquals(performed, 0)
    performed = 0
    assertEquals(counted(5, () => performed += 1).runFoldUntil(using FoldUntil.take[Int](100)).runWith, Vector(1, 2, 3, 4, 5))
    assertEquals(performed, 5)
  }

  test("the effectful writer program's .foldUntil: one using, the Handler[Async] in scope runs the rest") {
    var performed = 0
    assertEquals(counted(1000, () => performed += 1).foldUntil(using FoldUntil.take[Int](3)), Vector(1, 2, 3))
    assertEquals(performed, 2)
    assertEquals(counted(5, () => performed += 1).foldUntil(using FoldUntil.find[Int](_ > 9)), None)
  }

  // ------------------------------------------------------- stage 3

  /** lines, counting how many were PULLED: `Writer.uncons` applies the
   * continuation as it hands the element over, so the count after a
   * tell is the count of elements a consumer has taken */
  private def lines(xs: List[String], told: () => Unit): Unit ! Writer % String =
    xs.foldRight(okay.pure[Writer % String, Unit](()))((l, rest) => Writer.tell(l).flatMap(_ => { told(); rest }))

  /** a header parser: tell each `k: v` as (k, v), answer the count at the first blank line */
  private val header: Stage[String, (String, String), Either[Int, Int]] =
    Stage.transduceUntil[String, (String, String), Int, Either[Int, Int]](0)((n, line) =>
      if line.isEmpty then okay.pure(Right(Right(n)))
      else
        val Array(k, v) = line.split(": ", 2)
        Stage.tell[String, (String, String)]((k.trim, v.trim)).map(_ => Left(n + 1)),
      n => Left(n))   // Left: the input ended before a blank line

  test("transduceUntil stops the upstream: pulled up to the blank line, not one tell more") {
    var told = 0
    val doc = List("host: a", "port: 1", "", "body 1", "body 2", "body 3")
    val (out, answer) = !.run(Writer.run(through(lines(doc, () => told += 1))(header)))
    assertEquals(out, Seq(("host", "a"), ("port", "1")))
    assertEquals(answer, Right(2))
    assertEquals(told, 3, "host, port and the blank line were pulled; body 1 never was")
    // the instrument's control: a stage that never stops pulls all six
    told = 0
    val all = !.run(Writer.run(through(lines(doc, () => told += 1))(Stage.id[String])))
    assertEquals(all._1.size, 6)
    assertEquals(told, 6)
  }

  test("transduceUntil ends honestly when the input ends first") {
    val (out, answer) = !.run(Writer.run(through(lines(List("host: a"), () => ()))(header)))
    assertEquals(out, Seq(("host", "a")))
    assertEquals(answer, Left(1))
  }

  test("transduceUntil composes under through on both sides") {
    val upper: Stage[String, String, Unit] = Stage.transduce(())((_, l) => Stage.tell[String, String](l.toUpperCase), okay.pure)
    val keys: Stage[(String, String), String, Unit] = Stage.transduce(())((_, kv) => Stage.tell[(String, String), String](kv._1), okay.pure)
    val doc = List("host: a", "port: 1", "", "body")
    val (out, answer) = !.run(Writer.run(through(through(through(lines(doc, () => ()))(upper))(header))(keys)))
    assertEquals(out, Seq("HOST", "PORT"))
    assertEquals(answer, ())
  }

  test("transduce is transduceUntil with a step that never answers Right") {
    val step = (sum: Int, i: Int) => Stage.tell[Int, Int](sum + i).map(_ => sum + i)
    def told: Unit ! Writer % Int =
      (1 to 5).foldRight(okay.pure[Writer % Int, Unit](()))((i, r) => Writer.tell(i).flatMap(_ => r))
    val a = !.run(Writer.run(through(told)(Stage.transduce(0)(step, okay.pure))))
    val b = !.run(Writer.run(through(told)(
      Stage.transduceUntil[Int, Int, Int, Int](0)((s, i) => step(s, i).map(Left(_)), identity))))
    assertEquals(a, b)
    assertEquals(a, (Seq(1, 3, 6, 10, 15), 15))
  }

  test("pipe(producer)(Take.foldUntil) is Writer.foldUntil by the coroutine road, and stops the producer") {
    var told = 0
    def nums(n: Int): Unit ! Writer % Int =
      (1 to n).foldRight(okay.pure[Writer % Int, Unit](()))((i, r) => Writer.tell(i).flatMap(_ => { told += 1; r }))
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit =
      told = 0
      val viaPipe = pipe(nums(50))(Take.foldUntil[Int, S, X](using fo))
      val pulled = told
      assertEquals(viaPipe, nums(50).foldUntil(using fo), name)
      told = 0
      val _ = nums(50).foldUntil(using fo)
      assertEquals(pulled, told, s"$name: both roads pull the same number of elements")
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.find[Int](_ == 7), "find")
    check(FoldUntil.exists[Int](_ > 100), "exists-none")
    check(FoldUntil.headOption, "headOption")
  }

  test("Writer.foldUntil is tail-recursive across tells: 100 000 elements, the stop never firing") {
    val n = 100_000
    assertEquals(Source.range(0, n).runFoldUntil(using FoldUntil.exists[Long](_ < 0)).runWith, false)
    assertEquals(Chunks.foldUntil(Chunks.range(0, n))(using FoldUntil.exists[Long](_ < 0)), false)
  }
