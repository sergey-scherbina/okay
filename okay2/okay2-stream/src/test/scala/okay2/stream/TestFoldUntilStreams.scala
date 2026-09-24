package okay2.stream

import scala.collection.immutable.ArraySeq
import okay2._
import okay2.Later.later
import okay2.Stream.{FeedOps, FeedInOps}
import okay2.stream.Pipe.{into, pipe}

/** the stopping fold on the stream carriers: `Chunks.foldUntil`,
 * `Writer.foldUntil`, the coroutine road — they agree with the pure
 * road, and each STOPS */
class TestFoldUntilStreams extends munit.FunSuite {

  type R = Writer[Int] + Later

  /** 1, perform, 2, perform, ... — a G op after every tell */
  private def counted(n: Int, performed: () => Unit): Unit ! R = {
    def go(i: Int): Unit ! R =
      if (i > n) pure(())
      else Writer.tell(i).at[R].flatMap(_ => later { performed() }.at[R]).flatMap(_ => go(i + 1))
    pure[R, Unit](()).flatMap(_ => go(1))
  }

  private def chunked(xs: List[Int], size: Int): Chunks[Int] = {
    def go(rest: List[Int]): Chunks[Int] =
      if (rest.isEmpty) pure(())
      else Writer.tell(ArraySeq.from(rest.take(size)): Chunk[Int]).flatMap(_ => go(rest.drop(size)))
    go(xs)
  }

  test("the carriers agree with the pure road on every instance") {
    val xs = (1 to 20).toList
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit = {
      val expected = Stream.foldUntil(xs)(fo)
      assertEquals(Chunks.foldUntil(chunked(xs, 4))(fo), expected, s"Chunks $name")
      assertEquals(Writer.foldUntil[Int, S, Unit, X, R](counted(20, () => ()))(fo).runWith, expected, s"Writer $name")
      assertEquals(counted(20, () => ()).foldUntil(fo), expected, s"FeedInOps $name")
    }
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.take(100), "take(100)")
    check(FoldUntil.find[Int](_ > 7), "find")
    check(FoldUntil.find[Int](_ > 70), "find-none")
    check(FoldUntil.exists[Int](_ == 11), "exists")
    check(FoldUntil.forall[Int](_ < 5), "forall")
    check(FoldUntil.headOption, "headOption")
    check(FoldUntil.until[Int, Int, Int](0)((s, a) => if (s + a > 30) Right(s) else Left(s + a))(identity), "until")
  }

  test("Chunks.foldUntil's unboxed arms agree with the generic one and stop at the chunk") {
    var pulls = 0
    val chunked = Chunks.generateWith(0) { i => pulls += 1; (ArraySeq.range(i, i + 4): Chunk[Int], i + 4) }
    val sumL = FoldUntil.long[Int, Long](0L)((s, a) => s + a)(_ > 50)(identity)
    assertEquals(Chunks.foldUntil(chunked)(sumL), 55L)
    assertEquals(pulls, 3)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil[Int, Long, Long](0L)((s, a) => s + a)(_ > 50)(identity)), 55L)
    assertEquals(pulls, 3)
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.int[Int, Int](0)((s, a) => s + a)(_ > 50)(identity)), 55)
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.double[Int, Double](0.0)((s, a) => s + a)(_ > 50)(identity)), 55.0)
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.exists[Int](_ == 9)), true)
  }

  test("Chunks.foldUntil pulls no chunk after the one that satisfied it") {
    var pulls = 0
    val chunked = Chunks.generateWith(0) { i => pulls += 1; (ArraySeq.range(i, i + 4): Chunk[Int], i + 4) }
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.take[Int](3)), Vector(0, 1, 2))
    assertEquals(pulls, 1)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.find[Int](_ == 5)), Some(5))
    assertEquals(pulls, 2)
    pulls = 0
    assertEquals(Chunks.foldUntil(chunked)(FoldUntil.take[Int](0)), Vector.empty[Int])
    assertEquals(pulls, 0)
  }

  test("the effectful writer's foldUntil performs the op before the stop and not the one after it") {
    var performed = 0
    assertEquals(counted(1000, () => performed += 1).foldUntil(FoldUntil.take[Int](3)), Vector(1, 2, 3))
    assertEquals(performed, 2)
    performed = 0
    assertEquals(counted(1000, () => performed += 1).foldUntil(FoldUntil.take[Int](0)), Vector.empty[Int])
    assertEquals(performed, 0)
    performed = 0
    assertEquals(counted(5, () => performed += 1).foldUntil(FoldUntil.take[Int](100)), Vector(1, 2, 3, 4, 5))
    assertEquals(performed, 5)
  }

  private def lines(xs: List[String], told: () => Unit): Unit ! Writer[String] =
    xs.foldRight(pure[Writer[String], Unit](()))((l, rest) => Writer.tell(l).flatMap(_ => { told(); rest }))

  /** a header parser: tell each `k: v` as (k, v), answer the count at the first blank line */
  private val header: Stage[String, (String, String), Either[Int, Int]] =
    Stage.transduceUntil[String, (String, String), Int, Either[Int, Int]](0)((n, line) =>
      if (line.isEmpty) pure(Right(Right(n)))
      else {
        val kv = line.split(": ", 2)
        Stage.tell[String, (String, String)]((kv(0).trim, kv(1).trim)).map(_ => Left(n + 1))
      },
      n => Left(n))

  test("transduceUntil stops the upstream: pulled up to the blank line, not one tell more") {
    var told = 0
    val doc = List("host: a", "port: 1", "", "body 1", "body 2", "body 3")
    val (out, answer) = Effects.run(Writer.run[(String, String), Either[Int, Int], Writer[(String, String)]](into(lines(doc, () => told += 1))(header)))
    assertEquals(out, Seq(("host", "a"), ("port", "1")))
    assertEquals(answer, Right(2))
    assertEquals(told, 3, "host, port and the blank line were pulled; body 1 never was")
    told = 0
    val all = Effects.run(Writer.run[String, Unit, Writer[String]](into(lines(doc, () => told += 1))(Stage.id[String])))
    assertEquals(all._1.size, 6)
    assertEquals(told, 6)
  }

  test("transduceUntil ends honestly when the input ends first") {
    val (out, answer) = Effects.run(Writer.run[(String, String), Either[Int, Int], Writer[(String, String)]](into(lines(List("host: a"), () => ()))(header)))
    assertEquals(out, Seq(("host", "a")))
    assertEquals(answer, Left(1))
  }

  test("pipe(producer)(Take.foldUntil) is Writer.foldUntil by the coroutine road, and stops the producer") {
    var told = 0
    def nums(n: Int): Unit ! Writer[Int] =
      (1 to n).foldRight(pure[Writer[Int], Unit](()))((i, r) => Writer.tell(i).flatMap(_ => { told += 1; r }))
    def check[S, X](fo: FoldUntil[Int, S, X], name: String): Unit = {
      told = 0
      val viaPipe = pipe(nums(50))(Take.foldUntil[Int, S, X](fo))
      val pulled = told
      assertEquals(viaPipe, nums(50).foldUntil(fo), name)
      told = 0
      val _ = nums(50).foldUntil(fo)
      assertEquals(pulled, told, s"$name: both roads pull the same number of elements")
    }
    check(FoldUntil.take(3), "take(3)")
    check(FoldUntil.take(0), "take(0)")
    check(FoldUntil.find[Int](_ == 7), "find")
    check(FoldUntil.exists[Int](_ > 100), "exists-none")
    check(FoldUntil.headOption, "headOption")
  }

  test("Writer.foldUntil is tail-recursive across tells: 100 000 elements, the stop never firing") {
    val n = 100000
    val p: Unit ! Writer[Long] = (0 until n).foldRight(pure[Writer[Long], Unit](()))((i, r) => Writer.tell(i.toLong).flatMap(_ => r))
    assertEquals(p.foldUntil(FoldUntil.exists[Long](_ < 0)), false)
    assertEquals(Chunks.foldUntil(Chunks.range(0, n))(FoldUntil.exists[Long](_ < 0)), false)
  }

  test("phased: the CSV shape — header typed into the body's state, rows keyed by it") {
    def csv: Stage[String, Vector[(String, String)], Either[Unit, Vector[String]]] =
      Stage.phased[String, Vector[(String, String)], Unit, Vector[String]](())(
        head = (_, line) => Right((line.split(',').toVector, Vector.empty)),
        body = (cols, line) => (cols, Vector(cols.zip(line.split(',').toVector))),
        endHead = _ => Vector.empty,
        endBody = _ => Vector.empty)
    val told: String ! Writer[String] = Seq("name,age", "ann,25", "bo,31").foldLeft(pure[Writer[String], String](""))((m, l) => m.flatMap(_ => Writer.tell(l).map(_ => l)))
    val (rows, answer) = Effects.run(Writer.run[Vector[(String, String)], Either[Unit, Vector[String]], Writer[Vector[(String, String)]]](into(told)(csv)))
    assertEquals(rows, Seq(Vector("name" -> "ann", "age" -> "25"), Vector("name" -> "bo", "age" -> "31")))
    assertEquals(answer, Right(Vector("name", "age")))
    // input ending DURING the head answers Left
    val empty: String ! Writer[String] = pure("")
    assertEquals(Effects.run(Writer.run[Vector[(String, String)], Either[Unit, Vector[String]], Writer[Vector[(String, String)]]](into(empty)(csv)))._2, Left(()))
  }
}
