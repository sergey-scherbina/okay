package scala2probe

import okay.scala2._

/** okay.scala2.Source from Scala 2.13 (specs/scala2-facade.md, stage 4) */
class TestSourceFromScala2 extends munit.FunSuite {

  def collect[A](s: Source[A]): Vector[A] = Eff.runAsync(s.runCollect)

  test("constructors and transformations") {
    assertEquals(collect(Source(1, 2, 3).map(_ * 10)), Vector(10, 20, 30))
    assertEquals(collect(Source.fromIterable(1 to 10).filter(_ % 2 == 0)), Vector(2, 4, 6, 8, 10))
    assertEquals(collect(Source.range(0, 5).take(3)), Vector(0L, 1L, 2L))
    assertEquals(collect(Source.range(0, 10).takeWhile(_ < 4)), Vector(0L, 1L, 2L, 3L))
    assertEquals(collect(Source.range(0, 5).drop(3)), Vector(3L, 4L))
    assertEquals(collect(Source("a", "b").zipWithIndex), Vector(("a", 0L), ("b", 1L)))
    assertEquals(collect(Source(1, 2) ++ Source(3)), Vector(1, 2, 3))
    assertEquals(collect(Source(1, 2).mapConcat(n => List.fill(n)(n))), Vector(1, 2, 2))
    assertEquals(collect(Source.empty[Int]), Vector.empty[Int])
  }

  test("take on an infinite source ends") {
    val nats = Source.unfold(0)(n => Some((n, n + 1)))
    assertEquals(collect(nats.map(_ * 2).take(4)), Vector(0, 2, 4, 6))
  }

  test("a source written as an Eff that tells: nothing runs until it is run") {
    var reads = 0
    def read(): Eff[Async, Int] = Async.delay { reads += 1; reads }
    val lines: Eff[Writer[String] + Async, Unit] = for {
      a <- read()
      _ <- Writer.tell("line " + a)
      b <- read()
      _ <- Writer.tell("line " + b)
    } yield ()
    val src = Source.fromEff(lines).map(_.toUpperCase)
    assertEquals(reads, 0)
    assertEquals(collect(src), Vector("LINE 1", "LINE 2"))
    assertEquals(Eff.runAsync(Writer.run(src.toEff)), (Vector("LINE 3", "LINE 4"), ()))
  }

  test("runForeach and runFold see every element in order") {
    val seen = scala.collection.mutable.ListBuffer.empty[Int]
    Eff.runAsync(Source(3, 1, 2).runForeach(n => Async.delay { seen += n; () }))
    assertEquals(seen.toList, List(3, 1, 2))
    assertEquals(Eff.runAsync(Source(3, 1, 2).runFold("")(_ + _)), "312")
  }

  test("merge delivers both sources, whatever the interleaving") {
    val merged = Source.range(0, 100).merge(Source.range(100, 200))
    assertEquals(collect(merged).sorted, (0L until 200L).toVector)
  }

  test("100 000 elements through map, filter and runFold") {
    val n = Eff.runAsync(Source.range(0, 100000).map(_ + 1).filter(_ % 2 == 0).runFold(0L)((c, _) => c + 1))
    assertEquals(n, 50000L)
  }
}
