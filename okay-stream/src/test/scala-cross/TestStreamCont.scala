package okay

import okay.StreamCont.{Src, range}

/** the machine's stream twin (stream-twin): one source, the JVM, JS and Native suites */
class TestStreamCont extends munit.FunSuite {

  given scala.concurrent.ExecutionContext = munitExecutionContext

  /** the synchronous ones finish inline: no Await is ever pending */
  def now[A](p: A ! StreamCont.R): A = AsyncCont.runAsync(p).value match
    case Some(t) => t.get
    case None => fail("did not complete synchronously")

  test("map, filter, take over a range") {
    assertEquals(now(range(0, 100).map(_ * 2).filter(_ % 3 == 0).take(4).toVector), Vector(0L, 6L, 12L, 18L))
  }

  test("take does not pull past the elements it keeps") {
    var pulled = 0
    val s = StreamCont.fromIterator(Iterator.from(0).map { i => pulled += 1; i })
    assertEquals(now(s.take(3).toVector), Vector(0, 1, 2))
    assertEquals(pulled, 3)
  }

  test("++ and flatMap, in order") {
    val s = StreamCont(1, 2) ++ StreamCont(3)
    assertEquals(now(s.flatMap(i => StreamCont(i, i * 10)).toVector), Vector(1, 10, 2, 20, 3, 30))
  }

  test("a million elements, most filtered out, in constant stack") {
    assertEquals(now(range(0, 1000000).filter(_ % 100000 == 0).toVector).size, 10)
  }

  test("evalMap: an Async step per element") {
    val s = StreamCont(1, 2, 3).evalMap(i => AsyncCont.async(i + 1).at)
    assertEquals(now(s.toVector), Vector(2, 3, 4))
  }

  test("merge: every element of both, each side in its own order") {
    val a = range(0, 50)
    val b = range(100, 150).evalMap(i => AsyncCont.sleep(0).map(_ => i))
    AsyncCont.runAsync(a.merge(b).toVector).map { v =>
      assertEquals(v.sorted, (0L until 50L).toVector ++ (100L until 150L).toVector)
      assertEquals(v.filter(_ < 100), (0L until 50L).toVector)
      assertEquals(v.filter(_ >= 100), (100L until 150L).toVector)
    }
  }

  test("the bridges: a classic Source in, the machine's stream out, and back") {
    val in: Src[Long] = StreamCont.fromSource(Source.range(0, 5))
    assertEquals(now(in.map(_ + 1).toVector), Vector(1L, 2L, 3L, 4L, 5L))
    val back: Source[Long] = StreamCont.toSource(range(0, 5).map(_ * 2))
    Async.runAsync(back.runCollect).map(v => assertEquals(v, Vector(0L, 2L, 4L, 6L, 8L)))
  }
}
