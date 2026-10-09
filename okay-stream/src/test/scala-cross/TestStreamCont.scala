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

  test("a consumer that stops first: merge cancels the pull still pending on the other side") {
    // a fiber's cancel reaches it on its own thread of control (Native: an OS thread), so the cancellation is
    // AWAITED, not read the moment the consumer's program ends; never cancelled is munit's timeout
    val cancelled = scala.concurrent.Promise[Unit]()
    val never: Src[Int] = StreamCont.eval(AsyncCont.awaitEither[Int](_ => () => cancelled.trySuccess(()): Unit).at)
    val prog = StreamCont(1, 2, 3).merge(never).take(2).toVector
    AsyncCont.runAsync(prog).flatMap(v => cancelled.future.map(_ => assertEquals(v.size, 2)))
  }

  test("channels: send and receive as operations; a channel read as a stream; a stream pumped into a channel") {
    val c = Channel[Int](4)
    val prog: Vector[Int] ! StreamCont.R = for
      _ <- StreamCont.send(c, 1)
      _ <- StreamCont.send(c, 2)
      x <- StreamCont.receive(c)
      _ <- AsyncCont.async(c.close())
      rest <- StreamCont.fromChannel(c).toVector
    yield x.toVector ++ rest
    assertEquals(now(prog), Vector(1, 2))
    val d = Channel[Long](8)
    assertEquals(now(StreamCont.range(0, 5).toChannel(d).flatMap(_ => StreamCont.fromChannel(d).toVector)), Vector(0L, 1L, 2L, 3L, 4L))
  }

  test("buffer: a consumer that stops first cancels the pump") {
    // the producer gives three elements, then parks on an Await whose canceller says so: a cancelled pump
    // unregisters it (awaited — a fiber's cancel reaches it on its own thread of control)
    val cancelled = scala.concurrent.Promise[Unit]()
    val parked: Src[Long] = StreamCont.eval(AsyncCont.awaitEither[Long](_ => () => cancelled.trySuccess(()): Unit).at)
    val prog = (StreamCont(0L, 1L, 2L) ++ parked).buffer(8).take(3).toVector
    AsyncCont.runAsync(prog).flatMap(v => cancelled.future.map(_ => assertEquals(v, Vector(0L, 1L, 2L))))
  }
}

