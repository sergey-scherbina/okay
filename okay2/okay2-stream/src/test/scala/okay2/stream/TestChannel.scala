package okay2.stream

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import okay2._
import okay2.async._
import okay2.platform._
import okay2.Stream.StreamOps
import okay2.stream.Source.{SourceOps, ChunkSourceOps, ChunksMergeOps}
import okay2.stream.Channel.ChannelOps

/** Channels: the concurrency primitive of streams — merge and buffer. */
class TestChannel extends munit.FunSuite {

  private def wait1[A](f: scala.concurrent.Future[A]): A = Await.result(f, Duration(1, "s"))

  test("a channel is a linear async stream: send, close, drain") {
    val c = Channel[Int]()
    assert(c.offer(1)); assert(c.offer(2)); c.close()
    assertEquals(c.toLazyList.toList, List(1, 2))
    assertEquals(c.receiveBlocking(), None)
  }

  test("send after close is refused, not thrown: false, the element dropped, the end unchanged") {
    val c = Channel[Int]()
    assert(c.offer(1)); c.close()
    assert(!c.offer(2), "a closed channel took an element")
    assert(!c.sendBlocking(2), "a closed channel took an element")
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(c.receiveBlocking(), None)
    assert(!c.offer(3))
    assertEquals(c.receiveBlocking(), None)
  }

  test("a waiting receiver holds no thread: a thousand parked receives, freed by offers") {
    val chans = Vector.fill(1000)(Channel[Int]())
    val futs = chans.map(c => Async.runAsync(c.receive))
    assert(futs.forall(!_.isCompleted))
    chans.zipWithIndex.foreach { case (c, i) => assert(c.offer(i)) }
    futs.zipWithIndex.foreach { case (f, i) => assertEquals(wait1(f), Some(i)) }
  }

  test("a bounded send suspends as a program, and resumes when the consumer takes") {
    val c = Channel[Int](capacity = 1)
    assertEquals(wait1(Async.runAsync(c.send(1))), true)
    val second = Async.runAsync(c.send(2))
    Thread.sleep(20)
    assert(!second.isCompleted, "a send into a full channel completed without room")
    assertEquals(c.receiveBlocking(), Some(1))
    assertEquals(wait1(second), true)
    assertEquals(c.receiveBlocking(), Some(2))
  }

  test("close wakes a parked receiver at once; a parked sender's element was accepted and drains") {
    val c = Channel[Int](capacity = 1)
    val waiting = Async.runAsync(c.receive)
    Thread.sleep(20)
    assert(!waiting.isCompleted)
    c.close()
    assertEquals(wait1(waiting), None)
    val d = Channel[Int](capacity = 1)
    assert(d.offer(1))
    val parked = Async.runAsync(d.send(2))
    Thread.sleep(20)
    d.close()
    assertEquals(wait1(parked), true)
    assertEquals(d.receiveBlocking(), Some(1))
    assertEquals(d.receiveBlocking(), Some(2))
    assertEquals(d.receiveBlocking(), None)
  }

  test("many producers, many consumers, one bounded channel: every element exactly once (CAS)") {
    val c = Channel[Int](capacity = 16)
    val received = new java.util.concurrent.ConcurrentLinkedQueue[Int]()
    val producers = (0 until 8).map { p =>
      Threads.spawnThread(s"p$p")(() => for (i <- 0 until 1000) assert(c.sendBlocking(p * 1000 + i)))
    }
    val consumers = (0 until 4).map { i =>
      Threads.spawnThread(s"c$i") { () =>
        var go = true
        while (go) c.receiveBlocking() match {
          case Some(v) => val _ = received.add(v)
          case None => go = false
        }
      }
    }
    producers.foreach(_.join())
    c.close()
    consumers.foreach(_.join())
    val got = received.asScala.toList
    assertEquals(got.size, 8000)
    assertEquals(got.toSet, (0 until 8000).toSet)
  }

  test("the send/close race is exact: every accepted send is received, every refused one is not") {
    for (round <- 1 to 100) {
      val c = Channel[Int](capacity = 4)
      val accepted = java.util.concurrent.ConcurrentHashMap.newKeySet[Int]()
      val received = scala.collection.mutable.ArrayBuffer.empty[Int]
      val producer = Threads.spawnThread("producer") { () =>
        var i = 0
        var on = true
        while (on && i < 10000) {
          if (c.sendBlocking(i)) { val _ = accepted.add(i) } else on = false
          i += 1
        }
      }
      val consumer = Threads.spawnThread("consumer") { () =>
        var go = true
        while (go) c.receiveBlocking() match {
          case Some(v) => received += v
          case None => go = false
        }
      }
      Thread.sleep(0, (round % 7) * 100000)
      c.close()
      producer.join(); consumer.join()
      assertEquals(received.toSet, accepted.asScala.toSet, s"round $round")
      assertEquals(received.toList, received.toList.sorted, s"round $round: order")
    }
  }

  test("law: close does not discard what is already buffered; the batched read agrees with the single one") {
    val c = Channel[Int](1024)
    (1 to 300).foreach(x => { val _ = c.offer(x) })
    c.close()
    val out = Iterator.continually(c.receiveBlocking()).takeWhile(_.isDefined).flatten.toList
    assertEquals(out, (1 to 300).toList)
    def fill = { val d = Channel[Int](Int.MaxValue); (1 to 200).foreach(i => { val _ = d.offer(i) }); d.close(); d }
    assertEquals(fill.drained.toLazyList.toList, (1 to 200).toList)
    assertEquals(Writer.of[Channel, Async, Int](fill).toLazyList.toList, (1 to 200).toList)
    for (n <- List(0, 1, 63, 64, 65, 127, 128, 129)) {
      val d = Channel[Int](Int.MaxValue)
      (1 to n).foreach(i => { val _ = d.offer(i) })
      d.close()
      assertEquals(d.drained.toLazyList.toList, (1 to n).toList, s"n=$n")
    }
  }

  test("a failing producer's elements still arrive before the failure is seen") {
    val c = Channel[Int](Int.MaxValue)
    (1 to 10).foreach(i => { val _ = c.offer(i) })
    c.fail(new RuntimeException("boom"))
    c.close()
    val got = scala.util.Try(c.drained.toLazyList.toList)
    got match {
      case scala.util.Success(xs) => assertEquals(xs, (1 to 10).toList)
      case scala.util.Failure(e) => assert(e.getMessage.contains("boom"), e.toString)
    }
  }

  test("merge combines two async streams by readiness; each side keeps its own order") {
    def ticks[A](xs: List[A]): Source[A] =
      xs.foldRight(pure[Writer[A] + Async, Unit](()))((x, rest) =>
        Async(Thread.sleep(1)).at[Writer[A] + Async].flatMap(_ => Writer.tell(x).at[Writer[A] + Async]).flatMap(_ => rest))
    val merged: Source[Any] = ticks(List(1, 3, 5)).merge[Any](ticks(List("a", "b")))
    val got = merged.toLazyList.toList
    assertEquals(got.collect { case i: Int => i }, List(1, 3, 5))
    assertEquals(got.collect { case s: String => s }, List("a", "b"))
    // `either` keeps the side as data
    val tagged = ticks(List(1, 2)).either(ticks(List("x"))).toLazyList.toList
    assertEquals(tagged.collect { case Left(i) => i }, List(1, 2))
    assertEquals(tagged.collect { case Right(s) => s }, List("x"))
  }

  test("merge starts its fibers at the first pull, not before") {
    val pulled = new java.util.concurrent.atomic.AtomicInteger(0)
    def counted: Source[Int] =
      Async(pulled.incrementAndGet()).at[Writer[Int] + Async].flatMap(n => Writer.tell(n).at[Writer[Int] + Async])
    val merged = counted.merge(counted)
    Thread.sleep(20)
    assertEquals(pulled.get(), 0, "a source nobody consumes drained anyway")
    assertEquals(merged.toLazyList.toList.length, 2)
    assertEquals(pulled.get(), 2)
  }

  test("merge is CONCURRENT: eight sources overlap, chained or not") {
    def slow[A](ms: Long)(as: List[A]): Source[A] =
      as.foldRight(pure[Writer[A] + Async, Unit](()))((a, rest) =>
        Async(Thread.sleep(ms)).at[Writer[A] + Async].flatMap(_ => Writer.tell(a).at[Writer[A] + Async]).flatMap(_ => rest))
    val t0 = System.nanoTime()
    val eight = List.fill(8)(slow(100)(List(1, 2, 3)))
    assertEquals(eight.reduce((a, b) => a.merge(b)).toLazyList.length, 24)
    val t = (System.nanoTime() - t0) / 1000000
    assert(t < 1000, s"eight merged sources took ${t}ms — they did not overlap")
  }

  test("merge answers by READINESS: a silent source holds up nobody") {
    val silent: Source[String] =
      Async(Thread.sleep(300)).at[Writer[String] + Async].flatMap(_ => Writer.tell("late").at[Writer[String] + Async])
    val t0 = System.nanoTime()
    val first = Source.of((1 to 10).toList).merge[Any](silent.widen[Any]).toLazyList.take(10).toList
    val ms = (System.nanoTime() - t0) / 1000000
    assertEquals(first.collect { case i: Int => i }.length, 10)
    assert(ms < 200, s"the fast source waited ${ms}ms on the slow one")
  }

  test("merge is BOUNDED by default: an endless source does not run away") {
    def endless(produced: java.util.concurrent.atomic.AtomicInteger): Source[Int] =
      Async(produced.incrementAndGet()).at[Writer[Int] + Async]
        .flatMap(n => Writer.tell(n).at[Writer[Int] + Async])
        .flatMap(_ => endless(produced))
    val p = new java.util.concurrent.atomic.AtomicInteger(0)
    assertEquals(endless(p).merge(Source(0)).toLazyList.take(10).toList.length, 10)
    Thread.sleep(200)
    assert(p.get() < 1000, s"the producer ran ${p.get()} elements ahead of ten pulls")
  }

  test("buffer runs the producer ahead, at most capacity elements; bufferChunked in chunks") {
    val fibs = LazyList.unfold((0L, 1L)) { case (a, b) => Some((a, (b, a + b))) }.take(10)
    val c = Channel.buffer[Long, LazyList, Pure](2)(fibs)
    assertEquals(c.toLazyList.toList, fibs.toList)
    val list = (0L until 4000L).toList
    assertEquals(Channel.buffer[Long, List, Pure](1024)(list).drained.toLazyList.foldLeft(0L)(_ + _), list.sum)
    assertEquals(Channel.bufferChunked[Long, List, Pure](64, size = 256)(list).drained.toLazyList.foldLeft(0L)((a, ch) => a + ch.sum), list.sum)
  }

  test("a failed producer fails the consumer's program, not silently") {
    final case class Boom[A](n: Int)
    implicit val boomStream: Stream[Boom, Pure] = new Stream[Boom, Pure] {
      def uncons[A](b: Boom[A]): Option[(A, Boom[A])] ! Pure =
        if (b.n >= 3) throw new RuntimeException("the producer failed")
        else pure(Some(((b.n + 1).asInstanceOf[A], Boom[A](b.n + 1))))
    }
    val c = Channel.buffer[Int, Boom, Pure](8)(Boom[Int](0))
    def go(acc: List[Int]): List[Int] ! Async = Channel.stream.uncons(c).flatMap {
      case Some((a, _)) => go(a :: acc)
      case None => pure(acc.reverse)
    }
    val e = intercept[RuntimeException](Await.result(Async.runAsync(go(Nil)), Duration(5, "s")))
    assert(e.getMessage.contains("the producer failed"), e.getMessage)
    // and a healthy stream still ends cleanly, by callbacks alone
    val h = Channel.buffer[Int, LazyList, Pure](8)(LazyList(1, 2, 3))
    def all(acc: List[Int]): List[Int] ! Async = Channel.stream.uncons(h).flatMap {
      case Some((a, _)) => all(a :: acc)
      case None => pure(acc.reverse)
    }
    assertEquals(Await.result(Async.runAsync(all(Nil)), Duration(5, "s")), List(1, 2, 3))
  }

  test("the merge paths end, with a deadline: elementwise, chunked at three sizes, timed flush, Channel.merge") {
    val N = 2000L
    def l = Source.range(0L, N)
    def r = Source.range(N, 2L * N)
    val expect = (0L until 2L * N).sum
    def within[A](ms: Long, what: String)(body: => A): A = {
      var out: Option[A] = None
      val th = Threads.spawnThread(what)(() => { out = Some(body) })
      th.join(ms)
      assert(!th.isAlive, s"$what did not finish in ${ms} ms (livelock or hang)")
      out.get
    }
    assertEquals(within(10000, "elementwise")(l.merge(r).toLazyList.foldLeft(0L)(_ + _)), expect)
    for (k <- Seq(16, 256, 1024))
      assertEquals(within(10000, s"chunked k=$k")(l.merge(r, capacity = k, chunked = true).toLazyList.foldLeft(0L)(_ + _)), expect)
    assertEquals(within(10000, "flush")(l.merge(r, capacity = 1024, chunked = true, flushAfter = Some(1000)).toLazyList.foldLeft(0L)(_ + _)), expect)
    assertEquals(within(10000, "Channel.merge")(
      Channel.merge[Long, LazyList, Pure, LazyList, Pure](LazyList.range(0L, 500L), LazyList.range(500L, 1000L)).toLazyList.foldLeft(0L)(_ + _)), (0L until 1000L).sum)
  }

  test("chunked/unchunked leave no trace in the elements; a chunked merge is one queue operation per chunk") {
    val src: Source[Int] = Source.of(LazyList.range(0, 50))
    for (size <- List(1, 3, 16, 64))
      assertEquals(src.chunked(size).unchunked.toLazyList.toList, (0 until 50).toList, s"chunk size $size changed the elements")
    val merged = Chunks.range(0, 500).merge(Chunks.range(500, 1000))
    var sum = 0L
    var c = merged.receiveBlocking()
    while (c.isDefined) { sum += c.get.sum; c = merged.receiveBlocking() }
    assertEquals(sum, (0L until 1000L).sum)
    // a partial final chunk is flushed, not dropped; an empty source contributes nothing
    val out = Source.of((1 to 7).toList).merge(Source.of((8 to 12).toList), chunked = true).toLazyList.toList
    assertEquals(out.toSet, (1 to 12).toSet)
    assertEquals(Source.of(List.empty[Int]).merge(Source.of(List(1, 2, 3)), chunked = true).toLazyList.toList, List(1, 2, 3))
  }

  test("runCollect, runForeach, runFoldUntil and concat are programs, not parked values") {
    val s = Source.range(0L, 100L)
    assertEquals(s.runCollect.runWith, (0L until 100L).toVector)
    val seen = List.newBuilder[Long]
    s.runForeach(a => Async(seen += a).map(_ => ())).runWith
    assertEquals(seen.result(), (0L until 100L).toList)
    assertEquals(s.runFoldUntil(FoldUntil.take[Long](3)).runWith, Vector(0L, 1L, 2L))
    assertEquals(Source.concat(s.chunked(7)).runWith, (0L until 100L).toVector)
    assertEquals(Source.unfold(1)(i => if (i > 4) None else Some((i * i, i + 1))).runCollect.runWith, Vector(1, 4, 9, 16))
  }

  test("a channel bridges sent values into an Async stream by callbacks alone") {
    val c = Channel[Int]()
    assert(c.offer(1)); assert(c.offer(2)); c.close()
    assert(!c.offer(3))
    def drain(acc: List[Int]): List[Int] ! Async = Channel.stream.uncons(c).flatMap {
      case Some((a, _)) => drain(a :: acc)
      case None => pure(acc.reverse)
    }
    assertEquals(Await.result(Async.runAsync(drain(Nil)), Duration(1, "s")), List(1, 2))
  }
}
