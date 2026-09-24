package scala2probe

import okay.scala2._

/** okay.scala2 fibers and channels from Scala 2.13
 * (specs/scala2-facade.md, stage 5) */
class TestFibersChannelsFromScala2 extends munit.FunSuite {

  test("a forked fiber runs concurrently; join answers, joinEither reports a failure") {
    val latch = new java.util.concurrent.CountDownLatch(1)
    val prog = for {
      f <- Async.fork(Async { latch.await(); 42 })
      _ <- Async(latch.countDown())
      n <- f.join
    } yield n
    assertEquals(prog.runWith, 42)

    val boom = new IllegalStateException("boom")
    val failed = for {
      f <- Async.fork(Async[Int](throw boom))
      r <- f.joinEither
    } yield r
    assertEquals(failed.runWith, Left(boom))
  }

  test("par answers both, race the faster, timeout None past its deadline") {
    assertEquals(Async.par(Async(1), Async("a")).runWith, (1, "a"))
    val slow = Async.sleep(5000).map(_ => "slow")
    assertEquals(Async.race(slow, Async("fast")).runWith, "fast")
    assertEquals(Async.timeout(20)(slow).runWith, None)
    assertEquals(Async.timeout(5000)(Async(7)).runWith, Some(7))
  }

  test("producer and consumer over a bounded channel: every element once, in order") {
    val ch = Channel[Int](4)
    def produce(i: Int): Unit ! Async =
      if (i > 1000) Async(ch.close())
      else ch.send(i).flatMap(_ => produce(i + 1))
    def consume(acc: Vector[Int]): Vector[Int] ! Async =
      ch.receive.flatMap {
        case Some(n) => consume(acc :+ n)
        case None => pure(acc)
      }
    val prog = for {
      p <- Async.fork(produce(1))
      got <- consume(Vector.empty)
      _ <- p.join
    } yield got
    assertEquals(prog.runWith, (1 to 1000).toVector)
  }

  test("offer does not wait: false once the channel is full") {
    val ch = Channel[Int](2)
    assert(ch.offer(1))
    assert(ch.offer(2))
    assert(!ch.offer(3))
    ch.close()
    assert(ch.isClosed)
  }

  test("source drains a channel into a stream that ends on close") {
    val ch = Channel[String](16)
    assert(ch.offer("a"))
    assert(ch.offer("b"))
    ch.close()
    assertEquals(ch.source.map(_.toUpperCase).runCollect.runWith, Vector("A", "B"))
  }

  test("cancel stops a fiber that is waiting") {
    val prog = for {
      f <- Async.fork(Async.sleep(60000).map(_ => 1))
      _ <- f.cancel
      r <- f.joinEither
    } yield r
    assert(prog.runWith.isLeft)
  }
}
