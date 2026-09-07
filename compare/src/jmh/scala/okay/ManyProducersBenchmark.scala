package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import cats.effect.IO
import java.util.concurrent.TimeUnit

/**
 * The shape every earlier table in this repo left out: MANY
 * producers.
 *
 * Everything measured so far was one producer and one consumer, and
 * at that width a single ring's tail CAS is uncontended and a relaxed
 * buffer can only lose — it adds a part to pick and gains nothing.
 * The reason a partitioned buffer exists is the other end of the curve,
 * every producer fights for the same counter, and the only honest way
 * to say whether it earns its keep is to sweep the width.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(3)
class ManyProducersBenchmark {

  @Param(Array("1", "4", "16"))
  var producers: Int = 1

  final val Total = 8000
  final val Cap = 1024

  /**
   * The chunked consumer, and the one the zio lane's stream uses. The
   * first version of this benchmark read every okay lane with
   * `receiveBlocking` per element while `ZStream.fromQueue` pulled
   * arrays -- the fifth appearance in this repository of one
   * mismatch, and the first I introduced myself rather than found.
   */
  private def runChunked(c: Channel[Long]): Long =
    val per = Total / producers
    val ps = (0 until producers).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val done = Thread.ofVirtual().start { () => ps.foreach(_.join()); c.close() }
    val cb = summon[CanBlock]
    var sum = 0L
    var go = true
    while go do
      val chunk = cb.block[Either[Throwable, okay.Chunk[Long]]] { k =>
        c.receiveManyAsync(4096)(k); () => ()
      }.fold(throw _, identity)
      if chunk.length == 0 then go = false
      var i = 0
      while i < chunk.length do { sum += chunk(i); i += 1 }
    done.join()
    sum

  private def run(c: Channel[Long]): Long =
    val per = Total / producers
    val ps = (0 until producers).map(w => Thread.ofVirtual().start { () =>
      var i = 0L
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val done = Thread.ofVirtual().start { () =>
      ps.foreach(_.join()); c.close()
    }
    var sum = 0L
    var go = true
    while go do
      c.receiveBlocking() match
        case Some(v) => sum += v
        case None => go = false
    done.join()
    sum

  // ── the comparable lanes: a CHUNKED consumer, as zio's stream has ──

  /**
   * The CONSUMER axis, which this benchmark did not have (2026-09-07,
   * the operator asked). Producers get a part each; consumers share
   * the scan cursor, so the two sides do not scale for the same
   * reason and a producer sweep cannot answer for both. One consumer
   * is the shape above; `consumers` > 1 puts several readers on the
   * same channel and sums what each of them got.
   */
  @Param(Array("1", "4"))
  var consumers: Int = 1

  private def runManyConsumers(c: Channel[Long]): Long =
    val per = Total / producers
    val ps = (0 until producers).map(w => Thread.ofVirtual().start { () =>
      var i = 0
      while i < per do { val _ = c.sendBlocking(w.toLong * per + i); i += 1 }
    })
    val done = Thread.ofVirtual().start { () => ps.foreach(_.join()); c.close() }
    val sums = java.util.concurrent.atomic.AtomicLong()
    val cs = (0 until consumers).map(_ => Thread.ofVirtual().start { () =>
      var mine = 0L
      var go = true
      while go do
        c.receiveBlocking() match
          case Some(v) => mine += v
          case None => go = false
      val _ = sums.addAndGet(mine)
    })
    cs.foreach(_.join())
    done.join()
    sums.get

  @Benchmark def ringManyConsumers(): Long =
    runManyConsumers(Queues.strong[Long].bounded(Cap).build)

  @Benchmark def adaptiveManyConsumers(): Long =
    runManyConsumers(Queues.strong[Long].adaptive.each(Cap / 8 max 8).build)

  @Benchmark def oneRing_chunk(): Long =
    runChunked(Queues.strong[Long].bounded(Cap).build)

  /** DIAGNOSTIC (solo-part, 2026-09-07): a buffer that does NOTHING
   * but forward to a ring. If this costs what the partitioned buffer
   * costs at one producer, then the price is the extra layer of call
   * — a channel calling `Buffer` calling `Ring` — and no amount of
   * bookkeeping removed from the adaptive buffer will close it. */
  private final class Forwarding[A](to: Buffer[A]) extends Buffer[A]:
    def capacity: Int = to.capacity
    def push(a: A): Boolean = to.push(a)
    def pushDeciding(a: A, unless: java.util.concurrent.atomic.AtomicBoolean, orElse: A): A | Null =
      to.pushDeciding(a, unless, orElse)
    def pop(): A | Null = to.pop()
    def popMany(max: Int)(sink: A => Unit): Int = to.popMany(max)(sink)
    override def pushMany(n: Int)(src: Int => A): Int = to.pushMany(n)(src)
    def size: Int = to.size
    def isEmpty: Boolean = to.isEmpty
    def hasReady: Boolean = to.hasReady
    override def hasRoom: Boolean = to.hasRoom

  @Benchmark def forwarded_chunk(): Long =
    runChunked(Queues.strong[Long].on([T] => (_: Int) => Forwarding[T](Ring[T](Cap))).build)

  @Benchmark def oneUnbounded_chunk(): Long =
    runChunked(Queues.strong[Long].unbounded.build)

  @Benchmark def relaxed_chunk(): Long =
    runChunked(Queues.strong[Long].relaxed.parts(producers).each(Cap / producers max 8).build)

  @Benchmark def relaxedUnbounded_chunk(): Long =
    runChunked(Queues.strong[Long].relaxed.parts(producers).unbounded.build)

  /**
   * The whole point of this benchmark, now that the table above shows
   * the answer depends on the producer count: a buffer that decides
   * for itself. It must be no worse than one ring at a single
   * producer and close to the relaxed lane at sixteen, or it is not
   * worth having.
   */
  /** DIAGNOSTIC (adaptive-one-producer, 2026-09-07): the partitioned
   * buffer with ONE part and no room to grow. If this costs what the
   * growable one costs, the price is the layer's fixed work; if it
   * costs what a plain ring costs, the price is the growth machinery
   * (the claim, the open count, the scan) and only appears because
   * the buffer is prepared to adapt. */
  @Benchmark def onePart_chunk(): Long =
    runChunked(Queues.strong[Long].adaptive.parts(1).each(Cap).build)

  /** the one that decides for itself: a ring while one producer keeps
   * up, a partitioned buffer once producers contend (queue-swap) */
  @Benchmark def growing_chunk(): Long =
    runChunked(Queues.strong[Long].growing(Cap, parts = 16).build)

  @Benchmark def adaptive_chunk(): Long =
    runChunked(Queues.strong[Long].adaptive.parts(16).each(Cap).build)

  @Benchmark def adaptiveUnbounded_chunk(): Long =
    runChunked(Queues.strong[Long].adaptive.parts(16).unbounded.build)

  // ── DIAGNOSTIC: the same, read one element at a time. Kept because
  //    what it measures is real, but zio has no per-element read of a
  //    queue to put beside it ────────────────────────────────────────

  /** one ring: every producer contends for one tail */
  @Benchmark def oneRing_elem(): Long = run(Queues.strong[Long].bounded(Cap).build)

  /** parts as wide as the producers: contention only when two land on
   * the same part */
  @Benchmark def relaxed_elem(): Long =
    run(Queues.strong[Long].relaxed.parts(producers).each(Cap / producers max 8).build)

  /**
   * The same relaxation over parts that GROW. The hypothesis this
   * lane exists to test: what breaks `relaxed` above is not the
   * relaxation but its combination with boundedness. The channel's
   * queue of waiting senders is GLOBAL while the resource is PER
   * PART, so a freed slot in one part wakes an arbitrary sender, who
   * usually finds its own part still full and parks again. With parts
   * that never fill, no sender ever parks and the mismatch cannot
   * arise.
   */
  @Benchmark def relaxedUnbounded_elem(): Long =
    run(Queues.strong[Long].relaxed.parts(producers).unbounded.build)

  /** and one growable buffer, so the comparison is like for like */
  @Benchmark def oneUnbounded_elem(): Long = run(Queues.strong[Long].unbounded.build)

  /**
   * cats-effect + fs2, the third comparison: a bounded queue with a
   * None sentinel — which is how fs2 spells a terminated queue — read
   * by `fromQueueNoneTerminated`, whose `limit` makes it take a chunk
   * at a time rather than an element. Same shape as the zio lane, and
   * the same granularity as the okay `_chunk` lanes.
   */
  @Benchmark
  def catsQueue_chunk(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val per = Total / producers
    // the consumer must run CONCURRENTLY with the producers: joining
    // them first deadlocks on a bounded queue with nobody draining,
    // which is what the first version of this lane did to itself
    val prog = for
      q <- cats.effect.std.Queue.bounded[IO, Option[Long]](Cap)
      // a Stream, NOT foldLeft over IO: the fold builds a left-nested
      // chain of `per` effects before anything runs, which is a shape
      // this repository has already been burned by once (see the kyo
      // note in benchmarks.md) and would price cats unfairly
      fs <- (0 until producers).toList.traverseFibers(w =>
              fs2.Stream.range(0L, per.toLong)
                .evalMap(i => q.offer(Some(w.toLong * per + i)))
                .compile.drain)
      _ <- (fs.traverse_(_.join) *> q.offer(None)).start
      s <- fs2.Stream.fromQueueNoneTerminated(q, limit = 4096)
             .compile.fold(0L)(_ + _)
    yield s
    prog.unsafeRunSync()

  extension [A](xs: List[A])
    private def traverseFibers(f: A => IO[Unit]): IO[List[cats.effect.FiberIO[Unit]]] =
      xs.foldLeft(IO.pure(List.empty[cats.effect.FiberIO[Unit]])) { (acc, a) =>
        acc.flatMap(l => f(a).start.map(_ :: l))
      }

  extension [A](xs: List[A])
    private def traverse_(f: A => IO[Any]): IO[Unit] =
      xs.foldLeft(IO.unit)((acc, a) => acc *> f(a).void)

  /** their queue, for scale */
  @Benchmark
  def zioQueue_chunk(): Long =
    import _root_.zio.*
    val per = Total / producers
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Option[Long]](Cap)
        fs <- ZIO.foreach(0 until producers)(w =>
                ZIO.foreachDiscard(0L until per.toLong)(i => q.offer(Some(w.toLong * per + i))).fork)
        _ <- ZIO.foreachDiscard(fs)(_.join).flatMap(_ => q.offer(None)).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).collectWhileSome.runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())
}
