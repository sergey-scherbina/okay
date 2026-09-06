package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * PROBE, not a table: what the competitor lanes pay that is not the
 * thing being measured.
 *
 * Two questions, both about whether this repository has been fair to
 * cats-effect and ZIO rather than to itself:
 *
 * 1. THE RUNTIME-ENTRY FLOOR. cats-effect 3.5.7's `unsafeRunSync`
 *    schedules the fiber onto the compute pool and blocks the caller
 *    on an ArrayBlockingQueue (IO.scala:1031, IOPlatform.scala:70) —
 *    two thread handoffs per benchmark invocation, paid by every
 *    cats lane in this project and by none of okay's, which run
 *    inline. ZIO's `unsafe.run` runs the fiber on the calling thread
 *    and only parks if it suspends. Nobody here has ever measured
 *    what that floor is, so nobody knows what share of "cats IO 140"
 *    is the handoff and what share is cats.
 *
 * 2. THE PURE-VS-DELAY PAIRING in the bind chain. `okayCont` binds
 *    `Cont.Pure(x + 1)`; `catsIO` binds `IO(x + 1)` — a Delay thunk,
 *    which is what a cats user types, but not the same node kind. The
 *    cats twin of `Pure` is `IO.pure`; ZIO's is `Exit.succeed`.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class FairnessProbeBenchmark {

  final val N = 10000

  // ── 1. the floor: an already-finished program, run ────────────────

  @Benchmark
  def floor_okay(): Int = pure[Nothing, Int](1).runWith

  @Benchmark
  def floor_catsIO(): Int =
    import cats.effect.unsafe.implicits.global
    cats.effect.IO.pure(1).unsafeRunSync()

  @Benchmark
  def floor_zio(): Int =
    import _root_.zio.*
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(ZIO.succeed(1)).getOrThrowFiberFailure())

  @Benchmark
  def floor_kyoEval(): Int =
    import _root_.kyo.*
    (1: Int < Any).eval

  @Benchmark
  def floor_kyoRunAndBlock(): Int =
    import _root_.kyo.*
    import AllowUnsafe.embrace.danger
    KyoApp.Unsafe.runAndBlock(Duration.Infinity)(1: Int < (Abort[Nothing] & Async)).getOrThrow

  // ── 2. the bind chain, with the node kind matched ─────────────────

  /** the lane as it is in CompareBenchmark: a Delay per bind */
  @Benchmark
  def chain_catsIO_delay(): Int =
    import cats.effect.unsafe.implicits.global
    (1 to N).foldLeft(cats.effect.IO.pure(0))((m, _) => m.flatMap(x => cats.effect.IO(x + 1)))
      .unsafeRunSync()

  /** the same chain with `IO.pure`, the twin of `Cont.Pure` */
  @Benchmark
  def chain_catsIO_pure(): Int =
    import cats.effect.unsafe.implicits.global
    (1 to N).foldLeft(cats.effect.IO.pure(0))((m, _) => m.flatMap(x => cats.effect.IO.pure(x + 1)))
      .unsafeRunSync()

  /** as in CompareBenchmark: `ZIO.succeed` is by-name, a Sync node */
  @Benchmark
  def chain_zio_succeed(): Int =
    import _root_.zio.*
    val z = (1 to N).foldLeft(ZIO.succeed(0): UIO[Int])((m, _) => m.flatMap(x => ZIO.succeed(x + 1)))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  /** the same with an already-evaluated value per bind */
  @Benchmark
  def chain_zio_exit(): Int =
    import _root_.zio.*
    val z = (1 to N).foldLeft(ZIO.succeed(0): UIO[Int])((m, _) => m.flatMap(x => Exit.succeed(x + 1)))
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  /** okay's lane from CompareBenchmark, same run, for the tie */
  @Benchmark
  def chain_okayCont(): Int =
    reset((1 to N).foldLeft(Cont.Pure(0): Int /> Int)((m, _) => m.flatMap(x => Cont.Pure(x + 1))))

  // ── 3. the queue consumer: Ref.update per element is not what a ───
  //    zio user writes to sum a stream, and it is not what okay's
  //    side pays (a plain var write)

  private val list: List[Long] = (0L until 4000L).toList
  private val Q = 4000

  private def runZio[A](z: _root_.zio.ZIO[Any, Any, A]): A =
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(z).getOrThrowFiberFailure())

  /** the spelling every zio queue lane in this project uses */
  @Benchmark
  def queue_zio_refUpdate(): Long =
    import _root_.zio.*
    runZio(for
      q <- Queue.bounded[Long](1024)
      _ <- ZIO.foreachDiscard(list)(q.offer).fork
      r <- Ref.make(0L)
      _ <- zio.stream.ZStream.fromQueue(q).take(Q.toLong).runForeach(x => r.update(_ + x))
      s <- r.get
    yield s)

  /** the exact mirror of okay's callback: a plain var, no CAS */
  @Benchmark
  def queue_zio_varSum(): Long =
    import _root_.zio.*
    var sum = 0L
    runZio(for
      q <- Queue.bounded[Long](1024)
      _ <- ZIO.foreachDiscard(list)(q.offer).fork
      _ <- zio.stream.ZStream.fromQueue(q).take(Q.toLong).runForeach(x => ZIO.succeed { sum += x })
    yield ())
    sum

  /** the idiomatic terminal: no callback at all */
  @Benchmark
  def queue_zio_runSum(): Long =
    import _root_.zio.*
    runZio(for
      q <- Queue.bounded[Long](1024)
      _ <- ZIO.foreachDiscard(list)(q.offer).fork
      s <- zio.stream.ZStream.fromQueue(q).take(Q.toLong).runSum
    yield s)

  /** okay's chunk-native lane, same run, for the tie */
  @Benchmark
  def queue_okay_chunkNative(): Long =
    var sum = 0L
    Channel.bufferChunked(64, size = 256)(list).drained.runForeach(ch =>
      okay.effect[Async, Unit](Async.Run(() =>
        var i = 0
        while i < ch.length do { sum += ch(i); i += 1 }))).runWith
    sum

  // ── 4. the §5 pipeline with the sources their users write ────────
  //    §5 prices ZIO and fs2 from `iterate`, a per-element source,
  //    and says so; kyo got its chunked `Stream.range` lane, they did
  //    not. fs2 additionally has a PURE mode with no runtime at all.

  private val P = 1000

  @Benchmark
  def pipeline_stdIterator(): Int =
    Iterator.from(0).map(_ * 2).filter(_ % 3 == 0).take(P).sum

  @Benchmark
  def pipeline_zioIterate(): Int =
    val s = _root_.zio.stream.ZStream.iterate(0)(_ + 1)
      .map(_ * 2).filter(_ % 3 == 0).take(P.toLong).runSum
    runZio(s)

  @Benchmark
  def pipeline_zioRange(): Int =
    val s = _root_.zio.stream.ZStream.range(0, 3 * P + 4)
      .map(_ * 2).filter(_ % 3 == 0).take(P.toLong).runSum
    runZio(s)

  @Benchmark
  def pipeline_fs2IterateIO(): Int =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    fs2.Stream.iterate(0)(_ + 1).covary[IO].map(_ * 2).filter(_ % 3 == 0).take(P.toLong)
      .compile.fold(0)(_ + _).unsafeRunSync()

  @Benchmark
  def pipeline_fs2RangeIO(): Int =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    fs2.Stream.range(0, 3 * P + 4).covary[IO].map(_ * 2).filter(_ % 3 == 0).take(P.toLong)
      .compile.fold(0)(_ + _).unsafeRunSync()

  /** fs2 with NO effect: a pure stream compiles synchronously */
  @Benchmark
  def pipeline_fs2RangePure(): Int =
    fs2.Stream.range(0, 3 * P + 4).map(_ * 2).filter(_ % 3 == 0).take(P.toLong)
      .compile.fold(0)(_ + _)

  // ── 5. the cats producer in ManyProducers: fs2 evalMap per element
  //    where zio's side is a plain effect loop ─────────────────────

  private val Total = 8000
  private val Cap = 1024

  @Benchmark
  def producer_cats_fs2EvalMap(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    (for
      q <- cats.effect.std.Queue.bounded[IO, Option[Long]](Cap)
      f <- fs2.Stream.range(0L, Total.toLong).evalMap(i => q.offer(Some(i))).compile.drain.start
      _ <- (f.join *> q.offer(None)).start
      s <- fs2.Stream.fromQueueNoneTerminated(q, limit = 4096).compile.fold(0L)(_ + _)
    yield s).unsafeRunSync()

  @Benchmark
  def producer_cats_ioLoop(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    (for
      q <- cats.effect.std.Queue.bounded[IO, Option[Long]](Cap)
      f <- {
        def go(i: Long): IO[Unit] = if i < Total then q.offer(Some(i)) >> go(i + 1) else IO.unit
        go(0L).start
      }
      _ <- (f.join *> q.offer(None)).start
      s <- fs2.Stream.fromQueueNoneTerminated(q, limit = 4096).compile.fold(0L)(_ + _)
    yield s).unsafeRunSync()

  /** zio's producer as ManyProducers spells it, same run */
  @Benchmark
  def producer_zio_foreach(): Long =
    import _root_.zio.*
    runZio(for
      q <- Queue.bounded[Option[Long]](Cap)
      f <- ZIO.foreachDiscard(0L until Total.toLong)(i => q.offer(Some(i))).fork
      _ <- (f.join *> q.offer(None)).fork
      s <- zio.stream.ZStream.fromQueue(q).collectWhileSome.runSum
    yield s)

  /** fs2's CHUNKED source: `range` is `emit(o) ++ go(o + step)`, a
   * singleton per element (Stream.scala:3981-3993, 3.10.2); its own
   * doc says "to produce the sequence in one chunk use emits" */
  @Benchmark
  def pipeline_fs2EmitsPure(): Int =
    fs2.Stream.emits(0 until 3 * P + 4).map(_ * 2).filter(_ % 3 == 0).take(P.toLong)
      .compile.fold(0)(_ + _)

  // ── 6. the fs2 merge: ChunkFlush chunks AFTER the merge, so the
  //    merge itself still sees singletons. Chunk BEFORE it ─────────

  private val M = 2000

  @Benchmark
  def merge_fs2_singletons(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    fs2.Stream.range(0L, M.toLong).covary[IO].merge(fs2.Stream.range(M.toLong, 2L * M).covary[IO])
      .compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def merge_fs2_chunkedBefore(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val a = fs2.Stream.range(0L, M.toLong).chunkN(256).unchunks.covary[IO]
    val b = fs2.Stream.range(M.toLong, 2L * M).chunkN(256).unchunks.covary[IO]
    a.merge(b).compile.fold(0L)(_ + _).unsafeRunSync()

  @Benchmark
  def merge_fs2_emitsBefore(): Long =
    import cats.effect.IO, cats.effect.unsafe.implicits.global
    val a = fs2.Stream.emits(0L until M.toLong).chunkN(256).unchunks.covary[IO]
    val b = fs2.Stream.emits(M.toLong until 2L * M).chunkN(256).unchunks.covary[IO]
    a.merge(b).compile.fold(0L)(_ + _).unsafeRunSync()

  /** zio's chunked merge from ChunkFlush, same run, for the tie */
  @Benchmark
  def merge_zio_range(): Long =
    import _root_.zio.stream.ZStream
    val a = ZStream.range(0, M).map(_.toLong)
    val b = ZStream.range(M, 2 * M).map(_.toLong)
    runZio(a.merge(b).runFold(0L)(_ + _))
}
