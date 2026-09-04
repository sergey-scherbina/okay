package okay

import org.openjdk.jmh.annotations.{State as JmhState, *}
import java.util.concurrent.TimeUnit

/**
 * Like for like on GRANULARITY, which the guarantee table still had
 * wrong. `ZStream.fromQueue` takes up to `maxChunkSize` (4096 by
 * default) elements per queue operation; our consumer called
 * `receiveBlocking` once per element. So "okayWeak 206 vs zioWeak
 * 169" compared one queue op per element against one per batch.
 *
 * Four lanes, two granularities, both libraries at the SAME weak
 * guarantee, so the only variable left is how many elements one
 * coordination step carries.
 */
@JmhState(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 4, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 6, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ChannelGranularityBenchmark {

  final val N = 4000
  final val Cap = 1024

  @Benchmark
  def okayElementwise(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
    }
    var sum = 0L
    var got = 0
    while got < N do
      c.receiveBlocking() match
        case Some(v) => sum += v; got += 1
        case None => got = N
    t.join(); c.close()
    sum

  @Benchmark
  def okayChunked(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
    }
    var sum = 0L
    var got = 0
    val cb = summon[CanBlock]
    while got < N do
      val chunk = cb.block[Either[Throwable, Chunk[Long]]] { k =>
        c.receiveManyAsync(4096)(k); () => ()
      }.fold(throw _, identity)
      val n = chunk.length
      var i = 0
      while i < n do { sum = sum + chunk(i); i += 1 }
      got += n
      if n == 0 then got = N
    t.join(); c.close()
    sum

  // ── the SEND side, same question ────────────────────────────────
  // A producer holding a batch of elements for an element channel.
  // `feedChunked` does not answer this: it amortizes by
  // REPRESENTATION, putting whole chunks into a Channel[Chunk[A]], so
  // its channel already pays one transaction per chunk. These two
  // lanes are the element channel, offered one at a time against
  // offered in runs.

  final val Batch = 64

  @Benchmark
  def okaySendElem(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
    }
    var sum = 0L; var got = 0
    while got < N do
      c.receiveBlocking() match
        case Some(v) => sum += v; got += 1
        case None => got = N
    t.join(); c.close(); sum

  @Benchmark
  def okaySendBulk(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0
      while i < N do
        val room = if N - i < Batch then N - i else Batch
        val base = i.toLong
        val took = c.sendManyNow(room)(j => base + j)
        // nothing fit: PARK on a single send rather than spin. The
        // first version of this lane busy-waited here and measured
        // 282us against the elementwise lane's 202 -- it was timing
        // the spin, not the bulk claim, because a full ring is the
        // normal state when the consumer takes one element at a time
        if took == 0 then { val _ = c.sendBlocking(base); i += 1 }
        else i += took
    }
    var sum = 0L; var got = 0
    while got < N do
      c.receiveBlocking() match
        case Some(v) => sum += v; got += 1
        case None => got = N
    t.join(); c.close(); sum

  // The bulk send only has room to pay when the CONSUMER leaves room.
  // With an elementwise consumer the ring is full almost always, so
  // every bulk attempt fails its scan and falls back anyway -- which
  // is strictly more work than sending one element (measured: 282 vs
  // 202). These two lanes drain in chunks, which is the shape a bulk
  // producer is actually paired with.

  private def drainChunked(c: Channel[Long]): Long =
    val cb = summon[CanBlock]
    var sum = 0L; var got = 0
    while got < N do
      val chunk = cb.block[Either[Throwable, Chunk[Long]]] { k =>
        c.receiveManyAsync(4096)(k); () => ()
      }.fold(throw _, identity)
      var i = 0
      while i < chunk.length do { sum = sum + chunk(i); i += 1 }
      got += chunk.length
      if chunk.length == 0 then got = N
    sum

  @Benchmark
  def okaySendElemRecvChunk(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0L
      while i < N do { val _ = c.sendBlocking(i); i += 1 }
    }
    val s = drainChunked(c); t.join(); c.close(); s

  @Benchmark
  def okaySendBulkRecvChunk(): Long =
    val c = AbruptChannel[Long](Cap)
    val t = Thread.ofVirtual().start { () =>
      var i = 0
      while i < N do
        val room = if N - i < Batch then N - i else Batch
        val base = i.toLong
        val took = c.sendManyNow(room)(j => base + j)
        if took == 0 then { val _ = c.sendBlocking(base); i += 1 }
        else i += took
    }
    val s = drainChunked(c); t.join(); c.close(); s

  /** their queue, OUR granularity: one take per element, no stream */
  @Benchmark
  def zioElementwise(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- ZIO.foreachDiscard(0 until N)(_ => q.take.flatMap(x => r.update(_ + x)))
        s <- r.get
      yield s).getOrThrowFiberFailure())

  /** their queue, THEIR granularity: the stream pulls whole chunks */
  @Benchmark
  def zioChunked(): Long =
    import _root_.zio.*
    _root_.zio.Unsafe.unsafe(implicit u =>
      _root_.zio.Runtime.default.unsafe.run(for
        q <- Queue.bounded[Long](Cap)
        _ <- ZIO.foreachDiscard(0L until N.toLong)(q.offer).fork
        r <- Ref.make(0L)
        _ <- zio.stream.ZStream.fromQueue(q).take(N.toLong).runForeach(x => r.update(_ + x))
        s <- r.get
      yield s).getOrThrowFiberFailure())
}
