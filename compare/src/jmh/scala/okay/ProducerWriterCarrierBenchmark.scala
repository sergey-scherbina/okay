package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq
import Row.plus

/**
 * producer-to-writer-carrier, STAGE 0 (specs/producer-to-writer-carrier.md):
 * is there a real cost to moving the pure pull stream off `Producer[A]
 * = A ! Produce` (the identity signature — an operation IS its
 * element, so the element type IS the answer type) onto the writer
 * carrier `Unit ! Writer % W` / `Source[W] = Unit ! Writer % W +
 * Async` (the element named in the type, the answer free)?
 *
 * Four shapes, each a pair on the SAME data and the SAME accumulator,
 * so the difference is the carrier and nothing else:
 *
 *  1. `Chunks.fold` — sum N longs, chunked, one tree step per chunk.
 *  2. `Chunks.map` then fold — the resume-based per-chunk transform
 *     `Chunks.map` already has, against `Writer.map`'s identical
 *     shape (both already exist; nothing new is written to compare
 *     them).
 *  3. The `Source.ofProducer` / `Source.toProducer` bridges, on the
 *     Async-shaped elementwise stream Blob actually uses — what the
 *     round trip between the two carriers costs TODAY, since stage 2
 *     deletes it.
 *  4. A blob byte lane — `Producer.each` (what `Blob.getBytes` calls)
 *     against `Writer.fold` with a side-effecting sink (what
 *     `Blob.getSource`, drained, would call) — no engine, a synthetic
 *     chunk array, so the number is the carrier's cost alone.
 *
 * The elementwise, un-chunked, no-Async comparison (`Writer.fold`
 * against `Stream.fold`, both with `Fold.sumLong`) already exists as
 * `FoldConsumersBenchmark.writerSpecialized` / `.streamSpecialized` —
 * not duplicated here; run that class alongside this one for the
 * fifth number stage 0 asks for.
 *
 * Verdict goes in specs/producer-to-writer-carrier.md `## Results`
 * and in sprint.d/doing/producer-to-writer-carrier.md, with the
 * numbers, per the performance skill: three whole runs of this class
 * (each fork already restarts the JVM), median compared, host load
 * recorded, a row appended to src/jmh/history.tsv.
 *
 * ADDED LATER, the chunk-aware fold stage 0 named as a prerequisite
 * (`Chunks.foldLeftWriter`/`foldWriter` in okay-stream's Chunks.scala):
 * `chunksFoldWriterAsync` / `chunksFoldLeftWriterDirect` /
 * `chunksFoldLeftProducerDirect` / `chunksFoldWriterDispatched` — the
 * literal-step form (`foldLeftWriter`, what `okay-cluster`'s own
 * `Chunks.foldLeft` call sites use) reaches parity — 5.00 vs
 * `chunksFoldLeftProducerDirect`'s 4.76 us/op. The `Fold`-instance-
 * dispatched form (`foldWriter`, what `Chunks.fold`/`agg.fold` need)
 * did NOT (17.30 us/op) until fixed 2026-09-19 in two steps: routing
 * it through `writerStreamIn`'s `.iterator` instead of
 * `Writer.foldWith`'s fused trampoline (wrapped in `async{}` to keep
 * it a suspended program, signature narrowed to `Async`+`CanBlock`)
 * got to 6.29 us/op; giving `writerStreamIn` its OWN hand-specialized
 * `.iterator` override (mirroring `Stream[Producer, Pure]`'s own, in
 * Generate.scala) — instead of `.iterator`'s default `Iterator.unfold`,
 * which still paid an `Option`+`Either`+`Free`-node per chunk — got to
 * 5.43 us/op median (3 rounds), now matching `chunksFoldLeftWriterDirect`'s
 * own 5.0us almost exactly. See Chunks.scala's own doc on `foldWriter`
 * for why the remaining ~2x against `chunksFoldProducer`'s 2.55us is a
 * different, larger question (walking a `Free`-tree program at all)
 * than this combinator's own dispatch tax.
 *
 * CORRECTED (producer-writer-carrier-pure-iterator, 2026-09-19): that
 * "~2x" was the loop living INSIDE the `@Benchmark` method. The
 * `chunksFold*FeedPure*` rows and the `Probe` object below measure
 * `Chunks.fold`'s own loop over the PURE writer stream's new
 * `.iterator` (`Chunks[A]` is Pure — no G, no `CanBlock`): 4.9 inside
 * the benchmark method, 2.63 in an ordinary method, against library
 * `Chunks.fold`'s 2.53. Parity. The same move does nothing for
 * Producer in this module (4.5 either way) — see
 * backlog.d/okay-core/chunks-fold-vs-foldleft-2x-gap.md.
 */
@State(Scope.Thread)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@Warmup(iterations = 3, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Fork(2)
class ProducerWriterCarrierBenchmark {

  private val n = 10000
  private val chunkSize = 64

  private val cb: CanBlock = summon[CanBlock]

  // ---------------------------------------------------------- 1 & 2
  // Chunks.fold and Chunks.map, chunked, both carriers

  private val chunks: Array[Chunk[Long]] =
    LazyList.range(0L, n.toLong).grouped(chunkSize)
      .map(g => ArraySeq.from(g): Chunk[Long]).toArray

  // the OLD carrier, kept as the historical control: `Chunks[A]` was
  // `Producer[Chunk[A]]` until producer-writer-carrier-chunks-retype
  // (2026-09-19), so the library's `Chunks.*` can no longer take this;
  // the `Probe` loops below walk it the way `Chunks.fold` used to
  private def producerChunks: Producer[Chunk[Long]] =
    def go(i: Int): Producer[Chunk[Long]] =
      if i >= chunks.length then okay.pure(Chunks.emptyChunk[Long])
      else produce(chunks(i)).flatMap(_ => go(i + 1))
    go(0)

  private def writerChunks: Unit ! Writer % Chunk[Long] =
    def go(i: Int): Unit ! Writer % Chunk[Long] =
      if i >= chunks.length then okay.pure(())
      else Writer.tell(chunks(i)).flatMap(_ => go(i + 1))
    go(0)

  private def writerChunksAsync: Source[Chunk[Long]] =
    def go(i: Int): Source[Chunk[Long]] =
      if i >= chunks.length then okay.pure(())
      else Writer.tell(chunks(i)).plus[Async].flatMap(_ => go(i + 1))
    go(0)

  /** the same unboxed per-chunk sum on both sides: `Chunks.fold`
   * dispatches to this shape internally for `Fold.OfLong`; `Writer`
   * has no such dispatch (its own told value IS the chunk here), so
   * this is the honest generic `Fold[Chunk[Long], Long]` it would use */
  private given sumChunk: Fold[Chunk[Long], Long] = Fold(0L): (s, c) =>
    var t = s
    var i = 0
    while i < c.length do
      t += c(i)
      i += 1
    t

  // THE LIBRARY METHOD ON THE RETYPED CARRIER (producer-writer-carrier-
  // chunks-retype): `Chunks.fold` over `Chunks[Long] = Feed[Chunk[Long]]`
  // — the number that replaces `chunksFoldProducer`'s 2.53 (the same
  // method over `Producer[Chunk[Long]]`, history.tsv up to
  // producer-writer-carrier-pure-iterator; it cannot be run any more)
  @Benchmark
  def chunksFoldChunks(): Long =
    Chunks.fold(writerChunks)(using Fold.sumLong)

  @Benchmark
  def chunksFoldWriter(): Long =
    Writer.fold[Chunk[Long], Long, Unit, Nothing](writerChunks).runWith._1

  // THE PURE SHAPE (producer-writer-carrier-pure-iterator): `Chunks[A]`
  // is `Producer[Chunk[A]]` — PURE, no G — so what `Chunks.fold` will
  // do once `Chunks[A] = Feed[Chunk[A]]` is exactly `Chunks.fold`'s
  // own loop over the PURE writer stream's `.iterator` (Writer.scala,
  // the override added in this lane), `Handler[Pure]` only, no
  // `CanBlock`, no `TypeableK`. Every earlier writer row in this file
  // walks the G-effectful `writerStreamIn`; these two are the honest
  // pair for `chunksFoldProducer` / `chunksFoldLeftProducerDirect`.
  private val feedStream = okay.feedStream[Unit]

  @Benchmark
  def chunksFoldFeedPure(): Long =
    // `Chunks.fold`'s `Fold.OfLong` arm, verbatim: dispatch once
    // outside, `addLong` on a raw long inside
    val l: Fold.OfLong[Long] = Fold.sumLong
    var s = l.initLong
    val it = feedStream.iterator(writerChunks)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = l.addLong(s, c(i))
        i += 1
    s

  @Benchmark
  def chunksFoldLeftFeedPure(): Long =
    // `Chunks.foldLeft` with a literal step, verbatim
    var s = 0L
    val it = feedStream.iterator(writerChunks)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = s + c(i)
        i += 1
    s

  // THE SAME FOUR LOOPS, EACH IN ITS OWN NON-INLINE METHOD (the probe
  // backlog.d/okay-core/chunks-fold-vs-foldleft-2x-gap.md never got to
  // measure): `Chunks.fold` is an ordinary library method the JIT
  // compiles as ITS OWN unit; `Chunks.foldLeft` is `inline`, and the
  // two `*FeedPure` rows above are written out longhand, so those
  // three loops all land INSIDE the JMH-generated benchmark method.
  // If the gap is the compiled unit rather than the loop, these four
  // agree with `chunksFoldProducer` and not with the inline rows.
  @Benchmark
  def chunksFoldProducerOwnMethod(): Long = Probe.foldProducer(producerChunks, Fold.sumLong)
  @Benchmark
  def chunksFoldLeftProducerOwnMethod(): Long = Probe.foldLeftProducer(producerChunks)

  // chunks-fold-vs-foldleft-2x-gap, the probe the entry named next:
  // the SAME loop with A kept ABSTRACT at the loop (a generic method
  // dispatching on Fold.OfLong[A], as Chunks.fold's OfLong arm had it
  // when it was the library's own method over Producer) — the one
  // semantic difference found between the 2.53 library row and every
  // 4.5 row compiled in this module (with A = Long, dotty unboxes c(i)
  // before addLong boxes it again)
  @Benchmark
  def chunksFoldProducerAbstractOwnMethod(): Long = Probe.foldProducerAbstract[Long](producerChunks)(using Fold.sumLong)
  @Benchmark
  def chunksFoldFeedPureAbstractOwnMethod(): Long = Probe.foldFeedAbstract[Long](writerChunks)(using Fold.sumLong)
  @Benchmark
  def chunksFoldFeedPureOwnMethod(): Long = Probe.foldFeed(writerChunks, Fold.sumLong)
  @Benchmark
  def chunksFoldLeftFeedPureOwnMethod(): Long = Probe.foldLeftFeed(writerChunks)
  // the Feed loop with the instance summoned INSIDE the method (a
  // fresh `new` per call — the shape `Chunks.fold` itself has now). A
  // sibling row that ran the INLINE `Chunks.foldLeft` on Producer
  // inside an own method (4.53, same as the other Producer rows) went
  // with the retype: `Chunks.foldLeft` no longer takes a Producer.
  @Benchmark
  def chunksFoldFeedPureSummonOwnMethod(): Long = Probe.foldFeedSummon(writerChunks, Fold.sumLong)

  // The three below are Async-shaped (Blob's own shape) rather than
  // Pure, only because `Chunks.foldWriter`/`foldLeftWriter` marked
  // `inline` (needed to reach parity, see their doc in Chunks.scala)
  // stop compiling a literal `Nothing`/`Pure` at a fresh call site —
  // an unrelated inliner limitation. `Chunks.fold` has no G-generic
  // form to build an Async-shaped Producer control from (`Chunks[A]`
  // is Pure by definition), so the control these three compare
  // against stays the Pure `chunksFoldProducer` above; Async overhead
  // with no real async op is a few ns, not the multi-us this measures.

  @Benchmark
  def chunksFoldWriterAsync(): Long =
    given CanBlock = cb
    Writer.fold[Chunk[Long], Long, Unit, Async](writerChunksAsync).runWith._1

  // PARITY, MEASURED: foldLeftWriter called DIRECTLY, a literal step,
  // no Fold instance — the shape okay-cluster's own Chunks.foldLeft
  // call sites (Flows.scala, Job.scala) already use. This is the
  // number that says stage 2 can migrate THOSE, today.
  @Benchmark
  def chunksFoldLeftWriterDirect(): Long =
    given CanBlock = cb
    Chunks.foldLeftWriter[Long, Long, Async](writerChunksAsync)(0L)((s, a) => s + a).runWith._1

  // the SAME literal-step shape on Producer, chunked — the control
  // chunksFoldLeftWriterDirect reads against, since chunksFoldProducer
  // above uses Chunks.fold(using a Fold instance), not foldLeft
  @Benchmark
  def chunksFoldLeftChunksDirect(): Long =
    Chunks.foldLeft(writerChunks)(0L)((s, a) => s + a)

  // AT PARITY (2026-09-19, fixed): foldWriter, the Fold-INSTANCE-
  // dispatched form Chunks.fold/agg.fold need (Bulk.scala,
  // Pipeline.scala, Acceptance.scala). Three earlier shapes (a hand-
  // rolled walker, a foldWith-based one, and that one with foldWriter
  // ALSO inline) all landed at 16.7-18.8us — the fix was routing the
  // walk through Writer's own `.iterator` (small per-step `uncons`
  // calls) instead of `Writer.foldWith`'s fused resume/split/Bind
  // trampoline, which let escape analysis eliminate the element box
  // the way it already does for `chunksFoldProducer`. The signature
  // narrowed from an arbitrary G to Async+CanBlock as part of the fix
  // (see Chunks.scala's own doc on `foldWriter` for the whole story).
  @Benchmark
  def chunksFoldWriterDispatched(): Long =
    given CanBlock = cb
    Chunks.foldWriter[Long, Long](writerChunksAsync)(using Fold.sumLong).runWith._1

  /** the per-chunk doubler `Chunks.map` specializes internally
   * (`ChunkBuf.mapper`); `Writer.map` has no chunk-aware combinator
   * yet, so this is the tight unboxed loop it would need — written
   * here rather than in the library, since stage 0 is measure-only */
  private def doubleChunk(c: Chunk[Long]): Chunk[Long] =
    val len = c.length
    val arr = new Array[Long](len)
    var i = 0
    while i < len do
      arr(i) = c(i) * 2L
      i += 1
    ArraySeq.unsafeWrapArray(arr)

  @Benchmark
  def chunksMapChunks(): Long =
    Chunks.fold(Chunks.map(writerChunks)(_ * 2L))(using Fold.sumLong)

  @Benchmark
  def chunksMapWriter(): Long =
    Writer.fold[Chunk[Long], Long, Unit, Nothing](Writer.map[Chunk[Long], Chunk[Long], Unit, Pure](writerChunks)(doubleChunk)).runWith._1

  // ---------------------------------------------------------- 3
  // Source.ofProducer / Source.toProducer: the bridge cost, on the
  // Async-shaped elementwise stream Blob actually builds

  private def producerLongs: Long ! Produce + Async =
    def go(i: Long): Long ! Produce + Async =
      if i >= n then okay.pure(0L) else produce(i).plus[Async].flatMap(_ => go(i + 1))
    go(0L)

  private def writerLongs: Unit ! Writer % Long + Async =
    def go(i: Long): Unit ! Writer % Long + Async =
      if i >= n then okay.pure(()) else Writer.tell(i).plus[Async].flatMap(_ => go(i + 1))
    go(0L)

  @Benchmark
  def bridgeProducerDirect(): Long =
    given CanBlock = cb
    Stream.fold(producerLongs)(using Fold.sumLong)

  // producer-effectful-stream-iterator: the G-effectful producer's
  // `.iterator`, specialized (an override, mirroring writerStreamIn's)
  // against the DEFAULT walk it replaced, written out here verbatim
  // (`Iterator.unfold(s)(uncons(_).runWith)`) so the pair alternates
  // in one run. Elementwise, N=10000 Async-widened produces.
  private val producerStreamAsync = summon[Stream[[A] =>> A ! Produce + Async, Async]]

  @Benchmark
  def producerIteratorSpecialized(): Long =
    given CanBlock = cb
    var s = 0L
    val it = producerStreamAsync.iterator(producerLongs)
    while it.hasNext do s += it.next()
    s

  @Benchmark
  def producerIteratorDefaultUnfold(): Long =
    given CanBlock = cb
    var s = 0L
    val it = Iterator.unfold(producerLongs)(x => producerStreamAsync.uncons(x).runWith)
    while it.hasNext do s += it.next()
    s

  @Benchmark
  def bridgeProducerThroughSource(): Long =
    given CanBlock = cb
    val asSource: Unit ! Writer % Long + Async = Source.ofProducer[Long, Async](producerLongs)
    Writer.fold[Long, Long, Unit, Async](asSource)(using summon)(using summon, Fold.sumLong).runWith._1

  @Benchmark
  def bridgeWriterDirect(): Long =
    given CanBlock = cb
    Writer.fold[Long, Long, Unit, Async](writerLongs)(using summon)(using summon, Fold.sumLong).runWith._1

  @Benchmark
  def bridgeWriterThroughProducer(): Long =
    given CanBlock = cb
    val asProducer: Long ! Produce + Async = Source.toProducer[Long, Async](writerLongs)(0L)
    Stream.fold(asProducer)(using Fold.sumLong)

  // ---------------------------------------------------------- 4
  // a blob byte lane: Producer.each (Blob.getBytes' own call) against
  // Writer.fold with a side-effecting sink (Blob.getSource, drained).
  // No engine — a synthetic chunk array — so the number is the
  // carrier's cost, not an I/O cost. Blob's real answer type is
  // `Either[String, Unit]`; simplified here to the chunk type itself
  // (Chunks' own end-of-stream convention), since the Either is not
  // what this measures.

  private val byteChunks: Array[Chunk[Byte]] =
    Array.tabulate(64)(i => ArraySeq.fill(1024)((i % 256).toByte))

  private def byteProducer: Chunk[Byte] ! Produce + Async =
    def go(i: Int): Chunk[Byte] ! Produce + Async =
      if i >= byteChunks.length then okay.pure(Chunks.emptyChunk[Byte])
      else produce(byteChunks(i)).plus[Async].flatMap(_ => go(i + 1))
    go(0)

  private def byteSource: Unit ! Writer % Chunk[Byte] + Async =
    def go(i: Int): Unit ! Writer % Chunk[Byte] + Async =
      if i >= byteChunks.length then okay.pure(())
      else Writer.tell(byteChunks(i)).plus[Async].flatMap(_ => go(i + 1))
    go(0)

  @Benchmark
  def blobBytesProducer(): Array[Byte] =
    given CanBlock = cb
    val out = new java.io.ByteArrayOutputStream(byteChunks.length * 1024)
    val _ = Producer.each[Chunk[Byte], Chunk[Byte], Async](byteProducer)(c => out.write(c.toArray)).runWith
    out.toByteArray

  @Benchmark
  def blobBytesWriter(): Array[Byte] =
    given CanBlock = cb
    val out = new java.io.ByteArrayOutputStream(byteChunks.length * 1024)
    val sink: Fold[Chunk[Byte], Unit] = Fold(())((_, c) => out.write(c.toArray))
    val _ = Writer.fold[Chunk[Byte], Unit, Unit, Async](byteSource)(using summon)(using summon, sink).runWith
    out.toByteArray

  // ---------------------------------------------------------- 5
  // writer-collect-loops: the three collect drains as calls to
  // `Writer.loopWith` (cons per element, one reverse, the finisher
  // INSIDE the loop) against the shape they replaced, written out
  // verbatim as the control rows so each pair alternates in one run:
  // `Source.concat` was `Writer.collect(s).map(_._1.flatten)` — a
  // `Vector :+` per chunk and a `.map` over a program that still
  // forwards Async — and `runCollect` was its own copy of the loop
  // with a `Vector :+` per element. Two source shapes for `concat`:
  // the byte chunks above told plainly, and the same with ONE `async`
  // step between chunks, which is the shape a page-fetching driver
  // (okay-jdbc's drains) actually has and the one where the mapped
  // residual costs a rotation per forwarded operation.

  private def byteSourceAsync: Unit ! Writer % Chunk[Byte] + Async =
    def go(i: Int): Unit ! Writer % Chunk[Byte] + Async =
      if i >= byteChunks.length then okay.pure(())
      else async(()).plus[Writer % Chunk[Byte]].flatMap(_ =>
        Writer.tell(byteChunks(i)).plus[Async].flatMap(_ => go(i + 1)))
    go(0)

  /** the old `Writer.collect`, verbatim: its own loop split on G, a
   * `Vector :+` per told value */
  private def collectControl[W, A, G[+_] : TypeableK](a: A ! Writer % W + G): (Vector[W], A) ! G =
    import !.*
    import scala.annotation.tailrec
    def again(acc: Vector[W])(x: A ! Writer % W + G): (Vector[W], A) ! G = loop(acc)(x)
    @tailrec def loop(acc: Vector[W])(x: A ! Writer % W + G): (Vector[W], A) ! G =
      (x.resume: @unchecked) match
        case Free.Return(v) => okay.pure((acc, v))
        case Inject(e) => split[G, Writer % W](e)
          (g => Inject(g).map(v => (acc, v)): (Vector[W], A) ! G)
          { w0 => (w0: @unchecked) match
              case Writer.Say(w) => okay.pure((acc :+ w, ())) }
        case Bind(Inject(e), k) => split[G, Writer % W](e)
          (g => Inject(g).flatMap(v => again(acc)(k(v))))
          { w0 => (w0: @unchecked) match
              case Writer.Say(w) => loop(acc :+ w)(k(())) }
    loop(Vector.empty)(a)

  /** the old `Source.concat`, verbatim */
  private def concatControl[X](s: Source[Chunk[X]]): Vector[X] ! Async =
    collectControl[Chunk[X], Unit, Async](s).map(_._1.flatten)

  /** the old `Source.runCollect`, verbatim */
  private def runCollectControl[A](s: Source[A]): Vector[A] ! Async =
    import !.*
    import scala.annotation.tailrec
    def again(acc: Vector[A])(x: Source[A]): Vector[A] ! Async = loop(acc)(x)
    @tailrec def loop(acc: Vector[A])(x: Source[A]): Vector[A] ! Async =
      (x.resume: @unchecked) match
        case Free.Return(_) => okay.pure(acc)
        case Inject(e) => split[Async, Writer % A](e)
          (g => Inject(g).map(_ => acc): Vector[A] ! Async)
          { case Writer.Say(a) => okay.pure(acc :+ a) }
        case Bind(Inject(e), k) => split[Async, Writer % A](e)
          (g => Inject(g).flatMap(v => again(acc)(k(v))))
          { w0 => (w0: @unchecked) match
              case Writer.Say(a) => loop(acc :+ a)(k(())) }
    loop(Vector.empty)(s)

  @Benchmark
  def concatBytes(): Int =
    given CanBlock = cb
    Source.concat(byteSource).runWith.length

  @Benchmark
  def concatBytesControl(): Int =
    given CanBlock = cb
    concatControl(byteSource).runWith.length

  @Benchmark
  def concatBytesAsync(): Int =
    given CanBlock = cb
    Source.concat(byteSourceAsync).runWith.length

  @Benchmark
  def concatBytesAsyncControl(): Int =
    given CanBlock = cb
    concatControl(byteSourceAsync).runWith.length

  @Benchmark
  def runCollectLongs(): Int =
    given CanBlock = cb
    writerLongs.runCollect.runWith.length

  @Benchmark
  def runCollectLongsControl(): Int =
    given CanBlock = cb
    runCollectControl(writerLongs).runWith.length
}

/** the four chunked-fold loops as ordinary (non-inline, non-benchmark)
 * methods — `Chunks.fold`'s own shape, one compiled unit each; see the
 * `*OwnMethod` rows in the class above for what they answer */
object Probe {
  private val feedStream = okay.feedStream[Unit]
  private val producerStream = summon[Stream[Producer, Pure]]

  def foldProducer(p: Producer[Chunk[Long]], l: Fold.OfLong[Long]): Long =
    var s = l.initLong
    val it = producerStream.iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = l.addLong(s, c(i))
        i += 1
    s

  /** A abstract at the loop: `c(i)` is `apply(i): Object` handed
   * straight to `addLong(J, Object)`, no unbox-then-box — the shape
   * `Chunks.fold`'s OfLong arm compiles to inside the library */
  def foldProducerAbstract[A](p: Producer[Chunk[A]])(using fo: Fold[A, Long]): Long = fo match
    case l: Fold.OfLong[A @unchecked] =>
      var s = l.initLong
      val it = producerStream.iterator(p)
      while it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length do
          s = l.addLong(s, c(i))
          i += 1
      s
    case _ => throw IllegalArgumentException("the probe is for Fold.OfLong")

  def foldFeedAbstract[A](p: Unit ! Writer % Chunk[A])(using fo: Fold[A, Long]): Long = fo match
    case l: Fold.OfLong[A @unchecked] =>
      var s = l.initLong
      val it = okay.feedStream[Unit].iterator(p)
      while it.hasNext do
        val c = it.next()
        var i = 0
        while i < c.length do
          s = l.addLong(s, c(i))
          i += 1
      s
    case _ => throw IllegalArgumentException("the probe is for Fold.OfLong")

  def foldLeftProducer(p: Producer[Chunk[Long]]): Long =
    var s = 0L
    val it = producerStream.iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = s + c(i)
        i += 1
    s

  def foldFeed(p: Unit ! Writer % Chunk[Long], l: Fold.OfLong[Long]): Long =
    var s = l.initLong
    val it = feedStream.iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = l.addLong(s, c(i))
        i += 1
    s

  def foldFeedSummon(p: Unit ! Writer % Chunk[Long], l: Fold.OfLong[Long]): Long =
    var s = l.initLong
    val it = okay.feedStream[Unit].iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = l.addLong(s, c(i))
        i += 1
    s

  def foldLeftFeed(p: Unit ! Writer % Chunk[Long]): Long =
    var s = 0L
    val it = feedStream.iterator(p)
    while it.hasNext do
      val c = it.next()
      var i = 0
      while i < c.length do
        s = s + c(i)
        i += 1
    s
}
