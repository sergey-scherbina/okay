package okay

import org.openjdk.jmh.annotations.*
import java.util.concurrent.TimeUnit
import scala.collection.immutable.ArraySeq
import scala.annotation.nowarn
import RowLift.plus

/**
 * producer-to-writer-carrier, STAGE 0 (specs/producer-to-writer-carrier.md):
 * is there a real cost to moving the pure pull stream off `Producer[A]
 * = A ! Produce` (the identity signature — an operation IS its
 * element, so the element type IS the answer type) onto the writer
 * carrier `Unit ! Writer % W` / `Source[W] = Unit ! (Writer % W +
 * Async)` (the element named in the type, the answer free)?
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

  private def producerChunks: Chunks[Long] =
    def go(i: Int): Chunks[Long] =
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

  @Benchmark
  def chunksFoldProducer(): Long =
    Chunks.fold(producerChunks)(using Fold.sumLong)

  // Writer % Chunk[X]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), same caveat
  // Writer.scala documents on Writer.run (E092, the TypeableK caveat)
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def chunksFoldWriter(): Long =
    Writer.fold[Chunk[Long], Long, Unit, Nothing](writerChunks).runWith._1

  // The three below are Async-shaped (Blob's own shape) rather than
  // Pure, only because `Chunks.foldWriter`/`foldLeftWriter` marked
  // `inline` (needed to reach parity, see their doc in Chunks.scala)
  // stop compiling a literal `Nothing`/`Pure` at a fresh call site —
  // an unrelated inliner limitation. `Chunks.fold` has no G-generic
  // form to build an Async-shaped Producer control from (`Chunks[A]`
  // is Pure by definition), so the control these three compare
  // against stays the Pure `chunksFoldProducer` above; Async overhead
  // with no real async op is a few ns, not the multi-us this measures.

  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def chunksFoldWriterAsync(): Long =
    given CanBlock = cb
    Writer.fold[Chunk[Long], Long, Unit, Async](writerChunksAsync).runWith._1

  // PARITY, MEASURED: foldLeftWriter called DIRECTLY, a literal step,
  // no Fold instance — the shape okay-cluster's own Chunks.foldLeft
  // call sites (Flows.scala, Job.scala) already use. This is the
  // number that says stage 2 can migrate THOSE, today.
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def chunksFoldLeftWriterDirect(): Long =
    given CanBlock = cb
    Chunks.foldLeftWriter[Long, Long, Async](writerChunksAsync)(0L)((s, a) => s + a).runWith._1

  // the SAME literal-step shape on Producer, chunked — the control
  // chunksFoldLeftWriterDirect reads against, since chunksFoldProducer
  // above uses Chunks.fold(using a Fold instance), not foldLeft
  @Benchmark
  def chunksFoldLeftProducerDirect(): Long =
    Chunks.foldLeft(producerChunks)(0L)((s, a) => s + a)

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
  def chunksMapProducer(): Long =
    Chunks.fold(Chunks.map(producerChunks)(_ * 2L))(using Fold.sumLong)

  // Writer % Chunk[X]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), same caveat
  // Writer.scala documents on Writer.run (E092, the TypeableK caveat)
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def chunksMapWriter(): Long =
    Writer.fold[Chunk[Long], Long, Unit, Nothing](Writer.map(writerChunks)(doubleChunk)).runWith._1

  // ---------------------------------------------------------- 3
  // Source.ofProducer / Source.toProducer: the bridge cost, on the
  // Async-shaped elementwise stream Blob actually builds

  private def producerLongs: Long ! (Produce + Async) =
    def go(i: Long): Long ! (Produce + Async) =
      if i >= n then okay.pure(0L) else produce(i).plus[Async].flatMap(_ => go(i + 1))
    go(0L)

  private def writerLongs: Unit ! (Writer % Long + Async) =
    def go(i: Long): Unit ! (Writer % Long + Async) =
      if i >= n then okay.pure(()) else Writer.tell(i).plus[Async].flatMap(_ => go(i + 1))
    go(0L)

  @Benchmark
  def bridgeProducerDirect(): Long =
    given CanBlock = cb
    Stream.fold(producerLongs)(using Fold.sumLong)

  @Benchmark
  def bridgeProducerThroughSource(): Long =
    given CanBlock = cb
    val asSource: Unit ! (Writer % Long + Async) = Source.ofProducer[Long, Async](producerLongs)
    Writer.fold[Long, Long, Unit, Async](asSource)(using summon, Fold.sumLong).runWith._1

  @Benchmark
  def bridgeWriterDirect(): Long =
    given CanBlock = cb
    Writer.fold[Long, Long, Unit, Async](writerLongs)(using summon, Fold.sumLong).runWith._1

  @Benchmark
  def bridgeWriterThroughProducer(): Long =
    given CanBlock = cb
    val asProducer: Long ! (Produce + Async) = Source.toProducer[Long, Async](writerLongs)(0L)
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

  private def byteProducer: Chunk[Byte] ! (Produce + Async) =
    def go(i: Int): Chunk[Byte] ! (Produce + Async) =
      if i >= byteChunks.length then okay.pure(Chunks.emptyChunk[Byte])
      else produce(byteChunks(i)).plus[Async].flatMap(_ => go(i + 1))
    go(0)

  private def byteSource: Unit ! (Writer % Chunk[Byte] + Async) =
    def go(i: Int): Unit ! (Writer % Chunk[Byte] + Async) =
      if i >= byteChunks.length then okay.pure(())
      else Writer.tell(byteChunks(i)).plus[Async].flatMap(_ => go(i + 1))
    go(0)

  @Benchmark
  def blobBytesProducer(): Array[Byte] =
    given CanBlock = cb
    val out = new java.io.ByteArrayOutputStream(byteChunks.length * 1024)
    val _ = Producer.each[Chunk[Byte], Chunk[Byte], Async](byteProducer)(c => out.write(c.toArray)).runWith
    out.toByteArray

  // Writer % Chunk[X]'s split test is unchecked under erasure — sound
  // by construction (Say is Writer's ONLY constructor), same caveat
  // Writer.scala documents on Writer.run (E092, the TypeableK caveat)
  @nowarn("msg=cannot be checked at runtime")
  @Benchmark
  def blobBytesWriter(): Array[Byte] =
    given CanBlock = cb
    val out = new java.io.ByteArrayOutputStream(byteChunks.length * 1024)
    val sink: Fold[Chunk[Byte], Unit] = Fold(())((_, c) => out.write(c.toArray))
    val _ = Writer.fold[Chunk[Byte], Unit, Unit, Async](byteSource)(using summon, sink).runWith
    out.toByteArray
}
