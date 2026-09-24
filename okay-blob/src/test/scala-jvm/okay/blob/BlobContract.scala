package okay.blob

import okay.{!, +, %, Async, Chunk, Fold, Source, Writer}
import okay.Row.plus
import okay.given
import scala.collection.immutable.ArraySeq

/**
 * The contract every engine must pass (the StoreSuite pattern): fs
 * runs it now, the S3 engine re-runs it at blob-s3 against MinIO.
 * Extend and provide `make`.
 */
abstract class BlobContract(engine: String) extends munit.FunSuite {

  def make(): Blob

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** a Source of `total` deterministic bytes in `piece`-sized chunks */
  def bytes(total: Int, piece: Int = 8 * 1024): Source[Chunk[Byte]] =
    def go(off: Int): Source[Chunk[Byte]] =
      if off >= total then okay.pure(())
      else
        val n = math.min(piece, total - off)
        val a = Array.tabulate[Byte](n)(i => ((off + i) % 251).toByte)
        Writer.tell(ArraySeq.unsafeWrapArray(a)).plus[Async].flatMap(_ => go(off + n))
    go(0)

  /** drain a get: the collected bytes, the outcome, and the LARGEST
   * chunk seen — the constant-memory witness */
  // Writer % Chunk[Byte]'s split test is unchecked under erasure —
  // sound by construction, the TypeableK caveat Writer.scala documents
  def drainGet(p: Either[String, Unit] ! Writer % Chunk[Byte] + Async): (Array[Byte], Either[String, Unit], Int) =
    val out = java.io.ByteArrayOutputStream()
    var biggest = 0
    val sink: Fold[Chunk[Byte], Unit] = Fold(())((_, c) => { out.write(c.toArray); biggest = math.max(biggest, c.length) })
    val (_, outcome) = run(Writer.fold[Chunk[Byte], Unit, Either[String, Unit], Async](p)(using summon, sink))
    (out.toByteArray, outcome, biggest)

  def drainList(p: Source[Chunk[Meta]]): Vector[Meta] =
    run(Writer.collect(p))._1.flatMap(_.toVector)

  test(s"$engine: put then get round-trips at constant memory") {
    val b = make()
    val big = 3 * 64 * 1024 + 17   // larger than any single chunk
    val _ = run(b.put("seg/0001.dat", bytes(big)))
    val (got, outcome, biggest) = drainGet(b.get("seg/0001.dat"))
    assertEquals(outcome, Right(()))
    assertEquals(got.length, big)
    assert(got.zipWithIndex.forall((x, i) => x == (i % 251).toByte), "bytes differ")
    assert(biggest <= 64 * 1024, s"a chunk of $biggest bytes — not constant memory")
  }

  test(s"$engine: a range is exactly the slice; head reports size and etag without a body") {
    val b = make()
    val etag = run(b.put("r/x", bytes(1000)))
    val (slice, outcome, _) = drainGet(b.get("r/x", Some((100L, 110L))))
    assertEquals(outcome, Right(()))
    assertEquals(slice.toVector, Vector.tabulate(10)(i => ((100 + i) % 251).toByte))
    val meta = run(b.head("r/x")).get
    assertEquals(meta.size, 1000L)
    assertEquals(meta.etag, etag)
  }

  test(s"$engine: list(prefix) yields every key once, in key order") {
    val b = make()
    for k <- Seq("a/2", "a/1", "b/1", "a/10") do
      val _ = run(b.put(k, bytes(8)))
    assertEquals(drainList(b.list("a/")).map(_.key), Vector("a/1", "a/10", "a/2"))
    assertEquals(drainList(b.list("")).map(_.key), Vector("a/1", "a/10", "a/2", "b/1"))
  }

  test(s"$engine: absent keys — get is a Left naming the key, head None, delete idempotent") {
    val b = make()
    val (got, outcome, _) = drainGet(b.get("no/such"))
    assertEquals(got.length, 0)
    assert(outcome.left.exists(_.contains("no/such")), outcome.toString)
    assertEquals(run(b.head("no/such")), None)
    run(b.delete("no/such"))   // a no-op, not a throw
    run(b.delete("no/such"))
  }

  test(s"$engine: overwrite is last-write-wins; delete removes") {
    val b = make()
    val _ = run(b.put("k", bytes(10)))
    val _ = run(b.put("k", bytes(20)))
    assertEquals(run(b.head("k")).get.size, 20L)
    run(b.delete("k"))
    assertEquals(run(b.head("k")), None)
  }

  test(s"$engine: counted — the seam counts puts, gets and misses, heads, lists and deletes as a Schema value") {
    val b = Blob.counted(engine, make())
    run(b.put("c/one", bytes(100))): Unit
    drainGet(b.get("c/one")): Unit
    drainGet(b.get("c/absent")): Unit
    run(b.head("c/one")): Unit
    run(b.delete("c/one"))
    val s = b.stats
    assertEquals((s.engine, s.puts, s.gets, s.misses, s.heads, s.deletes, s.failures), (engine, 1L, 2L, 1L, 1L, 1L, 0L))
    assert(okay.codec.Json.write(s).contains("\"misses\":1"))
  }

  test(s"$engine: the plain road — putBytes, putChunk, getBytes, and a range") {
    val b = make()
    val data = Array.tabulate[Byte](10_000)(i => (i % 251).toByte)
    val etag = run(b.putBytes("plain/one", data))
    assertEquals(run(b.getBytes("plain/one")).map(_.toVector), Right(data.toVector))
    assertEquals(run(b.getBytes("plain/one", Some((10L, 20L)))).map(_.toVector),
      Right(data.slice(10, 20).toVector))
    assertEquals(run(b.head("plain/one")).map(m => (m.size, m.etag)), Some((10_000L, etag)))
    assert(run(b.getBytes("plain/none")).left.exists(_.contains("plain/none")))
    val _ = run(b.putChunk("plain/two", ArraySeq.unsafeWrapArray(data)))
    assertEquals(run(b.getBytes("plain/two")).map(_.length), Right(10_000))
    // the program is a VALUE: running getBytes twice answers the same
    // bytes twice, not the bytes twice over
    val twice = b.getBytes("plain/one")
    assertEquals(run(twice).map(_.length), Right(10_000))
    assertEquals(run(twice).map(_.length), Right(10_000))
  }
}
