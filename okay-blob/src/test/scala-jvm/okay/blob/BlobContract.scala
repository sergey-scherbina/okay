package okay.blob

import okay.{!, +, Async, Chunk, Produce, Stream, effect, pure}
import okay.given
import scala.collection.immutable.ArraySeq

/**
 * The contract every engine must pass (the StoreSuite pattern): fs
 * runs it now, the S3 engine re-runs it at blob-s3 against MinIO.
 * Extend and provide `make`.
 */
abstract class BlobContract(engine: String) extends munit.FunSuite {

  def make(): Blob

  private type F = Produce + Async

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** a producer of `total` deterministic bytes in `piece`-sized chunks */
  def bytes(total: Int, piece: Int = 8 * 1024): Chunk[Byte] ! F =
    def go(off: Int): Chunk[Byte] ! F =
      if off >= total then pure(okay.Chunks.emptyChunk)
      else
        val n = math.min(piece, total - off)
        val a = Array.tabulate[Byte](n)(i => ((off + i) % 251).toByte)
        effect[F, Chunk[Byte]](ArraySeq.unsafeWrapArray(a)).flatMap(_ => go(off + n))
    go(0)

  /** drain a get: the collected bytes, the outcome, and the LARGEST
   * chunk seen — the constant-memory witness */
  /** uncons would lose the program's final value at its None, and
   * get's OUTCOME is that value — so this walks the tree itself */
  def drainGet(p: Either[String, Unit] ! F): (Array[Byte], Either[String, Unit], Int) =
    val out = java.io.ByteArrayOutputStream()
    var biggest = 0
    val outcome = run(walk(p, c => { out.write(c.toArray); biggest = math.max(biggest, c.length) }))
    (out.toByteArray, outcome, biggest)

  private def walk[A](p: A ! F, each: Chunk[Byte] => Unit): A ! Async =
    import okay.!.*
    (p.resume: @unchecked) match
      case Pure(a) => okay.pure(a)
      case Effect(e) => okay.<|>[Async, Produce](e) match
        case Left(a) => effect[Async, A](a.asInstanceOf[Async[A]])
        case Right(c) =>
          each(c.asInstanceOf[Chunk[Byte]])
          okay.pure(c.asInstanceOf[A])   // a terminal produce answers its value
      case Bind(Effect(e), k) => okay.<|>[Async, Produce](e) match
        case Left(a) =>
          effect[Async, Any](a.asInstanceOf[Async[Any]]).flatMap(x => walk(k(x.asInstanceOf), each))
        case Right(c) =>
          each(c.asInstanceOf[Chunk[Byte]])
          walk(k(c.asInstanceOf), each)

  def drainList(p: Chunk[Meta] ! F): Vector[Meta] =
    val S = summon[Stream[[X] =>> X ! F, Async]]
    def go(rest: Chunk[Meta] ! F): Vector[Meta] ! Async =
      S.uncons(rest).flatMap {
        case None => pure(Vector.empty)
        case Some((c, more)) => go(more).map(c.toVector ++ _)
      }
    run(go(p))

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
}
