package okay.blob

import okay.{!, Async, Chunk, Source, Writer}
import okay.given
import okay.http.{Method, Request, Transports}
import scala.collection.immutable.ArraySeq

/**
 * A PUT HOLDS ONE PART (s3-multipart-put): an object several parts
 * long goes up as an S3 multipart upload, one part in memory, and comes
 * back byte for byte; a put that fails mid-way leaves no object and no
 * upload open. Live, against MinIO on 127.0.0.1:9000, like TestLiveS3.
 */
class TestS3Multipart extends munit.FunSuite {
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(300, "s")
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = !TestLiveS3.up

  private val creds = SigV4.Creds("minioadmin", "minioadmin")
  private val http = Transports.http()

  def bucket(): String =
    val name = s"okay-mp-${System.nanoTime()}"
    val stamp = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(java.time.ZoneOffset.UTC).format(java.time.Instant.now)
    val auth = SigV4.sign("PUT", s"/$name", Nil,
      Seq("host" -> "127.0.0.1:9000"), SigV4.emptyHash, "us-east-1", stamp, creds)
    val r = !.run(Async.run[okay.http.Response, Nothing](
      http.send(Request(Method.Put, s"${TestLiveS3.endpoint}/$name", auth))))
    assert(r.ok, s"bucket create: HTTP ${r.status}")
    name

  def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))

  /** byte `i` of the object: a pure function, so the bytes need not be held */
  def byteAt(i: Long): Byte =
    var z = i / 8 + 0x9e3779b97f4a7c15L
    z = (z ^ (z >>> 30)) * 0xbf58476d1ce4e5b9L
    z = (z ^ (z >>> 27)) * 0x94d049bb133111ebL
    ((z ^ (z >>> 31)) >>> ((i % 8) * 8)).toByte

  /** `n` bytes in chunks of 64 KiB, generated as they are pulled; it
   * throws once `failAt` bytes have gone, when asked to */
  def source(n: Long, failAt: Long = Long.MaxValue): Source[Chunk[Byte]] =
    Source.unfold(0L) { at =>
      if at >= n then None
      else if at >= failAt then throw IllegalStateException(s"the source failed at byte $at")
      else
        val len = math.min(64 * 1024L, n - at).toInt
        Some((ArraySeq.unsafeWrapArray(Array.tabulate(len)(k => byteAt(at + k))), at + len))
    }

  test("an object of four parts goes up in parts and comes back byte for byte") {
    val s3 = S3(http, TestLiveS3.endpoint, bucket(), "us-east-1", creds, partSize = S3.MinPart)
    val n = 3L * S3.MinPart + 123457
    val etag = run(s3.put("big/object", source(n)))
    // S3's etag of a multipart object ends in the number of its parts
    assert(etag.value.endsWith("-4"), s"not a four-part upload: ${etag.value}")
    assertEquals(run(s3.head("big/object")).map(_.size), Some(n))
    var at = 0L
    var bad = -1L
    val out = Writer.fold[Chunk[Byte], Unit, Either[String, Unit], Async](s3.get("big/object"))(
      using summon)(using summon, okay.Fold(())((_, c) =>
        for b <- c do { if bad < 0 && b != byteAt(at) then bad = at; at += 1 }))
    run(out)
    assertEquals(at, n)
    assertEquals(bad, -1L, s"byte $bad differs")
  }

  test("a small object is still one PUT") {
    val s3 = S3(http, TestLiveS3.endpoint, bucket(), "us-east-1", creds, partSize = S3.MinPart)
    val etag = run(s3.put("small", source(1000)))
    assert(!etag.value.contains("-"), etag.value)
  }

  test("a put that fails mid-way leaves no object and no upload open") {
    val name = bucket()
    val s3 = S3(http, TestLiveS3.endpoint, name, "us-east-1", creds, partSize = S3.MinPart)
    val e = intercept[IllegalStateException](run(s3.put("half/object", source(4L * S3.MinPart, failAt = 2L * S3.MinPart + 1))))
    assert(e.getMessage.contains("failed at byte"), e.getMessage)
    assertEquals(run(s3.head("half/object")), None, "a half-written object is visible")
    assertEquals(run(s3.pending("half/object")), Vector.empty, "the failed put left its upload open")
  }

  test("an upload a dead process left open is found and abandoned") {
    val name = bucket()
    val s3 = S3(http, TestLiveS3.endpoint, name, "us-east-1", creds, partSize = S3.MinPart)
    // what a killed writer leaves: an upload started and never finished
    val opened = run(s3.begin("orphan/object"))
    assertEquals(run(s3.pending("orphan/object")).map(_._1), Vector("orphan/object"))
    run(s3.abandon("orphan/object", opened))
    assertEquals(run(s3.pending("orphan/object")), Vector.empty)
  }
}
