package okay.blob

import okay.{!, Async}
import okay.given
import okay.http.{Method, Request, Transports}

/**
 * The S3 engine against MinIO — the TestLive pattern: the suite runs
 * where a MinIO answers on 127.0.0.1:9000 (minioadmin/minioadmin,
 * the default dev credentials) and SKIPS where it does not, so a red
 * here means the endpoint died, not the code. It is the SAME
 * BlobContract the fs engine passes — the seam's whole claim.
 *
 *   docker run -p 9000:9000 minio/minio server /data
 */
object TestLiveS3:
  val endpoint = "http://127.0.0.1:9000"
  lazy val up: Boolean =
    try
      val s = java.net.Socket()
      s.connect(java.net.InetSocketAddress("127.0.0.1", 9000), 300)
      s.close(); true
    catch case _: Exception => false

class TestLiveS3 extends BlobContract("s3") {

  override def munitIgnore: Boolean = !TestLiveS3.up

  private val creds = SigV4.Creds("minioadmin", "minioadmin")
  private val http = Transports.http()
  private var n = 0

  def make(): Blob =
    n += 1
    val bucket = s"okay-test-$n-${System.currentTimeMillis}"
    // the bucket is the test's fixture; creating it is one signed PUT
    val stamp = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(java.time.ZoneOffset.UTC).format(java.time.Instant.now)
    val auth = SigV4.sign("PUT", s"/$bucket", Nil,
      Seq("host" -> "127.0.0.1:9000"), SigV4.emptyHash, "us-east-1", stamp, creds)
    val r = !.run(Async.run[okay.http.Response, Nothing](
      http.send(Request(Method.Put, s"${TestLiveS3.endpoint}/$bucket", auth))))
    assert(r.ok, s"bucket create: HTTP ${r.status}")
    S3(http, TestLiveS3.endpoint, bucket, "us-east-1", creds)
}
