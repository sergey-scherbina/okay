package okay.lake

import okay.{!, Async}
import okay.given
import okay.blob.{S3, SigV4}
import okay.http.{Method, Request, Transports}

/**
 * THE LAKE BATTERY OVER S3 (stage 17), on a Live MinIO at 127.0.0.1:9000
 * (`docker run -p 9000:9000 minio/minio server /data`); it skips where
 * none answers. The size is `OKAY_LAKE_MB` of trips (each input object
 * two row groups) — the operator's 100 GB is that knob on a machine with
 * the disk for it; the default keeps a shared box's gate short.
 */
class TestLakeS3 extends LakeSuite:
  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(3600, "s")
  override def munitIgnore: Boolean = !up

  lazy val up: Boolean =
    try { val s = java.net.Socket(); s.connect(java.net.InetSocketAddress("127.0.0.1", 9000), 300); s.close(); true }
    catch case _: Exception => false

  /** about 40 bytes a trip in Parquet: `OKAY_LAKE_MB` spread over the objects */
  override def rowsPer: Int =
    val mb = sys.env.get("OKAY_LAKE_MB").flatMap(_.toIntOption).getOrElse(16)
    math.max(2000, (mb.toLong * 1024 * 1024 / 40 / objects).toInt)

  private val creds = SigV4.Creds("minioadmin", "minioadmin")
  private val http = Transports.http()

  def lake(): String =
    val bucket = s"okay-lake-${System.nanoTime()}"
    val stamp = java.time.format.DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss'Z'")
      .withZone(java.time.ZoneOffset.UTC).format(java.time.Instant.now)
    val auth = SigV4.sign("PUT", s"/$bucket", Nil, Seq("host" -> "127.0.0.1:9000"), SigV4.emptyHash, "us-east-1", stamp, creds)
    val r = !.run(Async.run[okay.http.Response, Nothing](http.send(Request(Method.Put, s"http://127.0.0.1:9000/$bucket", auth))))
    assert(r.ok, s"bucket create: HTTP ${r.status}")
    Lakes.register(bucket, S3(http, "http://127.0.0.1:9000", bucket, "us-east-1", creds))
    bucket

  def root(name: String): String = s"s3://$name"

  /** DuckDB with httpfs and a secret for the MinIO — the extension is
   * DuckDB's own download, so a box that cannot fetch it skips */
  override def duck(): java.sql.Connection =
    val c = super.duck()
    val st = c.createStatement()
    try st.execute("install httpfs; load httpfs")
    catch case e: java.sql.SQLException =>
      c.close()
      assume(false, s"DuckDB's httpfs extension is not available here: ${e.getMessage}")
    st.execute("create secret (type s3, key_id 'minioadmin', secret 'minioadmin', " +
      "endpoint '127.0.0.1:9000', url_style 'path', use_ssl false, region 'us-east-1')")
    c
