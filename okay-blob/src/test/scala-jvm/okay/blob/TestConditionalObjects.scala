package okay.blob

import okay.Async
import okay.freer.{!, pure}
import okay.given
import okay.freer.given
import okay.http.{Http, Request, Response}

class TestConditionalObjects extends okay.testkit.Munit.Diagnosed:
  private def run[A](p: A ! Async): A = !.run(Async.run[A, okay.freer.Pure](p))

  test("create-only condition is signed; 412 is distinct from 409 and errors") {
    var requests = Vector.empty[Request]
    var status = 200
    var releases = 0
    val http = new Http:
      def send(r: Request): Response ! Async =
        requests :+= r
        pure(Response(status, Nil, Http.one(Array.empty), release = okay.async { releases += 1 }))
    val s3 = S3(http, "https://example.test", "bucket", "us-east-1", SigV4.Creds("access", "secret"))
    assertEquals(run(s3.create("marker", Array[Byte](1))), ConditionalObjects.Created.Added)
    status = 412
    assertEquals(run(s3.create("marker", Array[Byte](1))), ConditionalObjects.Created.Exists)
    for code <- Vector(409, 403, 500) do
      status = code
      note(s"HTTP $code must not fall back to unconditional PUT")
      intercept[IllegalStateException](run(s3.create("marker", Array[Byte](1))))
    assertEquals(requests.size, 5)
    assertEquals(releases, 5)
    for r <- requests do
      assert(r.headers.contains("if-none-match" -> "*"))
      val md5 = java.util.Base64.getEncoder.encodeToString(java.security.MessageDigest.getInstance("MD5").digest(r.body.bytes))
      assert(r.headers.contains("content-md5" -> md5))
      assert(r.headers.find(_._1 == "authorization").exists(_._2.contains("content-md5;host;if-none-match;x-amz-content-sha256;x-amz-date")))
  }

  test("read bounds actual body, releases on failure and distinguishes absence") {
    var status = 200
    var releases = 0
    val http = new Http:
      def send(r: Request): Response ! Async =
        note(s"GET ${r.url} status=$status")
        pure(Response(status, Nil, Http.one(Array[Byte](1, 2, 3)), release = okay.async { releases += 1 }))
    val s3 = S3(http, "https://example.test", "bucket", "us-east-1", SigV4.Creds("access", "secret"))
    assertEquals(run(s3.read("k", 3)).map(_.toVector), Some(Vector[Byte](1, 2, 3)))
    val _ = intercept[IllegalStateException](run(s3.read("k", 2)))
    status = 404
    assertEquals(run(s3.read("k", 3)), None)
    status = 403
    val _ = intercept[IllegalStateException](run(s3.read("k", 3)))
    assertEquals(releases, 4)
    val _ = intercept[IllegalArgumentException](s3.read("k", -1))
    val _ = intercept[IllegalArgumentException](s3.create("k", new Array[Byte](ConditionalObjects.MaxBytes + 1)))
    assertEquals(releases, 4)
  }
