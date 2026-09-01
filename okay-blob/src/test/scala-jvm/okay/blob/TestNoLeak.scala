package okay.blob

import okay.{!, Async, pure}
import okay.given
import okay.http.{Http, Request, Response}

/** the conf invariants at this seam: the SECRET key reaches the HMAC
 * chain and nothing else — no URL, no header, no error. (The access
 * key travels in Authorization by design; the secret never.) */
class TestNoLeak extends munit.FunSuite {

  val canary = "SUPER-SECRET-CANARY-42"

  test("the secret appears in no request the engine sends, and in no error it raises") {
    var seen = Vector.empty[Request]
    val recording = new Http:
      def send(r: Request): Response ! Async =
        seen = seen :+ r
        pure(Response(404, Nil, okay.http.Http.one(Array.empty)))
    val s3 = S3(recording, "http://s3.example", "bucket", "us-east-1",
      SigV4.Creds("AKIAEXAMPLE", canary))

    def run[A](p: A ! Async): A = !.run(Async.run[A, Nothing](p))
    assertEquals(run(s3.head("k")), None)
    run(s3.delete("k"))
    val err = intercept[IllegalStateException](
      run(s3.put("k", pure(okay.Chunks.emptyChunk))))
    assert(!err.getMessage.contains(canary))

    for r <- seen do
      assert(!r.url.contains(canary), r.url)
      for (k, v) <- r.headers do assert(!v.contains(canary), s"$k: $v")
      assert(!String(r.body.bytes, "UTF-8").contains(canary))
    assert(seen.nonEmpty)
  }
}
