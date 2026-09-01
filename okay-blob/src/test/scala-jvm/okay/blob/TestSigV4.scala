package okay.blob

/**
 * The AWS documentation's own SigV4 examples ("Authenticating
 * Requests: Using the Authorization Header"), all three: the
 * algorithm pinned by the standard's vectors, not by trust. The
 * shared fixture is the documented one — 2013-05-24, examplebucket,
 * us-east-1, the EXAMPLE keypair.
 */
class TestSigV4 extends munit.FunSuite {

  val creds = SigV4.Creds("AKIAIOSFODNN7EXAMPLE",
    "wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY")
  val stamp = "20130524T000000Z"
  val host = "examplebucket.s3.amazonaws.com"

  def signatureOf(headers: Seq[(String, String)]): String =
    headers.collectFirst { case ("authorization", v) => v }.get
      .split("Signature=").last

  test("the GET object example signs to the documented signature") {
    val out = SigV4.sign("GET", "/test.txt", Nil,
      Seq("host" -> host, "range" -> "bytes=0-9"),
      SigV4.emptyHash, "us-east-1", stamp, creds)
    assertEquals(signatureOf(out),
      "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41")
  }

  test("the PUT object example signs to the documented signature") {
    val payload = "Welcome to Amazon S3."
    val out = SigV4.sign("PUT", "/test$file.text", Nil,
      Seq("host" -> host,
        "date" -> "Fri, 24 May 2013 00:00:00 GMT",
        "x-amz-storage-class" -> "REDUCED_REDUNDANCY"),
      SigV4.sha256Hex(payload.getBytes("UTF-8")), "us-east-1", stamp, creds)
    assertEquals(signatureOf(out),
      "98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd")
  }

  test("the list-objects example signs to the documented signature") {
    val out = SigV4.sign("GET", "/", Seq("max-keys" -> "2", "prefix" -> "J"),
      Seq("host" -> host), SigV4.emptyHash, "us-east-1", stamp, creds)
    assertEquals(signatureOf(out),
      "34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7")
  }

  test("uriEncode: the s3 rules — unreserved kept, slash kept in paths only") {
    assertEquals(SigV4.uriEncode("test$file.text"), "test%24file.text")
    assertEquals(SigV4.uriEncode("a/b c+d", keepSlash = true), "a/b%20c%2Bd")
    assertEquals(SigV4.uriEncode("a/b"), "a%2Fb")
    assertEquals(SigV4.uriEncode("тест"), "%D1%82%D0%B5%D1%81%D1%82")
  }
}
