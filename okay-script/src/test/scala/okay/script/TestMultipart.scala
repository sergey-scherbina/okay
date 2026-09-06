package okay.script

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response as HttpResponse}

import java.nio.charset.StandardCharsets.ISO_8859_1
import java.nio.file.{Files, Path}

/** okay-script-multipart: multipart/form-data uploads reach a page as
 * `Web.parts` / `Web.file` / `Web.form`. See specs/okay-script.md
 * "Uploads".
 */
class TestMultipart extends munit.FunSuite:

  private val boundary = "----okayBoundary42"

  /** a body the way a browser frames it; `file` is raw bytes */
  private def multipart(fields: Seq[(String, String)], file: Option[(String, String, String, Array[Byte])],
                        preamble: String = "", finalCrlf: Boolean = true): Array[Byte] =
    val out = new java.io.ByteArrayOutputStream
    def w(s: String): Unit = out.write(s.getBytes(ISO_8859_1))
    w(preamble)
    for (k, v) <- fields do
      w(s"--$boundary\r\nContent-Disposition: form-data; name=\"$k\"\r\n\r\n$v\r\n")
    file.foreach { (field, name, ct, bytes) =>
      w(s"--$boundary\r\ncontent-disposition: form-data; name=\"$field\"; filename=\"$name\"\r\nContent-Type: $ct\r\n\r\n")
      out.write(bytes)
      w("\r\n")
    }
    w(s"--$boundary--")
    if finalCrlf then w("\r\n")
    out.toByteArray

  private val binary: Array[Byte] = Array[Byte](0x89.toByte, 'P', 'N', 'G', '\r', '\n', 0, 0, '-', '-', 'x', '\r', '\n', 1)

  test("parse: a text field and a binary file part, byte-identical, with filename and content type") {
    val parts = Multipart.parse(multipart(Seq("title" -> "Okay tee"), Some(("img", "tee.png", "image/png", binary))), boundary)
    assertEquals(parts.map(_.name), Vector("title", "img"))
    assertEquals(parts(0).text, "Okay tee")
    assert(!parts(0).isFile)
    assertEquals(parts(1).filename, Some("tee.png"))
    assertEquals(parts(1).contentType, Some("image/png"))
    assert(parts(1).bytes.sameElements(binary), parts(1).bytes.toVector.toString)
  }

  test("parse: quoted boundary in the Content-Type, a preamble, no final CRLF, odd header case; no boundary -> no parts") {
    assertEquals(Multipart.boundaryOf(s"""multipart/form-data; boundary="$boundary""""), Some(boundary))
    assertEquals(Multipart.boundaryOf(s"multipart/form-data; Boundary=$boundary"), Some(boundary))
    assertEquals(Multipart.boundaryOf("multipart/form-data"), None)
    val body = multipart(Seq("a" -> "1", "b" -> "two"), None, preamble = "ignored preamble\r\n", finalCrlf = false)
    assertEquals(Multipart.parse(body, boundary).map(p => p.name -> p.text), Vector("a" -> "1", "b" -> "two"))
    assertEquals(Multipart.parse("nothing here".getBytes(ISO_8859_1), boundary), Vector.empty)
    assertEquals(Multipart.parse(Array.empty[Byte], boundary), Vector.empty)
  }

  test("through Site.handle: form(\"title\") and file(\"img\") from one multipart POST; the file is not in form") {
    val root = Files.createTempDirectory("okay-script-multipart-")
    val site = Site(root)
    try
      Files.writeString(root.resolve("upload.md"),
        "```scala\nimport okay.script.api.*\nval f = Web.current.file(\"img\").get\n```\n" +
          "title=${Web.current.form(\"title\")} name=${f.filename.get} ct=${f.contentType.get} " +
          "size=${f.bytes.length} same=${f.bytes.sameElements(Array[Byte](0x89.toByte, 'P', 'N', 'G', '\\r', '\\n', 0, 0, '-', '-', 'x', '\\r', '\\n', 1))} " +
          "inform=${Web.current.form.contains(\"img\")} parts=${Web.current.parts.size}\n"): Unit
      val body = multipart(Seq("title" -> "Okay tee"), Some(("img", "tee.png", "image/png", binary)))
      val req = Request.post("/upload", Body.Bytes(scala.collection.immutable.ArraySeq.unsafeWrapArray(body)),
        Seq("Content-Type" -> s"multipart/form-data; boundary=$boundary"))
      val r: HttpResponse = site.handle(req)
      val t = Async.run[String, Pure](Http.text(r)).runWith
      assertEquals(r.status, 200, t)
      assert(t.contains("title=Okay tee"), t)
      assert(t.contains("name=tee.png") && t.contains("ct=image/png"), t)
      assert(t.contains(s"size=${binary.length}") && t.contains("same=true"), t)
      assert(t.contains("inform=false") && t.contains("parts=2"), t)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
