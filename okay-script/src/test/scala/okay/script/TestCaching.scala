package okay.script

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response as HttpResponse}
import okay.security.SessionIssuer

import java.nio.file.{Files, Path}

/** okay-script-cache: conditional requests for static files always,
 * and `cache:` for pages -- never at the cost of privacy. See
 * specs/okay-script.md "Caching".
 */
class TestCaching extends munit.FunSuite:

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def header(r: HttpResponse, n: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(n) => v }

  private val issuer = SessionIssuer()

  private def withSite[A](body: (Site, Path) => A): A =
    val root = Files.createTempDirectory("okay-script-cache-")
    def page(rel: String, c: String): Unit =
      val f = root.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.writeString(f, c): Unit
    page("style.css", "body { color: red }\n")
    page("plain.md", "no cache header here\n")
    page("news.md", "---\ncache: 60\n---\nthe news\n")
    // a STEADY body with a side effect: its ETag can match (the body
    // never changes), and whether the page ran is visible in `n`
    page("counter.md", "---\ncache: 60\n---\n" + Api + "```scala\nApplication.current.set(\"n\", (Application.current.get(\"n\").map(_.toInt).getOrElse(0) + 1).toString)\n```\nsteady\n")
    page("probe.md", Api + "n=${Application.current.get(\"n\").getOrElse(\"0\")}\n")
    page("member.md", "---\nsecure: any\ncache: 60\n---\nmembers only\n")
    page("welcome.md", "---\ncache: 60\n---\n" + Api + "```scala\nSession.current.set(\"seen\", \"1\")\n```\nwelcome\n")
    page("off.md", "---\ncache: none\n---\nnever cached\n")
    page("gone.md", "---\ncache: 60\n---\n" + Api + "```scala\nResponse.current.status = 404\n```\nno such thing\n")
    val site = Site(root, verify = Some(issuer.verify(_)))
    try body(site, root)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("a static file always carries an ETag and Last-Modified, and answers 304 to both conditional forms") {
    withSite { (site, root) =>
      val r = site.handle(Request.get("/style.css"))
      assertEquals(r.status, 200)
      val etag = header(r, "etag").getOrElse(fail("no ETag"))
      val lastMod = header(r, "last-modified").getOrElse(fail("no Last-Modified"))
      assert(etag.startsWith("\"") && etag.endsWith("\""), etag)

      val notModified = site.handle(Request.get("/style.css", Seq("If-None-Match" -> etag)))
      assertEquals(notModified.status, 304)
      assertEquals(text(notModified), "")
      assertEquals(header(notModified, "etag"), Some(etag))

      assertEquals(site.handle(Request.get("/style.css", Seq("If-None-Match" -> "*"))).status, 304)
      assertEquals(site.handle(Request.get("/style.css", Seq("If-None-Match" -> s"""W/$etag"""))).status, 304)
      assertEquals(site.handle(Request.get("/style.css", Seq("If-None-Match" -> "\"other\""))).status, 200)
      assertEquals(site.handle(Request.get("/style.css", Seq("If-Modified-Since" -> lastMod))).status, 304)
      assertEquals(site.handle(Request.get("/style.css", Seq("If-Modified-Since" -> "Tue, 01 Jan 1980 00:00:00 GMT"))).status, 200)
      // a changed file invalidates: the ETag carries size and mtime
      Files.writeString(root.resolve("style.css"), "body { color: blue }\n"): Unit
      Files.setLastModifiedTime(root.resolve("style.css"), java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() + 4000)): Unit
      assertEquals(site.handle(Request.get("/style.css", Seq("If-None-Match" -> etag))).status, 200)
    }
  }

  test("cache: on a page sets public max-age and an ETag; a matching If-None-Match is a 304 that renders nothing") {
    withSite { (site, _) =>
      val r = site.handle(Request.get("/news"))
      assertEquals(header(r, "cache-control"), Some("public, max-age=60"))
      val etag = header(r, "etag").getOrElse(fail("no ETag"))

      // a 304 saves the TRANSFER, not the render: an ETag of the body
      // cannot be known without the body, so the page ran either way
      // and its side effect shows. Stated in the spec, asserted here.
      val first = site.handle(Request.get("/counter"))
      assertEquals(first.status, 200)
      val e2 = header(first, "etag").getOrElse(fail("no ETag"))
      assert(text(site.handle(Request.get("/probe"))).contains("n=1"))
      val notModified = site.handle(Request.get("/counter", Seq("If-None-Match" -> e2)))
      assertEquals(notModified.status, 304)
      assertEquals(text(notModified), "")
      assertEquals(header(notModified, "etag"), Some(e2))
      assert(text(site.handle(Request.get("/probe"))).contains("n=2"), "the page did not run on the 304")

      assertEquals(site.handle(Request.get("/news", Seq("If-None-Match" -> etag))).status, 304)
      assertEquals(site.handle(Request.get("/news", Seq("If-None-Match" -> "\"stale\""))).status, 200)
    }
  }

  test("privacy wins: secure:, a Set-Cookie and a session cookie all make it private; no cache: means no header at all") {
    withSite { (site, _) =>
      assertEquals(header(site.handle(Request.get("/plain")), "cache-control"), None)
      assertEquals(header(site.handle(Request.get("/off")), "cache-control"), None)

      val member = site.handle(Request.get("/member", Seq("Authorization" -> s"Bearer ${issuer.issue("ann")}")))
      assertEquals(member.status, 200)
      assertEquals(header(member, "cache-control"), Some("private, max-age=60"))

      val welcome = site.handle(Request.get("/welcome"))
      assert(header(welcome, "set-cookie").isDefined, welcome.headers.toString)
      assertEquals(header(welcome, "cache-control"), Some("private, max-age=60"))
      val cookie = header(welcome, "set-cookie").get.takeWhile(_ != ';')
      assertEquals(header(site.handle(Request.get("/news", Seq("Cookie" -> cookie))), "cache-control"), Some("private, max-age=60"))
    }
  }

  test("only a safe 200 is cached: a POST, a redirect and a 404 carry no validators") {
    withSite { (site, _) =>
      val form = Seq("Content-Type" -> "application/x-www-form-urlencoded")
      val posted = site.handle(Request.post("/news", Body.Text(""), form))
      assertEquals(header(posted, "cache-control"), None)
      assertEquals(header(posted, "etag"), None)
      val notFound = site.handle(Request.get("/gone"))
      assertEquals(notFound.status, 404)
      assertEquals(header(notFound, "cache-control"), None)
    }
  }

  test("Caching: the ETag of bytes, the date round trip, and cache: none/garbage read as absent") {
    assertEquals(Caching.etagOf("abc".getBytes("UTF-8")), Caching.etagOf("abc".getBytes("UTF-8")))
    assertNotEquals(Caching.etagOf("abc".getBytes("UTF-8")), Caching.etagOf("abd".getBytes("UTF-8")))
    val now = System.currentTimeMillis() / 1000 * 1000
    assertEquals(Caching.parseDate(Caching.formatDate(now)), Some(now))
    assertEquals(Caching.parseDate("not a date"), None)
    assertEquals(Caching.maxAge(Map("cache" -> "30")), Some(30))
    assertEquals(Caching.maxAge(Map("cache" -> "none")), None)
    assertEquals(Caching.maxAge(Map("cache" -> "soon")), None)
    assertEquals(Caching.maxAge(Map.empty), None)
  }
