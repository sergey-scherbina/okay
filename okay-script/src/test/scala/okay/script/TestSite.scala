package okay.script

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response as HttpResponse}

import java.nio.file.{Files, Path}

/** okay-script-site: the container, exercised through `Site.handle`
 * (no port -- the default gate). See specs/okay-script.md "Site — the
 * container". `TestSiteLive` is the same over a real Jetty port.
 */
class TestSite extends munit.FunSuite:

  private var root: Path = null
  private var site: Site = null

  override def beforeAll(): Unit =
    root = Files.createTempDirectory("okay-script-site-")
    site = Site(root)

  override def afterAll(): Unit =
    site.close()
    deleteAll(root)

  private def deleteAll(p: Path): Unit =
    if Files.isDirectory(p) then
      val s = Files.list(p)
      try s.forEach(deleteAll) finally s.close()
    Files.deleteIfExists(p): Unit

  private def page(rel: String, content: String): Unit =
    val f = root.resolve(rel)
    Files.createDirectories(f.getParent)
    Files.writeString(f, content): Unit

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith

  private def header(r: HttpResponse, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  private def headers(r: HttpResponse, name: String): Vector[String] =
    r.headers.collect { case (k, v) if k.equalsIgnoreCase(name) => v }.toVector

  private def get(url: String, hs: Seq[(String, String)] = Nil): HttpResponse =
    site.handle(Request.get(url, hs))

  private def post(url: String, form: String, hs: Seq[(String, String)]): HttpResponse =
    site.handle(Request.post(url, Body.Text(form), ("Content-Type", "application/x-www-form-urlencoded") +: hs))

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  // ---- routing

  test("GET / renders index.md; /shop renders shop.md; /shop/ renders shop/index.md; a missing path is outside routes") {
    page("index.md", "home\n")
    page("shop.md", "shop page\n")
    page("shop/index.md", "shop index\n")
    val r = get("/")
    assertEquals(r.status, 200)
    assert(text(r).contains("home"))
    assert(header(r, "content-type").exists(_.startsWith("text/html")), r.headers.toString)
    assert(text(get("/shop")).contains("shop page"))
    assert(text(get("/shop/")).contains("shop index"))
    assert(!site.routes.isDefinedAt(Request.get("/nope")))
    assertEquals(get("/nope").status, 404)
    assert(site.routes.isDefinedAt(Request.get("/shop?x=1")))
  }

  test("a .. segment never escapes root, and a page source is not served as a file") {
    page("index.md", "home\n")
    assert(!site.routes.isDefinedAt(Request.get("/../index.md")))
    assert(!site.routes.isDefinedAt(Request.get("/%2e%2e/index.md")))
    assert(!site.routes.isDefinedAt(Request.get("/index.md")))
  }

  test("a static file is served with its extension's content type and its exact bytes") {
    page("style.css", "body { color: red }\n")
    val r = get("/style.css")
    assertEquals(r.status, 200)
    assertEquals(header(r, "content-type"), Some("text/css; charset=utf-8"))
    assertEquals(text(r), "body { color: red }\n")
    page("data.bin", "xyz")
    assertEquals(header(get("/data.bin"), "content-type"), Some("application/octet-stream"))
  }

  test("[sku].md answers /product/anything with Web.current.params, and a literal sibling wins") {
    page("product/[sku].md", Api + "Sku: ${Web.current.params(\"sku\")}\n")
    page("product/special.md", "special\n")
    assert(text(get("/product/ok-42")).contains("Sku: ok-42"))
    assert(text(get("/product/special")).contains("special"))
    page("user/[id]/index.md", Api + "User ${Web.current.params(\"id\")}\n")
    assert(text(get("/user/7/")).contains("User 7"))
    assert(text(get("/user/7")).contains("User 7"))
  }

  // ---- request

  test("Web.current carries a urlencoded POST form, the Cookie header, query, and case-insensitive headers") {
    page("form.md", Api + "name=${Web.current.form(\"name\")} q=${Web.current.query.getOrElse(\"q\", \"-\")} " +
      "c=${Web.current.cookies.getOrElse(\"tok\", \"-\")} h=${Web.current.header(\"x-trace\").getOrElse(\"-\")} m=${Web.current.method}\n")
    val r = post("/form?q=1", "name=Ann+Lee&extra=%26", Seq("Cookie" -> "a=1; tok=t9", "X-Trace" -> "abc"))
    assertEquals(r.status, 200)
    val t = text(r)
    assert(t.contains("name=Ann Lee"), t)
    assert(t.contains("q=1"), t)
    assert(t.contains("c=t9"), t)
    assert(t.contains("h=abc"), t)
    assert(t.contains("m=POST"), t)
  }

  // ---- response

  test("Response.current.status / header / contentType reach the wire; redirect answers 302 with Location and no body") {
    page("status.md", Api + "```scala\nResponse.current.status = 404\nResponse.current.header(\"X-Page\", \"yes\")\nResponse.current.contentType(\"text/plain\")\n```\nnot here\n")
    val r = get("/status")
    assertEquals(r.status, 404)
    assertEquals(header(r, "x-page"), Some("yes"))
    assertEquals(header(r, "content-type"), Some("text/plain"))
    assert(text(r).contains("not here"))

    page("redir.md", Api + "```scala\nResponse.current.redirect(\"/cart\")\n```\nignored output\n")
    val r2 = get("/redir")
    assertEquals(r2.status, 302)
    assertEquals(header(r2, "location"), Some("/cart"))
    assertEquals(text(r2), "")
  }

  test("front-matter contentType sets the page's content type") {
    page("plain.md", "---\ncontentType: text/plain; charset=utf-8\n---\njust text\n")
    val r = get("/plain")
    assertEquals(header(r, "content-type"), Some("text/plain; charset=utf-8"))
    assert(text(r).contains("just text"))
  }

  // ---- session

  test("a session set in one request is read in the next via the cookie; untouched sessions get no cookie; invalidate expires it") {
    page("s-set.md", Api + "```scala\nSession.current.set(\"cart\", \"3\")\n```\nset\n")
    page("s-get.md", Api + "cart=${Session.current.get(\"cart\").getOrElse(\"none\")}\n")
    page("s-kill.md", Api + "```scala\nSession.current.invalidate()\n```\nkilled\n")

    val r0 = get("/s-get")
    assertEquals(headers(r0, "set-cookie"), Vector.empty)
    assert(text(r0).contains("cart=none"))

    val r1 = get("/s-set")
    val cookie = headers(r1, "set-cookie").find(_.startsWith(Site.SessionCookie + "=")).getOrElse(fail("no session cookie"))
    assert(cookie.contains("HttpOnly"), cookie)
    val id = cookie.drop(Site.SessionCookie.length + 1).takeWhile(_ != ';')
    assert(id.nonEmpty)
    assertEquals(site.sessions.size, 1)

    val r2 = get("/s-get", Seq("Cookie" -> s"${Site.SessionCookie}=$id"))
    assert(text(r2).contains("cart=3"), text(r2))
    assertEquals(headers(r2, "set-cookie"), Vector.empty)

    val r3 = get("/s-kill", Seq("Cookie" -> s"${Site.SessionCookie}=$id"))
    val expired = headers(r3, "set-cookie").find(_.startsWith(Site.SessionCookie + "=")).getOrElse(fail("no expiring cookie"))
    assert(expired.contains("Max-Age=0"), expired)
    assertEquals(site.sessions.size, 0)
    assert(text(get("/s-get", Seq("Cookie" -> s"${Site.SessionCookie}=$id"))).contains("cart=none"))
  }

  test("a session idle past ttl is gone") {
    val sessions = Sessions(java.time.Duration.ofMillis(50))
    val h = sessions.handle(None, now = 1000)
    h.set("k", "v")
    assertEquals(sessions.size, 1)
    val again = sessions.handle(Some(h.id), now = 1040)
    assertEquals(again.get("k"), Some("v"))
    val late = sessions.handle(Some(h.id), now = 1040 + 51)
    assertEquals(late.get("k"), None)
    assertEquals(sessions.size, 0)
  }

  // ---- include / forward

  test("include renders the named page in place with the same Web and Session; the 17th nesting fails with a message") {
    page("parts/header.md", Api + "[header for ${Web.current.path}]")
    page("inc.md", Api + "```scala\ninclude(\"parts/header.md\")\n```\nbody\n```scala\ninclude(\"/parts/header.md\")\n```\n")
    val t = text(get("/inc"))
    // a text run between two fences carries no trailing newline of its
    // own (the tokenizer's long-standing convention) -- hence no "\n"s
    assert(t.contains("[header for /inc]body[header for /inc]"), t)

    page("self.md", Api + "```scala\ninclude(\"self.md\")\n```\n")
    val r = get("/self")
    assertEquals(r.status, 500)
    assert(text(r).contains("nesting deeper than 16"), text(r))
  }

  test("forward answers with the target page's output and status, none of the forwarding page's") {
    page("fwd.md", Api + "never shown\n```scala\nforward(\"/target\")\n```\n")
    page("target.md", Api + "```scala\nResponse.current.status = 201\n```\ntarget for ${Web.current.path} via ${Web.current.params(\"forwarded\")}\n")
    val r = get("/fwd")
    assertEquals(r.status, 201)
    val t = text(r)
    assert(t.contains("target for /fwd via /target"), t)
    assert(!t.contains("never shown"), t)

    page("fwd-missing.md", Api + "```scala\nforward(\"/no-such-page\")\n```\n")
    assertEquals(get("/fwd-missing").status, 404)
  }

  // ---- declare

  test("a declare val is initialized once across requests, re-initialized after the file changes; a def is callable from an earlier ${expr}") {
    page("decl.md", "n=${next()}\n\n```scala declare\nval counter = new java.util.concurrent.atomic.AtomicInteger\ndef next(): Int = counter.incrementAndGet()\n```\n")
    val f = root.resolve("decl.md")
    assert(text(get("/decl")).contains("n=1"))
    assert(text(get("/decl")).contains("n=2"))
    Files.writeString(f, "m=${next()}\n\n```scala declare\nval counter = new java.util.concurrent.atomic.AtomicInteger\ndef next(): Int = counter.incrementAndGet()\n```\n")
    Files.setLastModifiedTime(f, java.nio.file.attribute.FileTime.fromMillis(System.currentTimeMillis() + 5000))
    assert(text(get("/decl")).contains("m=1"))
  }

  // ---- errors

  test("a throw answers 500 through error.md with Error.current; a compile error too; without error.md, plain text") {
    page("boom.md", "before\n```scala\nthrow new IllegalStateException(\"kaboom\")\n```\n")
    page("bad.md", "```scala\nval x: Int = \"no\"\n```\n")
    val r0 = get("/boom")
    assertEquals(r0.status, 500)
    assert(header(r0, "content-type").exists(_.startsWith("text/plain")), r0.headers.toString)
    assert(text(r0).contains("kaboom"), text(r0))

    page("error.md", Api + "oops: ${Error.current.map(_.message).getOrElse(\"?\")} at ${Web.current.path}\n")
    val r1 = get("/boom")
    assertEquals(r1.status, 500)
    assert(header(r1, "content-type").exists(_.startsWith("text/html")), r1.headers.toString)
    assert(text(r1).contains("oops: java.lang.IllegalStateException: kaboom at /boom"), text(r1))

    val r2 = get("/bad")
    assertEquals(r2.status, 500)
    assert(text(r2).contains("oops: L2:"), text(r2))

    page("own-error.md", Api + "custom: ${Error.current.map(_.errors.size).getOrElse(-1)}\n")
    page("bad2.md", "---\nerrorPage: own-error.md\n---\n```scala\nval y: String = 1\n```\n")
    assert(text(get("/bad2")).contains("custom: 1"), text(get("/bad2")))

    page("error.md", "```scala\nthrow new RuntimeException(\"error page broken\")\n```\n")
    val r3 = get("/boom")
    assertEquals(r3.status, 500)
    assert(header(r3, "content-type").exists(_.startsWith("text/plain")))
    assert(text(r3).contains("kaboom") && text(r3).contains("error page broken"), text(r3))
    Files.delete(root.resolve("error.md"))
  }

  // ---- concurrency

  test("two requests rendering the same page on two threads each get their own output and Web") {
    page("who.md", Api + "```scala\nThread.sleep(30)\n```\nyou are ${Web.current.query(\"me\")}\n")
    get("/who?me=warm"): Unit
    val results = new java.util.concurrent.ConcurrentHashMap[String, String]
    val threads = (1 to 4).map { i =>
      val t = new Thread(() => results.put(s"t$i", text(get(s"/who?me=t$i"))): Unit)
      t.start(); t
    }
    threads.foreach(_.join())
    for i <- 1 to 4 do
      val out = results.get(s"t$i")
      assert(out != null && out.contains(s"you are t$i") && !out.contains("you are t" + (i % 4 + 1)), s"t$i -> $out")
  }
