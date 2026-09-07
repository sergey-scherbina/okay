package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}

import java.nio.file.{Files, Path}

/** okay-script-warm: compiling the directory at boot, and what a Site
 * counts. See specs/okay-script.md "Warm and stats".
 */
class TestWarm extends munit.FunSuite:

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith

  private def withRoot[A](body: (Path, (String, String) => Unit) => A): A =
    val root = Files.createTempDirectory("okay-script-warm-")
    def page(rel: String, c: String): Unit =
      val f = root.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.writeString(f, c): Unit
    try body(root, page)
    finally Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("warm compiles every page, names the broken ones, and leaves the good ones ready to render") {
    withRoot { (root, page) =>
      page("index.md", "home\n")
      page("parts/header.md", "[header]\n")
      page("index.uk.md", "домівка\n")
      page("bad.md", "```scala\nval x: Int = \"no\"\n```\n")
      page("i18n/en.yaml", "greet: hi\n")
      val site = Site(root, languages = Vector("en", "uk"))
      try
        val broken = site.warm()
        assertEquals(broken.map(_._1), Vector("bad.md"))
        assert(broken.head._2.exists(_.contains("L2")), broken.head._2.mkString("; "))
        // four pages compiled: index, index.uk, bad, parts/header -- the
        // yaml under i18n/ is messages, not a page
        assertEquals(site.stats.compiles, 4L)
        assertEquals(site.stats.pagesHeld, 4)

        // a warmed page renders without compiling again
        val before = site.stats.compiles
        assert(text(site.handle(Request.get("/"))).contains("home"))
        assertEquals(site.stats.compiles, before)
        // and the broken one still serves its error, rather than the
        // site refusing to exist
        assertEquals(site.handle(Request.get("/bad")).status, 500)
      finally site.close()
    }
  }

  test("stats count what happened, and the two renderings are the same numbers") {
    withRoot { (root, page) =>
      page("index.md", "home\n")
      page("secret.md", "---\nsecure: admin\n---\nhidden\n")
      page("boom.md", Api + "```scala\nthrow new RuntimeException(\"x\")\n```\n")
      page("style.css", "body{}\n")
      val site = Site(root, verify = Some(_ => okay.security.Verified.No("nope")))
      try
        site.handle(Request.get("/")): Unit
        site.handle(Request.get("/")): Unit
        site.handle(Request.get("/style.css")): Unit
        val etag = site.handle(Request.get("/style.css")).headers.collectFirst { case (k, v) if k.equalsIgnoreCase("etag") => v }.get
        site.handle(Request.get("/style.css", Seq("If-None-Match" -> etag))): Unit
        site.handle(Request.get("/secret")): Unit
        site.handle(Request.get("/nope")): Unit
        site.handle(Request.get("/boom")): Unit

        val s = site.stats
        // a refusal and a failure ARE page requests -- the counter is
        // named for what it counts, which is why it is not `renders`
        assertEquals(s.pageRequests, 4L)   // index x2, secret (401), boom (500)
        assertEquals(s.statics, 3L)
        assertEquals(s.notModified, 1L)
        assertEquals(s.refused, 1L)
        assertEquals(s.notFound, 1L)
        assertEquals(s.failed, 1L)
        assert(s.pagesHeld >= 2, s.toString)

        assert(s.json.contains(s""""pageRequests":${s.pageRequests}"""), s.json)
        val prom = s.prometheus
        assert(prom.contains(s"okay_script_page_requests_total ${s.pageRequests}"), prom)
        assert(prom.contains("# TYPE okay_script_pages_held gauge"), prom)
        assert(prom.contains(s"okay_script_not_modified_total ${s.notModified}"), prom)
      finally site.close()
    }
  }

  test("ops routes are opt-in: not in `routes`, and answering when chained") {
    withRoot { (root, page) =>
      page("index.md", "home\n")
      val site = Site(root)
      try
        assert(!site.routes.isDefinedAt(Request.get("/metrics")))
        val all = site.routes orElse site.opsRoutes
        assert(all.isDefinedAt(Request.get("/metrics")))
        val health = Async.run[HttpResponse, Pure](all(Request.get("/healthz"))).runWith
        assertEquals(health.status, 200)
        assert(text(health).contains("live=true"))
        val stats = Async.run[HttpResponse, Pure](all(Request.get("/stats"))).runWith
        assert(text(stats).startsWith("{\"pageRequests\":"), text(stats))
        // a PAGE of that name wins: the site's own content is never
        // shadowed by an endpoint the deployment chose to add
        page("metrics.md", "my own metrics page\n")
        val site2 = Site(root)
        try
          val chained = site2.routes orElse site2.opsRoutes
          assert(text(Async.run[HttpResponse, Pure](chained(Request.get("/metrics"))).runWith).contains("my own metrics page"))
        finally site2.close()
      finally site.close()
    }
  }

  test("Serve.parse reads OKAY_OPS") {
    val root = Files.createTempDirectory("okay-script-warm-args-")
    try
      assertEquals(Serve.parse(Array(root.toString), _ => None).map(_.ops), Right(false))
      assertEquals(Serve.parse(Array(root.toString), k => Option.when(k == "OKAY_OPS")("1")).map(_.ops), Right(true))
      assertEquals(Serve.parse(Array(root.toString), k => Option.when(k == "OKAY_OPS")("true")).map(_.ops), Right(true))
      assertEquals(Serve.parse(Array(root.toString), k => Option.when(k == "OKAY_OPS")("no")).map(_.ops), Right(false))
    finally Files.deleteIfExists(root): Unit
  }
