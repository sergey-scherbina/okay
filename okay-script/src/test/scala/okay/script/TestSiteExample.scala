package okay.script

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response as HttpResponse}

import java.nio.file.{Files, Paths}

/** The worked example, okay-script/examples/site, driven through
 * `Site.handle` -- keeps the example honest without a port. See
 * specs/okay-script.md "Site — the container".
 */
class TestSiteExample extends munit.FunSuite:

  // the forked test JVM's working directory is the module's, an
  // unforked one's the repo root -- same idiom as the other examples
  private val root = Vector("examples/site", "okay-script/examples/site").map(Paths.get(_))
    .find(Files.isDirectory(_)).getOrElse(Paths.get("examples/site"))
  private lazy val site = Site(root)

  override def afterAll(): Unit = site.close()

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith

  private def header(r: HttpResponse, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }

  private def sessionCookie(r: HttpResponse): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(Site.SessionCookie + "=") => v.takeWhile(_ != ';') }

  test("the example store: catalog, product page, session cart via POST-redirect-GET, clear via forward, error page, css") {
    assume(Files.isDirectory(root), s"run from the repo root: $root not found")

    val home = site.handle(Request.get("/"))
    assertEquals(home.status, 200)
    val h = text(home)
    assert(h.contains("<h1>Okay Store</h1>") || h.contains("# Okay Store"), h)
    assert(h.contains("/product/ok-2") && h.contains("Cart (0)"), h)

    val product = text(site.handle(Request.get("/product/ok-2")))
    assert(product.contains("<h1>ok-2</h1>") && product.contains("$25"), product)
    assertEquals(site.handle(Request.get("/product/nope")).status, 404)

    val form = Seq("Content-Type" -> "application/x-www-form-urlencoded")
    val add = site.handle(Request.post("/cart", Body.Text("sku=ok-2"), form))
    assertEquals(add.status, 302)
    assertEquals(header(add, "location"), Some("/cart"))
    val sid = sessionCookie(add).getOrElse(fail("no session cookie"))

    val cart = text(site.handle(Request.get("/cart", Seq("Cookie" -> sid))))
    assert(cart.contains("<li>ok-2</li>") && cart.contains("Cart (1)"), cart)

    val cleared = site.handle(Request.post("/clear", Body.Empty, Seq("Cookie" -> sid)))
    assertEquals(cleared.status, 200)
    val c = text(cleared)
    assert(c.contains("Empty.") && c.contains("Cart (0)"), c)
    assert(sessionCookie(cleared).exists(_.endsWith("=")), cleared.headers.toString)

    val live = text(site.handle(Request.get("/live")))
    assert(live.contains("yes: 0   no: 0") && live.contains("""okayLive("poll")"""), live)

    val css = site.handle(Request.get("/style.css"))
    assertEquals(header(css, "content-type"), Some("text/css; charset=utf-8"))
  }
