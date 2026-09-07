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
  private val issuer = okay.security.SessionIssuer()
  private lazy val site = Site(root, verify = Some(issuer.verify(_)), issue = Some((s, sc) => issuer.issue(s, sc)))

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
    assert(product.contains("<h1>Direct style, no ceremony</h1>") && product.contains("$25"), product)
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

    val checkout = text(site.handle(Request.get("/checkout")))
    assert(checkout.contains("""<form method="post" action="/checkout">""") && checkout.contains("""name="email""""), checkout)
    val placed = text(site.handle(Request.post("/checkout", Body.Text("name=Ann&email=a%40b.c&qty=2&gift=on"), form)))
    assert(placed.contains("Thanks, Ann! 2 item(s) on the way, gift-wrapped."), placed)
    val tooMany = text(site.handle(Request.post("/checkout", Body.Text("name=Ann&email=a%40b.c&qty=9"), form)))
    assert(tooMany.contains("! only 5 in stock"), tooMany)

    // the admin: 302 to the login page without a cookie; the demo password
    // signs in through the container's issuer; a product posted on the
    // plain road lands in the application scope, so the index shows it
    assertEquals(header(site.handle(Request.get("/admin")), "location"), Some("/login?next=%2Fadmin"))
    val wrong = site.handle(Request.post("/login?next=%2Fadmin", Body.Text("password=nope"), form))
    assert(text(wrong).contains("Wrong password"), text(wrong))
    val signed = site.handle(Request.post("/login?next=%2Fadmin", Body.Text("password=okay"), form))
    assertEquals(signed.status, 302)
    val adminCookie = sessionCookie(signed).getOrElse(fail("no cookie on sign-in"))
    val admin = text(site.handle(Request.get("/admin", Seq("Cookie" -> adminCookie))))
    assert(admin.contains("Admin — admin") && admin.contains("ok-1:") && admin.contains("""okayLive("adder")"""), admin)
    val added = text(site.handle(Request.post("/admin", Body.Text("sku=ok-9&name=New+thing&price=7"), form ++ Seq("Cookie" -> adminCookie))))
    assert(added.contains("saved ok-9") && added.contains("ok-9: New thing"), added)
    assert(text(site.handle(Request.get("/"))).contains("/product/ok-9"))
    assert(text(site.handle(Request.get("/product/ok-9"))).contains("<h1>New thing</h1>"))
    val rejected = text(site.handle(Request.post("/admin", Body.Text("sku=ok-0&name=Free&price=0"), form ++ Seq("Cookie" -> adminCookie))))
    assert(rejected.contains("! must be positive"), rejected)
    assertEquals(site.handle(Request.get("/logout", Seq("Cookie" -> adminCookie))).status, 302)
    assertEquals(site.handle(Request.get("/admin", Seq("Cookie" -> adminCookie))).status, 302)

    val css = site.handle(Request.get("/style.css"))
    assertEquals(header(css, "content-type"), Some("text/css; charset=utf-8"))
  }
