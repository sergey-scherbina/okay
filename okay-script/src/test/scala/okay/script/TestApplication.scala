package okay.script

import okay.*
import okay.given
import okay.codec.Schema
import okay.http.{Http, Request, Response as HttpResponse}
import okay.persist.MemoryStore
import okay.script.api.Application
import okay.security.SessionIssuer

import java.nio.file.{Files, Path}

/** okay-script-application: JSP's application scope for a Site, and
 * `signIn` through the container's issuer. See specs/okay-script.md
 * "Application scope".
 */
class TestApplication extends munit.FunSuite:

  final case class Product(sku: String, price: Int)
  given Schema[Product] = Schema.derived

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def sessionCookie(r: HttpResponse): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(Site.SessionCookie + "=") => v.takeWhile(_ != ';') }

  test("two pages of one Site share an attribute; value/put round a case class; damage reads None") {
    val root = Files.createTempDirectory("okay-script-app-")
    Files.writeString(root.resolve("set.md"), Api + "```scala\nApplication.current.set(\"motd\", \"hi\")\n```\nset\n"): Unit
    Files.writeString(root.resolve("get.md"), Api + "motd=${Application.current.get(\"motd\").getOrElse(\"none\")}\n"): Unit
    val site = Site(root)
    try
      assert(text(site.handle(Request.get("/get"))).contains("motd=none"))
      site.handle(Request.get("/set")): Unit
      assert(text(site.handle(Request.get("/get"))).contains("motd=hi"))
      site.application.put("p", Product("ok-1", 10))
      assertEquals(site.application.value[Product]("p"), Some(Product("ok-1", 10)))
      site.application.set("p", "{not json")
      assertEquals(site.application.value[Product]("p"), None)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }

  test("persisted over a MemoryStore: set, reopen, read; remove, reopen, gone") {
    val store = MemoryStore()
    val a = Application.persisted(store)
    a.put("catalog", Vector(Product("ok-1", 10)))
    a.set("motd", "hi")
    val b = Application.persisted(store)
    assertEquals(b.value[Vector[Product]]("catalog"), Some(Vector(Product("ok-1", 10))))
    assertEquals(b.get("motd"), Some("hi"))
    b.remove("motd")
    val c = Application.persisted(store)
    assertEquals(c.get("motd"), None)
    assertEquals(c.attributes.keySet, Set("catalog"))
  }

  test("signIn on a Site with issue: the login page's cookie lets the secure page in; without issue it is a 500 naming the fix") {
    val root = Files.createTempDirectory("okay-script-app-")
    Files.writeString(root.resolve("login.md"), Api + "```scala\nsignIn(\"ann\", Set(\"admin\"))\n```\nsigned\n"): Unit
    Files.writeString(root.resolve("secret.md"), "---\nsecure: admin\n---\n" + Api + "hello ${Principal.current.map(_.id).getOrElse(\"?\")}\n"): Unit
    val issuer = SessionIssuer()
    val site = Site(root, verify = Some(issuer.verify(_)), issue = Some((s, sc) => issuer.issue(s, sc)))
    val bare = Site(root, verify = Some(issuer.verify(_)))
    try
      assertEquals(site.handle(Request.get("/secret")).status, 302)
      val in = site.handle(Request.get("/login"))
      assertEquals(in.status, 200)
      val cookie = sessionCookie(in).getOrElse(fail("no cookie"))
      val page = site.handle(Request.get("/secret", Seq("Cookie" -> cookie)))
      assertEquals(page.status, 200)
      assert(text(page).contains("hello ann"), text(page))

      val r = bare.handle(Request.get("/login"))
      assertEquals(r.status, 500)
      assert(text(r).contains("mints no tokens"), text(r))
    finally
      site.close()
      bare.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
