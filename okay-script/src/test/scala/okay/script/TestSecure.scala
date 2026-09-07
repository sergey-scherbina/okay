package okay.script

import okay.*
import okay.given
import okay.http.{Body, Http, Request, Response as HttpResponse}
import okay.security.SessionIssuer

import java.nio.file.{Files, Path}

/** okay-script-secure: a page's `secure:` front-matter, enforced by
 * the container with the deployment's verifier. See
 * specs/okay-script.md "Declarative security".
 */
class TestSecure extends munit.FunSuite:

  private val issuer = SessionIssuer()
  private val admin = issuer.issue("ann", scopes = Set("admin"))
  private val user = issuer.issue("bob", scopes = Set("shop"))

  private val Api = "```scala\nimport okay.script.api.*\n```\n"

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def header(r: HttpResponse, name: String): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase(name) => v }
  private def sessionCookie(r: HttpResponse): Option[String] =
    r.headers.collectFirst { case (k, v) if k.equalsIgnoreCase("set-cookie") && v.startsWith(Site.SessionCookie + "=") => v.takeWhile(_ != ';') }

  private def withRoot[A](verify: Option[String => okay.security.Verified])(body: (Site, Path) => A): A =
    val root = Files.createTempDirectory("okay-script-secure-")
    def page(rel: String, content: String): Unit =
      val f = root.resolve(rel)
      Files.createDirectories(f.getParent)
      Files.writeString(f, content): Unit
    page("secret.md", "---\nsecure: admin\n---\n" + Api + "hello ${Principal.current.map(_.id).getOrElse(\"?\")}\n")
    page("members.md", "---\nsecure: any\n---\n" + Api + "member ${Principal.current.map(_.id).getOrElse(\"?\")}\n")
    page("open.md", "open\n")
    val site = Site(root, verify = verify)
    try body(site, root)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  test("secure: on a Site without a verifier answers 500, not an open door") {
    withRoot(None) { (site, _) =>
      val r = site.handle(Request.get("/secret"))
      assertEquals(r.status, 500)
      assert(text(r).contains("without a verifier"), text(r))
      assertEquals(site.handle(Request.get("/open")).status, 200)
    }
  }

  test("no token: 401 + WWW-Authenticate without a login page; 302 to /login?next= with one; loginPage: overrides") {
    withRoot(Some(issuer.verify(_))) { (site, root) =>
      val r = site.handle(Request.get("/secret"))
      assertEquals(r.status, 401)
      assert(header(r, "www-authenticate").exists(_.contains("""error="invalid_token"""")), r.headers.toString)

      Files.writeString(root.resolve("login.md"), "login form\n"): Unit
      val r2 = site.handle(Request.get("/secret"))
      assertEquals(r2.status, 302)
      assertEquals(header(r2, "location"), Some("/login?next=%2Fsecret"))

      Files.createDirectories(root.resolve("auth"))
      Files.writeString(root.resolve("auth/index.md"), "other login\n"): Unit
      Files.writeString(root.resolve("vip.md"), "---\nsecure: admin\nloginPage: auth/index.md\n---\nvip\n"): Unit
      assertEquals(header(site.handle(Request.get("/vip")), "location"), Some("/auth/?next=%2Fvip"))
    }
  }

  test("a Bearer header: the scope lets in and Principal.current names the subject; the wrong scope is 403; any takes any valid token") {
    withRoot(Some(issuer.verify(_))) { (site, _) =>
      val ok = site.handle(Request.get("/secret", Seq("Authorization" -> s"Bearer $admin")))
      assertEquals(ok.status, 200)
      assert(text(ok).contains("hello ann"), text(ok))

      val no = site.handle(Request.get("/secret", Seq("Authorization" -> s"Bearer $user")))
      assertEquals(no.status, 403)
      assert(header(no, "www-authenticate").exists(_.contains("insufficient_scope")), no.headers.toString)

      val bad = site.handle(Request.get("/secret", Seq("Authorization" -> "Bearer not-a-token")))
      assertEquals(bad.status, 401)

      val any = site.handle(Request.get("/members", Seq("Authorization" -> s"Bearer $user")))
      assertEquals(any.status, 200)
      assert(text(any).contains("member bob"), text(any))
    }
  }

  test("the browser flow: login(token) on a login page, the cookie lets the next request in, logout() sends it back") {
    withRoot(Some(issuer.verify(_))) { (site, root) =>
      Files.writeString(root.resolve("login.md"),
        Api + "```scala\nWeb.current.form.get(\"token\").foreach(login)\nResponse.current.redirect(Web.current.query.getOrElse(\"next\", \"/\"))\n```\n"): Unit
      Files.writeString(root.resolve("bye.md"), Api + "```scala\nlogout()\n```\nbye\n"): Unit
      val form = Seq("Content-Type" -> "application/x-www-form-urlencoded")
      val in = site.handle(Request.post("/login?next=%2Fsecret", Body.Text(s"token=$admin"), form))
      assertEquals(in.status, 302)
      assertEquals(header(in, "location"), Some("/secret"))
      val cookie = sessionCookie(in).getOrElse(fail("no session cookie"))

      val page = site.handle(Request.get("/secret", Seq("Cookie" -> cookie)))
      assertEquals(page.status, 200)
      assert(text(page).contains("hello ann"), text(page))

      assertEquals(site.handle(Request.get("/bye", Seq("Cookie" -> cookie))).status, 200)
      val again = site.handle(Request.get("/secret", Seq("Cookie" -> cookie)))
      assertEquals(again.status, 302)
    }
  }

  test("a forward INTO a secure page is checked; an include of one is the author's composition") {
    withRoot(Some(issuer.verify(_))) { (site, root) =>
      Files.writeString(root.resolve("jump.md"), Api + "```scala\nforward(\"/secret\")\n```\n"): Unit
      Files.writeString(root.resolve("wrap.md"), Api + "```scala\ninclude(\"secret.md\")\n```\n"): Unit
      assertEquals(site.handle(Request.get("/jump")).status, 401)
      assertEquals(site.handle(Request.get("/jump", Seq("Authorization" -> s"Bearer $admin"))).status, 200)
      val wrapped = site.handle(Request.get("/wrap"))
      assertEquals(wrapped.status, 200)
      assert(text(wrapped).contains("hello ?"), text(wrapped))
    }
  }
