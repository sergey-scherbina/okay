package okay.script

import okay.*
import okay.given
import okay.http.{Http, Request, Response as HttpResponse}

import java.nio.file.{Files, Path}

/** specs/site-framework.md stage 1: a definition crosses a file. A
 * page exports names, a page imports them with a markdown link.
 */
class TestModules extends munit.FunSuite:

  private def withSite[A](files: (String, String)*)(f: (Path, Site) => A): A =
    val root = Files.createTempDirectory("okay-script-modules-")
    for (name, text) <- files do
      val p = root.resolve(name)
      Files.createDirectories(p.getParent)
      Files.writeString(p, text): Unit
    val site = Site(root)
    try f(root, site)
    finally
      site.close()
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)

  private def text(r: HttpResponse): String = Async.run[String, Pure](Http.text(r)).runWith
  private def get(site: Site, url: String): (Int, String) =
    val r = site.handle(Request.get(url))
    (r.status, text(r))

  private val money =
    """---
      |exports:
      |  - money
      |  - vat
      |route: false
      |---
      |
      |# money
      |
      |```scala declare
      |def money(cents: Long): String = f"${cents / 100.0}%.2f zł"
      |def vat(cents: Long): Long = cents * 23 / 100
      |private def secret: String = "not yours"
      |```
      |""".stripMargin

  test("a page calls a def from another page -- in a marker, in a body block and in its own declare") {
    withSite(
      "lib/money.md" -> money,
      "index.md" ->
        """[money, vat](/lib/money.md)
          |
          |```scala declare
          |def total(c: Long): String = money(c + vat(c))
          |```
          |price: ${money(1999)}
          |```scala
          |println("with vat: " + total(1999))
          |```
          |""".stripMargin,
    ) { (_, site) =>
      val (status, body) = get(site, "/")
      assertEquals(status, 200)
      assert(body.contains("price: 19.99 zł"), body)
      assert(body.contains("with vat: 24.58 zł"), body)   // 1999 + 1999*23/100, integer
    }
  }

  test("the import is BY NAME: what is not asked for is not in scope, and what is not exported is an error naming both") {
    withSite(
      "lib/money.md" -> money,
      "a.md" ->
        """[money](/lib/money.md)
          |${vat(100)}
          |""".stripMargin,
      "b.md" ->
        """[money, secret](/lib/money.md)
          |${money(1)}
          |""".stripMargin,
    ) { (_, site) =>
      val (_, a) = get(site, "/a")
      assert(a.contains("Not found") || a.contains("vat"), a)
      assert(!a.contains("23"), s"vat was in scope without being imported: $a")
      val (_, b) = get(site, "/b")
      assert(b.contains("does not export secret"), b)
      assert(b.contains("money.md"), b)
      assert(b.contains("it exports money, vat"), b)
    }
  }

  test("a relative target resolves against the importing page; a leading slash from the root") {
    withSite(
      "lib/money.md" -> money,
      "shop/near.md" ->
        """[money](../lib/money.md)
          |${money(500)}
          |""".stripMargin,
      "shop/far.md" ->
        """[money](/lib/money.md)
          |${money(700)}
          |""".stripMargin,
      "shop/missing.md" ->
        """[money](./nowhere.md)
          |${money(1)}
          |""".stripMargin,
    ) { (_, site) =>
      assert(get(site, "/shop/near")._2.contains("5.00 zł"), get(site, "/shop/near")._2)
      assert(get(site, "/shop/far")._2.contains("7.00 zł"), get(site, "/shop/far")._2)
      val (_, missing) = get(site, "/shop/missing")
      assert(missing.contains("no such module") && missing.contains("nowhere.md"), missing)
    }
  }

  test("a module imports a module; a diamond compiles the shared one ONCE") {
    withSite(
      "lib/money.md" -> money,
      "lib/left.md" ->
        """---
          |exports: [left]
          |route: false
          |---
          |[money](/lib/money.md)
          |```scala declare
          |def left(c: Long): String = "L" + money(c)
          |```
          |""".stripMargin,
      "lib/right.md" ->
        """---
          |exports: [right]
          |route: false
          |---
          |[money](/lib/money.md)
          |```scala declare
          |def right(c: Long): String = "R" + money(c)
          |```
          |""".stripMargin,
      "index.md" ->
        """[left](/lib/left.md)
          |[right](/lib/right.md)
          |${left(100)} ${right(200)}
          |""".stripMargin,
    ) { (_, site) =>
      val (status, body) = get(site, "/")
      assertEquals(status, 200, body)
      assert(body.contains("L1.00 zł") && body.contains("R2.00 zł"), body)
    }
  }

  test("a cycle is refused with the ring named, not a stack overflow") {
    withSite(
      "lib/a.md" ->
        """---
          |exports: [a]
          |route: false
          |---
          |[b](/lib/b.md)
          |```scala declare
          |def a: String = "a" + b
          |```
          |""".stripMargin,
      "lib/b.md" ->
        """---
          |exports: [b]
          |route: false
          |---
          |[a](/lib/a.md)
          |```scala declare
          |def b: String = "b"
          |```
          |""".stripMargin,
      "index.md" ->
        """[a](/lib/a.md)
          |${a}
          |""".stripMargin,
    ) { (_, site) =>
      val (_, body) = get(site, "/")
      assert(body.contains("cycle"), body)
      assert(body.contains("a.md") && body.contains("b.md"), body)
    }
  }

  test("route: false makes a module unroutable while its importer renders") {
    withSite(
      "lib/money.md" -> money,
      "index.md" ->
        """[money](/lib/money.md)
          |${money(1)}
          |""".stripMargin,
    ) { (_, site) =>
      assertEquals(get(site, "/lib/money")._1, 404)
      assertEquals(get(site, "/")._1, 200)
    }
  }

  test("editing a module re-renders the pages that import it") {
    withSite(
      "lib/money.md" -> money,
      "index.md" ->
        """[money](/lib/money.md)
          |${money(1999)}
          |""".stripMargin,
    ) { (root, site) =>
      assert(get(site, "/")._2.contains("19.99 zł"), get(site, "/")._2)
      Thread.sleep(1100)   // mtime resolution, not a race in the code
      Files.writeString(root.resolve("lib/money.md"), money.replace("zł", "PLN")): Unit
      val after = get(site, "/")._2
      assert(after.contains("19.99 PLN"), s"the importer kept a stale module: $after")
    }
  }

  test("a module's compile error is reported against the MODULE's own file") {
    withSite(
      "lib/broken.md" ->
        """---
          |exports: [broken]
          |route: false
          |---
          |```scala declare
          |def broken: String = notAThing
          |```
          |""".stripMargin,
      "index.md" ->
        """[broken](/lib/broken.md)
          |${broken}
          |""".stripMargin,
    ) { (_, site) =>
      val (_, body) = get(site, "/")
      assert(body.contains("broken.md"), body)
      assert(body.contains("notAThing"), body)
    }
  }

  test("an import line is a DEPENDENCY, not content: it never reaches the page's output") {
    withSite(
      "lib/money.md" -> money,
      "index.md" ->
        """[money, vat](/lib/money.md)
          |
          |# Shop
          |
          |price: ${money(100)}
          |
          |See [the price list](/prices.html) for more.
          |""".stripMargin,
    ) { (_, site) =>
      val (status, body) = get(site, "/")
      assertEquals(status, 200, body)
      assert(!body.contains("[money, vat]"), s"the import was printed into the page:\n$body")
      assert(!body.contains("/lib/money.md"), body)
      // ...and an ordinary markdown link in a SENTENCE is prose, and stays
      assert(body.contains("""See [the price list](/prices.html) for more."""), body)
      assert(body.contains("price: 1.00 zł"), body)
    }
  }
