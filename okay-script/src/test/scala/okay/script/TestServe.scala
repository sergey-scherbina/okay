package okay.script

import okay.*
import okay.given
import okay.jetty.Jetty

import java.net.{HttpURLConnection, URI}
import java.nio.file.{Files, Path}

/** okay-script-serve: `Site.serve(port)` is the whole server, and
 * `Serve` is the stock entry point. The port-binding test is Live.
 */
class TestServe extends munit.FunSuite:

  test("Serve.parse: a directory and an optional port; OKAY_DATA names the store; errors by name") {
    val dir = Files.createTempDirectory("okay-script-serve-")
    try
      val noEnv: String => Option[String] = _ => None
      assertEquals(Serve.parse(Array(dir.toString), noEnv).map(a => (a.port, a.data)), Right((8080, None)))
      assertEquals(Serve.parse(Array(dir.toString, "9000"), noEnv).map(_.port), Right(9000))
      assertEquals(Serve.parse(Array(dir.toString), k => Option.when(k == "OKAY_DATA")("/tmp/x")).map(_.data.map(_.toString)), Right(Some("/tmp/x")))
      assert(Serve.parse(Array(dir.toString, "lots"), noEnv).left.exists(_.contains("not a port")))
      assert(Serve.parse(Array(dir.resolve("nope").toString), noEnv).left.exists(_.contains("not a directory")))
      assert(Serve.parse(Array.empty, noEnv).left.exists(_.startsWith("usage")))
    finally Files.deleteIfExists(dir): Unit
  }

  test("Site.serve(0) answers a page over a real port; Serve.site with OKAY_DATA persists the application scope".tag(new munit.Tag("Live"))) {
    val root = Files.createTempDirectory("okay-script-serve-pages-")
    val data = Files.createTempDirectory("okay-script-serve-data-")
    Files.writeString(root.resolve("index.md"), "```scala\nimport okay.script.api.*\nApplication.current.set(\"hits\", (Application.current.get(\"hits\").map(_.toInt).getOrElse(0) + 1).toString)\n```\nhits=${Application.current.get(\"hits\").get}\n"): Unit
    def get(url: String): (Int, String) =
      val c = URI.create(url).toURL.openConnection() match
        case h: HttpURLConnection => h
        case o => throw new IllegalStateException(o.toString)
      val code = c.getResponseCode
      val body = new String((if code >= 400 then c.getErrorStream else c.getInputStream).readAllBytes(), "UTF-8")
      c.disconnect()
      (code, body)
    def once(): String =
      val a = Serve.parse(Array(root.toString, "0"), k => Option.when(k == "OKAY_DATA")(data.toString)).toOption.get
      val site = Serve.site(a)
      try
        Resource.run[String, Pure](site.serve(0).map { server =>
          val (code, body) = get(s"http://127.0.0.1:${Jetty.port(server)}/")
          assertEquals(code, 200)
          body
        }).runWith
      finally site.close()
    try
      assert(once().contains("hits=1"))
      assert(once().contains("hits=2"), "the application scope did not survive the restart")
    finally
      Files.walk(root).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
      Files.walk(data).sorted(java.util.Comparator.reverseOrder[Path]()).forEach(p => Files.deleteIfExists(p): Unit)
  }
