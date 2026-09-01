package okay.conf

import okay.codec.Schema

/** the file: half — mounts, newlines, misses; JVM (the Native leg
 * compiles the same Platform source) */
class TestConfFiles extends munit.FunSuite {

  def tmp(content: String): String =
    val p = java.nio.file.Files.createTempFile("okay-conf", ".secret")
    java.nio.file.Files.write(p, content.getBytes("UTF-8"))
    p.toFile.deleteOnExit()
    p.toString

  test("file: resolves content with exactly one trailing newline trimmed") {
    assertEquals(Secrets.file.get(Secret(s"file:${tmp("hunter2\n")}")), Right("hunter2"))
    // ending in two keeps one
    assertEquals(Secrets.file.get(Secret(s"file:${tmp("hunter2\n\n")}")), Right("hunter2\n"))
    assertEquals(Secrets.file.get(Secret(s"file:${tmp("no-newline")}")), Right("no-newline"))
  }

  test("a missing path and a directory each refuse naming the path") {
    assertEquals(Secrets.file.get(Secret("file:/no/such/okay/path")),
      Left("'/no/such/okay/path' does not exist"))
    val dir = java.nio.file.Files.createTempDirectory("okay-conf").toString
    assertEquals(Secrets.file.get(Secret(s"file:$dir")), Left(s"'$dir' is a directory"))
  }

  test("Conf.load reads a config file end to end") {
    final case class App(name: String, token: Secret)
    given Schema[App] = Schema.derived
    val path = tmp("""{"name":"svc","token":"env:TOKEN"}""")
    assertEquals(Conf.load[App](path), Right(App("svc", Secret("env:TOKEN"))))
    assert(Conf.load[App]("/no/such/file").isLeft)
  }

  test("the edge pattern: load, resolve, hand to a constructor — the whole gap") {
    final case class Db(url: String, user: String, password: Secret)
    given Schema[Db] = Schema.derived
    val path = tmp("""{"url":"jdbc:h2:mem:x","user":"app","password":"env:PATH"}""")
    val wired =
      for
        db <- Conf.load[Db](path)
        pw <- Secrets.env.get(db.password)
      yield (db.url, db.user, pw.nonEmpty)
    assertEquals(wired.map(w => (w._1, w._2, w._3)), Right(("jdbc:h2:mem:x", "app", true)))
  }
}
