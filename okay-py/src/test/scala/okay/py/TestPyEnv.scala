package okay.py

import okay.given

/** foreign-managed-env against a LIVE uv (specs/foreign-highlevel.md stage 8) */
class TestPyEnv extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  private lazy val uvOk = scala.util.Try(ProcessBuilder("uv", "--version").start().waitFor() == 0).getOrElse(false)
  override def munitIgnore: Boolean = !uvOk

  private lazy val cache = java.nio.file.Files.createTempDirectory("okay-py-envs")

  test("declared in code, built once, the second provision is the cache") {
    val env = PyEnv(python = "3.12", packages = Map("six" -> "1.16.0"), cache = Some(cache))
    val first = env.provision()
    val ready = env.dir.resolve(".okay-ready")
    val builtAt = java.nio.file.Files.getLastModifiedTime(ready)
    assertEquals(env.provision(), first)
    assertEquals(java.nio.file.Files.getLastModifiedTime(ready), builtAt, "a rebuild")
    val w = env.start()
    try
      given okay.Handler[PyEval] = w.handler
      assert(w.pythonVersion.startsWith("3.12"), w.pythonVersion)
      assertEquals(Py.fn[String]("six:ensure_str")("installed").runWith, Right("installed"))
    finally w.close()
  }

  test("a build that died (no ready mark) is rebuilt, not trusted") {
    val env = PyEnv(python = "3.12", cache = Some(cache))
    java.nio.file.Files.createDirectories(env.dir.resolve("bin")): Unit
    java.nio.file.Files.writeString(env.dir.resolve("half-built"), "x"): Unit
    val py = env.provision()
    assert(java.nio.file.Files.isExecutable(py), s"$py")
    assert(!java.nio.file.Files.exists(env.dir.resolve("half-built")), "the dead build's leftovers survived")
  }

  test("a package that does not resolve refuses at provision, with uv's own words") {
    val env = PyEnv(python = "3.12", packages = Map("okay-no-such-package-xyzzy" -> "1.0"), cache = Some(cache))
    val e = intercept[IllegalStateException](env.provision())
    assert(e.getMessage.contains("could not install the packages"), e.getMessage)
    assert(!java.nio.file.Files.exists(env.dir.resolve(".okay-ready")))
  }
}

/** what the cache is keyed by, without uv (default gate) */
class TestPyEnvKey extends munit.FunSuite {
  test("the key follows the declaration, not the order it was written in") {
    val a = PyEnv(packages = Map("a" -> "1", "b" -> ">=2"))
    val b = PyEnv(packages = Map("b" -> ">=2", "a" -> "1"))
    assertEquals(a.key, b.key)
    assertNotEquals(a.key, PyEnv(packages = Map("a" -> "2", "b" -> ">=2")).key)
    assertEquals(a.requirements, Vector("a==1", "b>=2"))
  }

}
