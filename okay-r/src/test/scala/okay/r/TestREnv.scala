package okay.r

import okay.given

/** foreign-managed-env against a LIVE R with network (specs/foreign-highlevel.md stage 8) */
class TestREnv extends munit.FunSuite {

  override def munitTests(): Seq[Test] = super.munitTests().map(_.tag(new munit.Tag("Live")))
  override def munitIgnore: Boolean = TestR.rscript.isEmpty

  // under java.io.tmpdir: the container shim mounts it
  private lazy val cache = java.nio.file.Files.createTempDirectory("okay-r-libs")

  test("a CRAN package declared in code: installed once into a keyed library, required by the session") {
    val env = REnv(packages = Seq("praise"), rscript = TestR.rscript.get, cache = Some(cache))
    val lib = env.provision()
    val builtAt = java.nio.file.Files.getLastModifiedTime(lib.resolve(".okay-ready"))
    assertEquals(env.provision(), lib)
    assertEquals(java.nio.file.Files.getLastModifiedTime(lib.resolve(".okay-ready")), builtAt, "a rebuild")
    val r = env.start()
    try
      given okay.Handler[REval] = r.handler
      val word = R.fn[String]("praise::praise")("${Adjective}").runWith
      assert(word.exists(_.nonEmpty), s"$word")
    finally r.close()
  }

  test("a package CRAN does not have refuses at provision, naming it") {
    val env = REnv(packages = Seq("okayNoSuchPackageXyzzy"), rscript = TestR.rscript.get, cache = Some(cache))
    val e = intercept[IllegalStateException](env.provision())
    assert(e.getMessage.contains("okayNoSuchPackageXyzzy"), e.getMessage)
  }
}

/** what the library is keyed by, without R (default gate) */
class TestREnvKey extends munit.FunSuite {
  test("keyed by the declaration: order does not matter, the repository does") {
    assertEquals(REnv(Seq("b", "a")).key, REnv(Seq("a", "b")).key)
    assertNotEquals(REnv(Seq("a")).key, REnv(Seq("a"), repos = "https://example.org").key)
  }
}
