import okay.*

/**
 * The installer half of the capability pair (ctx-everywhere):
 * expression-scoped, nests to the NEAREST, and with the doors it is
 * the DI story — a missing dependency is a COMPILE error, quoted.
 */
class TestProvide extends munit.FunSuite {

  trait Db { def q: String }
  trait Log { def tag: String }
  def app(using db: Db, log: Log): String = s"${log.tag}:${db.q}"

  test("provide installs for a block, in expression position") {
    val prod = new Db { val q = "prod-row" }
    val logged = new Log { val tag = "info" }
    assertEquals(provide(prod, logged)(app), "info:prod-row")
  }

  test("nesting resolves to the NEAREST provide — the override story") {
    val outer = new Db { val q = "outer" }
    val inner = new Db { val q = "inner" }
    val log = new Log { val tag = "t" }
    val got = provide(outer, log) {
      (provide(inner)(app), app)   // inner wins inside; outer after
    }
    assertEquals(got, ("t:inner", "t:outer"))
  }

  test("the DI claim: one program, two environments; a missing dependency does not COMPILE") {
    val test = new Db { val q = "stub" }
    val quiet = new Log { val tag = "quiet" }
    assertEquals(provide(test, quiet)(app), "quiet:stub")
    val errors = compileErrors("provide(new Db { val q = \"x\" })(app)")
    assert(errors.nonEmpty, "the missing Log compiled")
  }

  test("three capabilities at once") {
    trait Clock { def now: Long }
    def stamped(using db: Db, log: Log, c: Clock) = s"${log.tag}@${c.now}:${db.q}"
    val out = provide(new Db { val q = "r" }, new Log { val tag = "l" },
      new Clock { val now = 42L })(stamped)
    assertEquals(out, "l@42:r")
  }
}
