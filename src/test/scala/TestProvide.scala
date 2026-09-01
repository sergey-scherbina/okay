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

/** the generated arities (the Cats answer): 8 mid-scale, 22 = the cap */
class TestProvideN extends munit.FunSuite {
  case class C1(v: Int); case class C2(v: Int); case class C3(v: Int)
  case class C4(v: Int); case class C5(v: Int); case class C6(v: Int)
  case class C7(v: Int); case class C8(v: Int); case class C9(v: Int)
  case class C10(v: Int); case class C11(v: Int); case class C12(v: Int)
  case class C13(v: Int); case class C14(v: Int); case class C15(v: Int)
  case class C16(v: Int); case class C17(v: Int); case class C18(v: Int)
  case class C19(v: Int); case class C20(v: Int); case class C21(v: Int)
  case class C22(v: Int)

  test("arity 8") {
    def app(using a: C1, b: C2, c: C3, d: C4, e: C5, f: C6, g: C7, h: C8): Int =
      a.v + b.v + c.v + d.v + e.v + f.v + g.v + h.v
    assertEquals(provide(C1(1), C2(2), C3(3), C4(4), C5(5), C6(6), C7(7), C8(8))(app), 36)
  }

  test("arity 22 — the platform's own cap, like cats mapN") {
    def app(using c1: C1, c2: C2, c3: C3, c4: C4, c5: C5, c6: C6, c7: C7,
            c8: C8, c9: C9, c10: C10, c11: C11, c12: C12, c13: C13, c14: C14,
            c15: C15, c16: C16, c17: C17, c18: C18, c19: C19, c20: C20,
            c21: C21, c22: C22): Int =
      c1.v + c2.v + c3.v + c4.v + c5.v + c6.v + c7.v + c8.v + c9.v + c10.v +
      c11.v + c12.v + c13.v + c14.v + c15.v + c16.v + c17.v + c18.v + c19.v +
      c20.v + c21.v + c22.v
    assertEquals(provide(C1(1), C2(2), C3(3), C4(4), C5(5), C6(6), C7(7),
      C8(8), C9(9), C10(10), C11(11), C12(12), C13(13), C14(14), C15(15),
      C16(16), C17(17), C18(18), C19(19), C20(20), C21(21), C22(22))(app), 253)
  }
}
