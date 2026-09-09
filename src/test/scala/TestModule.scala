package okay

import okay.given

/** Modules: installers that acquire in a scope (specs/di.md, stage 0). */
class TestModule extends munit.FunSuite {

  trait Db { def q: String }
  trait Pool { def db: Db; def size: Int }
  trait Log { def tag: String }

  def run[A](p: A ! Resource): A = !.run(Resource.run[A, Nothing](p))

  test("modules acquire left to right and release in reverse at the end of the scope") {
    var log = List.empty[String]
    def open(n: String) = module[String]({ log ::= s"open $n"; n })(r => log ::= s"close $r")
    val got = run((open("a") and open("b")) { wire[String] })
    assertEquals(got, "b")   // right is the inner layer: nearest wins
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("a module's acquisition reads the module before it — the dependency graph") {
    var closed = List.empty[String]
    val db = module[Db](new Db { val q = "row" })(_ => closed ::= "db")
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](new Pool { val db = wire[Db]; val size = 4 })(_ => closed ::= "pool")
    val got = run((db and pool) { s"${wire[Pool].db.q}/${wire[Pool].size}" })
    assertEquals(got, "row/4")
    assertEquals(closed, List("db", "pool"))
  }

  test("a test double overrides by composing to the right, and builds nothing") {
    var opened = 0
    val real = module[Log]({ opened += 1; new Log { val tag = "prod" } })(_ => ())
    val fake = Module.value[Log](new Log { val tag = "test" })
    val base = real and Module.value[Db](new Db { val q = "r" })
    assertEquals(run((base and fake) { wire[Log].tag + wire[Db].q }), "testr")
    assertEquals(opened, 1)
  }

  test("a dependency no module installs is a compile error") {
    val errs = compileErrors(
      "import okay.*; " +
        "val db = module[TestModule#Db](null)(_ => ()); " +
        "val pool: TestModule#Db ?=> Module[[X] =>> TestModule#Pool ?=> X] = " +
        "  module[TestModule#Pool](null)(_ => ()); " +
        "(db and pool) { wire[TestModule#Log].tag }")
    assert(errs.contains("No given instance of type okay.TestModule#Log"), errs)
  }

  test("a failing acquisition releases what came before it") {
    var closed = List.empty[String]
    val a = module[String]("a")(_ => closed ::= "a")
    val boom = module[Int](throw RuntimeException("boom"))(_ => closed ::= "never")
    intercept[RuntimeException](run((a and boom) { wire[Int] })): Unit
    assertEquals(closed, List("a"))
  }
}
