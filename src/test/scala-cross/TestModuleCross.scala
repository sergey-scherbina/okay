package okay

import okay.given

/**
 * The module vocabulary on EVERY platform (di-cross, specs/di.md).
 *
 * `Module`, `module`, `and`, `plan`, `exports` and `Resource.open`
 * are shared core, so they compile for JS and Native — and until this
 * file existed, nothing ran them there: `TestModule` lives in
 * `src/test/scala`, and both non-JVM platforms replace the test
 * sources with `src/test/scala-cross` alone. The JVM suite keeps the
 * fuller story (compile errors, opaque qualifiers); this is the part
 * a platform could break.
 */
class TestModuleCross extends munit.FunSuite {

  trait Db { def q: String }
  trait Pool { def db: Db; def size: Int }
  final case class Conf(url: String)

  def run[A](p: A ! Resource): A = !.run(Resource.run[A, Nothing](p))

  test("modules acquire left to right and release in reverse at the end of the scope") {
    var log = List.empty[String]
    def open(n: String) = module[String]({ log ::= s"open $n"; n })(r => log ::= s"close $r")
    assertEquals(run((open("a") and open("b")) { wire[String] }), "b")
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("a module's acquisition reads the module before it, and a double overrides to the right") {
    val db = module[Db](new Db { val q = "row" })(_ => ())
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](new Pool { val db = wire[Db]; val size = 4 })(_ => ())
    assertEquals(run((db and pool) { s"${wire[Pool].db.q}/${wire[Pool].size}" }), "row/4")
    val fake = Module.value[Db](new Db { val q = "fake" })
    assertEquals(run((db and fake) { wire[Db].q }), "fake")
  }

  test("plan is read off the type, before anything is built") {
    var opened = 0
    val db = module[Db]({ opened += 1; new Db { val q = "" } })(_ => ())
    val conf = Module.value[Conf](Conf("u"))
    assertEquals((db and conf).plan, Vector("Db", "Conf"))
    assertEquals(opened, 0)
  }

  test("exports carry the name, the erased class and the value") {
    val conf = Module.value[Conf](Conf("u"))
    val db = module[Db](new Db { val q = "row" })(_ => ())
    val xs = run((conf and db).exports)
    assertEquals(xs.map(_.name), Vector("Conf", "Db"))
    assertEquals(xs.head.cls, classOf[Conf])
    assertEquals(xs.head.value, Conf("u"))
  }

  test("Resource.open acquires now and releases at the closer, once") {
    var log = List.empty[String]
    def open(n: String) = module[String]({ log ::= s"open $n"; n })(r => log ::= s"close $r")
    val (got, close) = Resource.open((open("a") and open("b")) { wire[String] })
    assertEquals(got, "b")
    assertEquals(log.reverse, List("open a", "open b"))
    close(); close()
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }
}
