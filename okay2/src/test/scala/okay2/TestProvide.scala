package okay2

import java.util.concurrent.atomic.AtomicInteger

/**
 * The installer half of the capability pair: expression-scoped, and
 * with the doors it is the DI story — a missing dependency is a COMPILE
 * error, quoted. In Scala 2 a body that takes several capabilities is
 * curried, `implicit db => implicit log => app`, and an override takes
 * the SAME NAME: Scala 2 shadows implicits by name, where Scala 3
 * prefers the nearer scope.
 */
class TestProvide extends munit.FunSuite {

  trait Db { def q: String }
  trait Log { def tag: String }
  def app(implicit db: Db, log: Log): String = s"${log.tag}:${db.q}"

  test("provide installs for a block, in expression position") {
    val prod: Db = new Db { val q = "prod-row" }
    val logged: Log = new Log { val tag = "info" }
    assertEquals(provide(prod, logged)(implicit db => implicit log => app), "info:prod-row")
  }

  test("nesting resolves to the NEAREST provide — the override story, by name") {
    val outer: Db = new Db { val q = "outer" }
    val inner: Db = new Db { val q = "inner" }
    val log: Log = new Log { val tag = "t" }
    val got = provide(outer, log) { implicit db => implicit log =>
      (provide(inner)(implicit db => app), app)   // inner wins inside; outer after
    }
    assertEquals(got, ("t:inner", "t:outer"))
  }

  test("the DI claim: one program, two environments; a missing dependency does not COMPILE") {
    val test: Db = new Db { val q = "stub" }
    val quiet: Log = new Log { val tag = "quiet" }
    assertEquals(provide(test, quiet)(implicit db => implicit log => app), "quiet:stub")
    val errors = compileErrors("provide(new Db { val q = \"x\" }: Db)(implicit db => app)")
    assert(errors.nonEmpty, "the missing Log compiled")
  }

  test("three capabilities at once") {
    trait Clock { def now: Long }
    def stamped(implicit db: Db, log: Log, c: Clock) = s"${log.tag}@${c.now}:${db.q}"
    val out = provide[Db, Log, Clock, String](new Db { val q = "r" }, new Log { val tag = "l" }, new Clock { val now = 42L })(
      implicit db => implicit log => implicit c => stamped)
    assertEquals(out, "l@42:r")
  }
}

/** The composable form: installers as values, `and` as applicative
 * composition via currying — no nesting, no arity cap. */
class TestProviding extends munit.FunSuite {

  trait Db { def q: String }
  trait Log { def tag: String }
  trait Clock { def now: Long }
  def app(implicit db: Db, log: Log, c: Clock): String = s"${log.tag}@${c.now}:${db.q}"

  val withDb = providing[Db](new Db { val q = "row" })
  val withLog = providing[Log](new Log { val tag = "t" })
  val withClock = providing[Clock](new Clock { val now = 7L })

  test("flat and-composition, no nesting") {
    assertEquals((withDb and withLog and withClock)(implicit d => implicit l => implicit c => app), "t@7:row")
  }

  test("compositions are values — build once, reuse") {
    val base = withDb and withLog
    assertEquals((base and withClock)(implicit d => implicit l => implicit c => app), "t@7:row")
    assertEquals((base and providing[Clock](new Clock { val now = 9L }))(implicit d => implicit l => implicit c => app), "t@9:row")
  }

  test("the right operand is the inner layer — override under nearest-wins") {
    val shadowed = withLog and providing[Log](new Log { val tag = "inner" })
    assertEquals(shadowed { _ => implicit l => implicitly[Log].tag }, "inner")
  }

  test("past the flat form's arity: layers by composition, the innermost wins") {
    case class N(v: Int)
    val o = providing[N](N(0))
    val stack = o and o and o and o and o and o and o and o and providing[N](N(9))
    assertEquals(stack { _ => _ => _ => _ => _ => _ => _ => _ => implicit n => wire[N].v }, 9)
  }

  test("the DI claim holds: a missing dependency does not COMPILE") {
    val errors = compileErrors("(withDb and withLog)(implicit d => implicit l => app)")
    assert(errors.nonEmpty, "the missing Clock compiled")
  }
}

/** The consumer one-liner: wire[A] is Reader's ask. */
class TestWire extends munit.FunSuite {

  trait Db { def q: String }
  trait Log { def tag: String }

  test("wire pulls the ambient capability by type") {
    assertEquals(provide[Db, String](new Db { val q = "row" })(implicit db => wire[Db].q), "row")
  }

  test("composes with providing; nearest given wins") {
    val base = providing[Db](new Db { val q = "outer" })
    assertEquals((base and providing[Db](new Db { val q = "inner" })) { _ => implicit d => wire[Db].q }, "inner")
  }

  test("a missing given does not COMPILE") {
    assert(compileErrors("wire[Db].q").nonEmpty, "the missing Db compiled")
  }
}

/**
 * A module is an installer that has not been built yet: it builds in
 * the Resource effect, so opening and closing in reverse order is the
 * region's obligation.
 */
class TestModule extends munit.FunSuite {

  trait Db { def name: String }
  trait Pool { def borrow(): Int }

  test("modules acquire in order and release in reverse, inside the scope; the right is built in the left's context") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val name = "db" } })(_ => log ::= "close db")
    val pool = db and { implicit d: Db =>
      module[Pool]({ log ::= s"open pool over ${wire[Db].name}"; new Pool { def borrow() = 7 } })(_ => log ::= "close pool")
    }
    val app: Int ! Resource = pool { _ => implicit p => wire[Pool].borrow() }
    assertEquals(Resource.scoped(app), 7)
    assertEquals(log.reverse, List("open db", "open pool over db", "close pool", "close db"))
  }

  test("a plain `and`: two independent modules, still one scope") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val name = "db" } })(_ => log ::= "close db")
    val pool = module[Pool]({ log ::= "open pool"; new Pool { def borrow() = 1 } })(_ => log ::= "close pool")
    val got = Resource.scoped((db and pool) { implicit d => implicit p => s"${wire[Db].name}:${wire[Pool].borrow()}" })
    assertEquals(got, "db:1")
    assertEquals(log.reverse, List("open db", "open pool", "close pool", "close db"))
  }

  test("use: a body that is itself a program in the scope") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val name = "db" } })(_ => log ::= "close db")
    val prog = db.use { implicit d => Resource.acquire { log ::= "open conn"; 3 } (_ => log ::= "close conn").map(_ + wire[Db].name.length) }
    assertEquals(Resource.scoped(prog), 5)
    assertEquals(log.reverse, List("open db", "open conn", "close conn", "close db"))
  }

  test("a value module is ready, and a test double shadows under nearest-wins") {
    val base = module[Db]({ new Db { val name = "real" } })(_ => ())
    val fake = Module.value[Db](new Db { val name = "fake" })
    assert(base.ready.isEmpty)
    assert(fake.ready.nonEmpty)
    assertEquals(Resource.scoped((base and fake) { _ => implicit d => wire[Db].name }), "fake")
  }

  object Routes extends Fact[Vector[String]](Vector.empty, _ ++ _)

  test("facts: several contributors, one collection, installed as a capability; ready modules preview it") {
    val admin = Module.value[Db](new Db { val name = "admin" }).declare(Routes)(Vector("/admin"))
    val chat = Module.contributing(Routes)(Vector("/chat"))
    val m = (admin and chat).installing(Routes)
    assertEquals(m.facts.get(Routes), Vector("/admin", "/chat"))
    val got = Resource.scoped(m { implicit d => implicit rs => s"${wire[Db].name}: ${wire[Vector[String]].mkString(",")}" })
    assertEquals(got, "admin: /admin,/chat")
  }

  test("declaring reads what THIS module installs; an acquired module's facts wait for the build") {
    var opened = 0
    val board = module[Db]({ opened += 1; new Db { val name = "board" } })(_ => ())
      .declaring(Routes)(implicit d => Vector(s"/${wire[Db].name}"))
    assertEquals(board.facts.get(Routes), Vector.empty[String], "the preview built the module")
    assertEquals(opened, 0)
    val (_, facts) = Resource.scoped(board.built)
    assertEquals(facts.get(Routes), Vector("/board"))
    assertEquals(opened, 1)
  }

  test("prototype/fresh: an instance per consumer, released by the region each fresh runs in") {
    val made = new AtomicInteger(0)
    var released = 0
    val conns = prototype[Int](made.incrementAndGet(), (_: Int) => released += 1)
    val app = conns.use { implicit n => fresh[Int].flatMap(a => fresh[Int].map(b => (a, b))) }
    assertEquals(Resource.scoped(app), (1, 2))
    assertEquals(released, 2)
    val e = compileErrors("okay2.fresh[String]")
    assert(e.contains("nothing installed the ability to MAKE"), e)
  }
}
