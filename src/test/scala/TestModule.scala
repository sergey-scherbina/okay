package okay

import okay.given

/** Modules: installers that acquire in a scope (specs/di.md, stage 0). */
class TestModule extends munit.FunSuite {

  trait Db { def q: String }
  trait Pool { def db: Db; def size: Int }
  trait Log { def tag: String }
  final case class Conf(url: String)

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

  // stage 1: qualifiers are TYPES — an opaque type per role, never a string (ModuleRoles below)
  import ModuleRoles.*

  test("two capabilities of one type are told apart by an opaque type per role") {
    val prim = Module.value[Primary](primary(RDb("p")))
    val repl = Module.value[Replica](replica(RDb("r")))
    assertEquals(run((prim and repl) { wire[Primary].db.q + wire[Replica].rdb.q }), "pr")
  }

  test("a role no module installs is a compile error, though the same type is installed under another") {
    val errs = compileErrors(
      "import okay.*; " +
        "val prim = Module.value[ModuleRoles.Primary](ModuleRoles.primary(ModuleRoles.RDb(\"p\"))); " +
        "prim { wire[ModuleRoles.Replica] }")
    assert(errs.contains("No given instance of type okay.ModuleRoles.Replica"), errs)
  }

  // stage 1: the plan is the type
  test("plan names what a module will install, in acquisition order, before anything is built") {
    var opened = 0
    val db = module[Db]({ opened += 1; new Db { val q = "" } })(_ => ())
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool]({ opened += 1; new Pool { val db = wire[Db]; val size = 1 } })(_ => ())
    val log = Module.value[Log](new Log { val tag = "" })
    assertEquals((db and pool and log).plan, Vector("Db", "Pool", "Log"))
    assertEquals(opened, 0)
    val prim = Module.value[Primary](primary(RDb("p")))
    assertEquals((prim and log).plan, Vector("Primary", "Log"))
  }

  // stage 2: what a container needs
  test("exports names, classes and values of what the scope built; an opaque role keeps its name under its erased class") {
    val db = module[Db](new Db { val q = "row" })(_ => ())
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](new Pool { val db = wire[Db]; val size = 2 })(_ => ())
    val prim = Module.value[Primary](primary(RDb("p")))
    val xs = run((db and pool and prim).exports)
    assertEquals(xs.map(_.name), Vector("Db", "Pool", "Primary"))
    assertEquals(xs.map(_.cls), Vector(classOf[Db], classOf[Pool], classOf[RDb]))
    assert(xs(1).value.isInstanceOf[Pool])
    assertEquals(xs(2).value, RDb("p"))
  }

  test("Resource.open acquires now and releases at the closer, reverse order, once") {
    var log = List.empty[String]
    def open(n: String) = module[String]({ log ::= s"open $n"; n })(r => log ::= s"close $r")
    val (xs, close) = Resource.open((open("a") and open("b")).exports)
    assertEquals(xs.map(_.value), Vector("a", "b"))
    assertEquals(log.reverse, List("open a", "open b"))
    close(); close()
    assertEquals(log.reverse, List("open a", "open b", "close b", "close a"))
  }

  test("Resource.open: a failing acquisition releases what came before, and throws") {
    var closed = List.empty[String]
    val a = module[String]("a")(_ => closed ::= "a")
    val boom = module[Int](throw RuntimeException("boom"))(_ => closed ::= "never")
    intercept[RuntimeException](Resource.open((a and boom) { wire[Int] })): Unit
    assertEquals(closed, List("a"))
  }

  test("moduleAs: acquired as the concrete type, installed as the capability, released as the concrete type") {
    class Conn(val name: String) extends Db { def q = s"row from $name"; var closed = false }
    val c = Conn("primary")
    val m = moduleAs[Db, Conn](c)(_.closed = true)
    assertEquals(run(m { wire[Db].q }), "row from primary")
    assertEquals(c.closed, true)
    // and what it installs is the CAPABILITY, not the concrete class
    assert(compileErrors("import okay.*; okay.Module.value[Int](1) { wire[String] }").nonEmpty)
  }

  test("use: a body that is itself a program in the scope, flattened once") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; new Db { val q = "row" } })(_ => log ::= "close db")
    // the body acquires further IN the same region, as a server would
    val body: Db ?=> (String ! Resource) =
      Resource.acquire({ log ::= "open server"; wire[Db].q })(_ => log ::= "close server")
    assertEquals(run(db.use(body)), "row")
    assertEquals(log.reverse, List("open db", "open server", "close server", "close db"))
  }

  // module-facts: what a module says about itself, and when it can be read
  // the whole declaration: how two contributions merge is the Monoid
  // the core already has for Vector (fact-is-monoid)
  object Tags extends Fact[Vector[String]]

  test("facts declared on modules merge left to right under and, by the kind's own rule") {
    val a = Module.value[Db](new Db { val q = "a" }).declare(Tags, Vector("db"))
    val b = Module.value[Log](new Log { val tag = "" }).declare(Tags, Vector("log")).declare(Tags, Vector("log2"))
    assertEquals((a and b).facts.get(Tags), Vector("db", "log", "log2"))
    assertEquals(a.facts.get(Tags), Vector("db"))
    assertEquals(module[Db](new Db { val q = "" })(_ => ()).facts.get(Tags), Vector.empty)
  }

  test("a ready left side lets the dependent right side be read before anything opens") {
    var opened = 0
    val conf = Module.value[Conf](Conf("/data/log"))
    val store: Conf ?=> Module[[X] =>> Db ?=> X] =
      module[Db]({ opened += 1; new Db { val q = wire[Conf].url } })(_ => ())
        .declare(Tags, Vector(s"volume ${wire[Conf].url}"))
    val app = conf and store
    assertEquals(app.facts.get(Tags), Vector("volume /data/log"))   // read off the value: nothing opened
    assertEquals(opened, 0)
    assertEquals(run(app { wire[Db].q }), "/data/log")
    assertEquals(opened, 1)
  }

  test("a right side behind an acquisition keeps its facts until the scope runs — stated, not hidden") {
    val db = module[Db](new Db { val q = "" })(_ => ())
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](new Pool { val db = wire[Db]; val size = 1 })(_ => ()).declare(Tags, Vector("pool"))
    assertEquals((db and pool).facts.get(Tags), Vector.empty)
    assertEquals((db and pool).ready, None)
  }

  // di-multibind: several contributors, one collection
  test("installing merges every contribution into a capability the body reads") {
    val a = Module.value[Db](new Db { val q = "a" }).declare(Tags, Vector("from db"))
    val b: Db ?=> Module[[X] =>> Log ?=> X] =
      Module.value[Log](new Log { val tag = "" }).declare(Tags, Vector("from log"))
    val app = (a and b).installing(Tags)
    assertEquals(run(app { wire[Vector[String]] }), Vector("from db", "from log"))
  }

  test("a contribution BELOW an acquisition still reaches the collection") {
    var opened = 0
    val conf = Module.value[Conf](Conf("/data"))
    val store: Conf ?=> Module[[X] =>> Db ?=> X] =
      module[Db]({ opened += 1; new Db { val q = wire[Conf].url } })(_ => ())
        .declare(Tags, Vector("volume /data"))
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](new Pool { val db = wire[Db]; val size = 1 })(_ => ())
        .declare(Tags, Vector("pool"))          // below an acquisition: invisible early
    val app = (conf and store and pool).installing(Tags)
    // the early PREVIEW stops at the first acquisition, as it must
    assertEquals((conf and store and pool).facts.get(Tags), Vector("volume /data"))
    // the built collection has everything, in acquisition order
    assertEquals(run(app { wire[Vector[String]] }), Vector("volume /data", "pool"))
    assertEquals(opened, 1)
  }

  // fact-is-monoid: a collection is not special — any monoid is a kind
  object Notes extends Fact[String]          // the String monoid: concatenation
  object Weight extends Fact[Int]            // numbers add (Group[N] is a Monoid)
  object Newest extends Fact[Option[String]](
    using Monoid.of(Option.empty[String])((_, b) => b))   // last wins

  test("a fact is any monoid, not a collection: text concatenates, numbers add, a rule of your own wins") {
    val a = Module.value[Db](new Db { val q = "a" })
      .declare(Notes, "opened db").declare(Weight, 3).declare(Newest, Some("db"))
    val b = Module.value[Log](new Log { val tag = "" })
      .declare(Notes, ", opened log").declare(Weight, 4).declare(Newest, Some("log"))
    val app = a and b
    assertEquals(app.facts.get(Notes), "opened db, opened log")
    assertEquals(app.facts.get(Weight), 7)
    assertEquals(app.facts.get(Newest), Some("log"))
    // and each kind installs on its own
    assertEquals(run(app.installing(Weight) { wire[Int] }), 7)
  }

  test("shadowed names a capability installed twice, which a test double does on purpose") {
    val db = module[Db](new Db { val q = "real" })(_ => ())
    val log = Module.value[Log](new Log { val tag = "t" })
    assertEquals((db and log).shadowed, Vector.empty)
    val withDouble = db and log and Module.value[Db](new Db { val q = "fake" })
    assertEquals(withDouble.shadowed, Vector("Db"))
    assertEquals(run(withDouble { wire[Db].q }), "fake")
  }

  test("a failing acquisition releases what came before it") {
    var closed = List.empty[String]
    val a = module[String]("a")(_ => closed ::= "a")
    val boom = module[Int](throw RuntimeException("boom"))(_ => closed ::= "never")
    intercept[RuntimeException](run((a and boom) { wire[Int] })): Unit
    assertEquals(closed, List("a"))
  }
}

/** the qualifier pattern: one opaque type per ROLE over the one domain type */
object ModuleRoles:
  final case class RDb(q: String)
  opaque type Primary = RDb
  opaque type Replica = RDb
  def primary(d: RDb): Primary = d
  def replica(d: RDb): Replica = d
  extension (p: Primary) def db: RDb = p
  extension (r: Replica) def rdb: RDb = r
