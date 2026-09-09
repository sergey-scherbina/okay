package okay.deploy

import okay.*
import okay.given
import Needs.needs

/**
 * The samples in docs/di.md, compiled and run.
 *
 * A guide whose code does not compile is worse than no guide, and
 * this one is written in the shapes an application actually uses. If
 * a rename breaks a sample here, the page is wrong and this says so
 * before a reader finds out.
 */
class TestDiDocs extends munit.FunSuite:

  trait Db { def q: String; def close(): Unit = () }
  trait Pool { def borrow(): Int }
  final class FileThing(val path: String) extends Db { var closed = false; def q = path }
  final case class Conf(db: String)

  object Db:
    def open(url: String): Db = new Db { val q = url }
  object Pool:
    def over(d: Db): Pool = new Pool { def borrow() = d.q.length }

  test("a module is an installer that has not been built yet") {
    val db = module[Db](Db.open("jdbc:x"))(_.close())
    val conf = Module.value[Conf](Conf("/app/data/board.log"))
    assertEquals(!.run(Resource.run[String, Pure]((conf and db) { wire[Db].q })), "jdbc:x")
  }

  test("what you open is not always what you install") {
    val t = FileThing("/tmp/x")
    val store = moduleAs[Db, FileThing](t)(_.closed = true)
    assertEquals(!.run(Resource.run[String, Pure](store { wire[Db].q })), "/tmp/x")
    assertEquals(t.closed, true)
  }

  test("the graph is the composition, and a double overrides to the right") {
    val db = module[Db](Db.open("real"))(_.close())
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] =
      module[Pool](Pool.over(wire[Db]))(_ => ())
    assertEquals(!.run(Resource.run[Int, Pure]((db and pool) { wire[Pool].borrow() })), 4)
    val fake = Module.value[Db](Db.open("fake"))
    assertEquals(!.run(Resource.run[String, Pure]((db and fake) { wire[Db].q })), "fake")
  }

  test("use: a body that is itself a program in the scope") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open db"; Db.open("u") })(_ => log ::= "close db")
    val server: Db ?=> (String ! Resource) =
      Resource.acquire({ log ::= "serve"; wire[Db].q })(_ => log ::= "stop")
    assertEquals(!.run(Resource.run[String, Pure](db.use(server))), "u")
    assertEquals(log.reverse, List("open db", "serve", "stop", "close db"))
  }

  test("Resource.open: the scope whose end belongs to somebody else") {
    val db = module[Db](Db.open("u"))(_.close())
    val (values, close) = Resource.open(db.exports)
    assertEquals(values.map(_.name), Vector("Db"))
    close()
  }

  test("qualifiers are types") {
    val prim = Module.value[DocRoles.Primary](DocRoles.primary(Db.open("p")))
    val repl = Module.value[DocRoles.Replica](DocRoles.replica(Db.open("r")))
    import DocRoles.*
    assertEquals(!.run(Resource.run[String, Pure]((prim and repl) {
      wire[DocRoles.Primary].db.q + wire[DocRoles.Replica].rdb.q })), "pr")
    assert(compileErrors("import okay.*; okay.Module.value[okay.deploy.DocRoles.Primary](null) { wire[okay.deploy.DocRoles.Replica] }").nonEmpty)
  }

  test("plan and exports read a module without building it") {
    var opened = 0
    val db = module[Db]({ opened += 1; Db.open("u") })(_ => ())
    val conf = Module.value[Conf](Conf("c"))
    assertEquals((conf and db).plan, Vector("Conf", "Db"))
    assertEquals(opened, 0)
    val xs = !.run(Resource.run[Vector[Installed], Pure]((conf and db).exports))
    assertEquals(xs.map(_.name), Vector("Conf", "Db"))
    assertEquals(xs.head.cls, classOf[Conf])
  }

  test("a component declares its need where it opens the thing, and the deployment reads it early") {
    val conf = Module.value[Conf](Conf("/app/data/board.log"))
    val store: Conf ?=> Module[[X] =>> Db ?=> X] =
      val path = wire[Conf].db
      moduleAs[Db, FileThing](FileThing(path))(_.closed = true)
        .needs(Need.Volume(java.nio.file.Path.of(path).getParent.nn.toString))
    assertEquals(Needs.declared(conf and store), Vector(Need.Volume("/app/data")))
  }

  test("what a deployment reads by type: the place's inputs, not the runtime's") {
    trait Pg
    given Needs[Pg] = Needs(Need.Database(Engine.Postgres, "16", "shop"))
    type Root = Pg ?=> okay.Timer ?=> Module[[X] =>> Db ?=> X]
    assertEquals(Needs.of[Root], Vector(Need.Database(Engine.Postgres, "16", "shop")))
  }

/** the guide's qualifier sample: one opaque type per ROLE */
object DocRoles:
  opaque type Primary = TestDiDocs#Db
  opaque type Replica = TestDiDocs#Db
  def primary(d: TestDiDocs#Db): Primary = d
  def replica(d: TestDiDocs#Db): Replica = d
  extension (p: Primary) def db: TestDiDocs#Db = p
  extension (r: Replica) def rdb: TestDiDocs#Db = r
