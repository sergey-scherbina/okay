package okay.deploy

import okay.{Handler, Module, Static, module, moduleAs, wire}
import okay.given
import Needs.needs

/** the capabilities an application's root still waits for, each saying what it is to a deployment */
object Caps:
  trait Pg { def url: String }
  trait Files { def root: String }
  trait Unknown
  given Needs[Pg] = Needs(Need.Database(Engine.Postgres, "16", "shop"))
  given Needs[Files] = Needs(Need.Volume("/app/data"))

/** specs/di.md stage 3: the root module's inputs are the deployment's needs */
class TestNeeds extends munit.FunSuite:
  import Caps.*

  trait App { def ready: Boolean }
  type Root = Pg ?=> Files ?=> Module[[X] =>> App ?=> X]
  val root: Root = module[App](new App { val ready = wire[Pg].url.nonEmpty && wire[Files].root.nonEmpty })(_ => ())

  test("the needs are read off the root's type, one per unresolved input, in order") {
    assertEquals(Needs.of[Root], Vector(Need.Database(Engine.Postgres, "16", "shop"), Need.Volume("/app/data")))
  }

  test("a tupled input chain and a root with nothing left to resolve") {
    type Both = (Pg, Files) ?=> Module[[X] =>> App ?=> X]
    assertEquals(Needs.of[Both], Needs.of[Root])
    assertEquals(Needs.of[Module[[X] =>> App ?=> X]], Vector.empty)
  }

  test("a mixed root: the place's inputs are declared, the runtime's are dropped") {
    // a Timer is the process's own — declared `runtime` in Needs's
    // companion, for every application — so it must not appear as a
    // deployment need, and must not be an error either (needs-runtime)
    type Mixed = Caps.Pg ?=> okay.Timer ?=> Caps.Files ?=> Module[[X] =>> App ?=> X]
    assertEquals(Needs.of[Mixed], Vector(Need.Database(Engine.Postgres, "16", "shop"), Need.Volume("/app/data")))
  }

  test("an input a capability declares as the runtime's says nothing to the deployment") {
    trait Ticker
    given Needs[Ticker] = Needs.runtime
    assertEquals(Needs.of[Ticker ?=> Module[[X] =>> App ?=> X]], Vector.empty)
  }

  test("an input the deployment does not know is a compile error naming it") {
    val errs = compileErrors("Needs.of[Caps.Unknown ?=> okay.Module[[X] =>> Int ?=> X]]")
    assert(errs.contains("does not know what that is") && errs.contains("Unknown"), errs)
    // and the message offers both answers, not just the place's one
    assert(errs.contains("Needs.runtime"), errs)
  }

  test("a Service carries them beside the needs only the place can say — the database said once, in the type") {
    val web = Service("web", Run.Image("shop/web"), needs = Needs.of[Root] :+ Need.Port(8080))
    assertEquals(web.databases.map(_.database), Vector("shop"))
    assertEquals(web.volumes.map(_.path), Vector("/app/data"))
    assertEquals(web.mainPort, Some(8080))
  }

  test("a component declares its own need where it opens the thing, and the deployment reads it before anything opens") {
    final case class Conf(log: String)
    trait Store; final class FileStore(val path: String) extends Store { var closed = false }
    var opened = 0
    val conf = Module.value[Conf](Conf("/app/data/board.log"))
    val store: Conf ?=> Module[[X] =>> Store ?=> X] =
      val path = wire[Conf].log
      moduleAs[Store, FileStore]({ opened += 1; FileStore(path) })(_.closed = true)
        .needs(Need.Volume(java.nio.file.Path.of(path).getParent.toString))
    val app = conf and store
    assertEquals(Needs.declared(app), Vector(Need.Volume("/app/data")))
    assertEquals(opened, 0)
    // the same component with a memory config declares nothing: the need follows the config
    val mem: Conf ?=> Module[[X] =>> Store ?=> X] =
      if wire[Conf].log == ":memory:" then Module.value[Store](new Store {})
      else store
    assertEquals(Needs.declared(Module.value[Conf](Conf(":memory:")) and mem), Vector.empty)
  }

  /**
   * di-needs-from-static (P13 item 4): the declaration beside the
   * code was a claim that could drift from it. A module that ASKS the
   * place for what it opens cannot: the path it opens is the answer.
   */
  test("a provisioned module's needs are its program's leaves, read before anything opens") {
    final case class Conf(log: String)
    trait Store; final class FileStore(val path: String) extends Store { var closed = false }
    var opened = 0
    val conf = Module.value[Conf](Conf("/app/data/board.log"))
    // installed under the concrete type here, so the test reads the
    // path with no cast; an application installs under `Store`
    val store: Conf ?=> Module[[X] =>> FileStore ?=> X] =
      val file = java.nio.file.Path.of(wire[Conf].log)
      Needs.provisioned[FileStore, FileStore](
        Static.op(Provision.Volume(file.getParent.toString))
          .map(dir => { opened += 1; FileStore(dir.resolve(file.getFileName).toString) }))(_.closed = true)
    val app = conf and store
    // nothing declared by hand, and the deployment reads the volume off the program
    assertEquals(Needs.declared(app), Vector(Need.Volume("/app/data")))
    assertEquals(opened, 0)
    // the default place mounts the volume where it was asked for
    val got = okay.!.run(okay.Resource.run[String, okay.Pure](app { wire[FileStore].path }))
    assertEquals(got, "/app/data/board.log")
    assertEquals(opened, 1)
  }

  test("the path a provisioned module opens is the path the PLACE answered — the drift the declaration allowed is gone") {
    trait Store; final class FileStore(val path: String) extends Store
    val store = Needs.provisioned[FileStore, FileStore](
      Static.op(Provision.Volume("/app/data")).map(d => FileStore(d.resolve("board.log").toString)))(_ => ())(
      // a test's place: the volume lives in a temporary directory
      using new Handler[Provision]:
        def handle[A](p: Provision[A]): A = p match
          case Provision.Volume(path, _, _) => java.nio.file.Path.of("/tmp/vol-test").resolve(path.stripPrefix("/"))
          case other => Provision.local.handle(other))
    assertEquals(Needs.declared(store), Vector(Need.Volume("/app/data")))   // what the deployment mounts
    val got = okay.!.run(okay.Resource.run[String, okay.Pure](store { wire[FileStore].path }))
    assertEquals(got, "/tmp/vol-test/app/data/board.log")                   // where the process wrote
  }

  test("two leaves, one spine: a server needs its port and its volume, and both are read off it") {
    trait Srv; final class Server(val port: Int, val dir: java.nio.file.Path) extends Srv
    val S = summon[okay.Selective[[A] =>> Static[Provision, A]]]
    val srv = Needs.provisioned[Server, Server](
      S.fmap(Static.op(Provision.Port(8090)), (p: Int) => (d: java.nio.file.Path) => Server(p, d))
        .app(Static.op(Provision.Volume("/app/data"))))(_ => ())
    assertEquals(Needs.declared(srv), Vector(Need.Port(8090), Need.Volume("/app/data")))
    val got = okay.!.run(okay.Resource.run[(Int, String), okay.Pure](srv { val s = wire[Server]; (s.port, s.dir.toString) }))
    assertEquals(got, (8090, "/app/data"))
  }

  test("a database the place did not set a URL for is an error naming the setting, not an empty string") {
    val err = intercept[IllegalStateException](
      Provision.local.handle(Provision.Database(Engine.Postgres, "16", "shop", as = "books")))
    assert(err.getMessage.contains("BOOKS_URL"), err.getMessage)
    assertEquals(Provision.urlSetting("db"), "DB_URL")
  }

