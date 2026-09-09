package okay.zio

import okay.{!, Module, Resource, module, wire}
import okay.given
import ZioLayers.*
import _root_.zio.{Runtime, Scope, Unsafe, ZEnvironment, ZIO, ZLayer}

trait Db { def q: String }
trait Pool { def db: Db }

/** specs/di.md stage 2: ZLayer ⇄ Module */
class TestZioLayers extends munit.FunSuite {

  def zrun[A](z: ZIO[Scope, Throwable, A]): A =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(ZIO.scoped(z)).getOrThrowFiberFailure())

  test("a module is a layer: acquired when the layer builds, released when ZIO's scope closes") {
    var log = List.empty[String]
    val db = module[Db]({ log ::= "open"; new Db { val q = "row" } })(_ => log ::= "close")
    val layer = toLayer(db)
    val got = zrun(layer.build.map(_.get[Db].q).tap(_ => ZIO.succeed(log ::= "used")))
    assertEquals(got, "row")
    assertEquals(log.reverse, List("open", "used", "close"))
  }

  test("a layer is a module: built at acquisition, its scope closed at release, composable with and") {
    var log = List.empty[String]
    val layer: ZLayer[Any, Throwable, Db] =
      ZLayer.scoped(ZIO.acquireRelease(ZIO.succeed { log ::= "open"; new Db { val q = "z" } })(_ => ZIO.succeed(log ::= "close")))
    val pool: Db ?=> Module[[X] =>> Pool ?=> X] = module[Pool](new Pool { val db = wire[Db] })(_ => ())
    val got = !.run(Resource.run[String, Nothing]((fromLayer(layer) and pool) { wire[Pool].db.q }))
    assertEquals(got, "z")
    assertEquals(log.reverse, List("open", "close"))
  }

  test("a built environment is a Providing") {
    val env = ZEnvironment[Db](new Db { val q = "env" })
    assertEquals(fromEnvironment(env) { wire[Db].q }, "env")
  }
}
