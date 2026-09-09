package okay.zio

import okay.{Module, Providing, Resource, module, providing, wire}
import _root_.zio.{Exit, Runtime, Scope, Tag, Unsafe, ZEnvironment, ZIO, ZLayer}

/**
 * ZLayer ⇄ Module (specs/di.md, stage 2). A layer IS a resource-scoped
 * constructor and a module is one too, so each side is the other's
 * scope: a module becomes a layer by `Resource.open` under ZIO's
 * `acquireRelease`; a layer becomes a module by building it in a
 * `Scope` the module closes at release. `ZEnvironment` is values
 * already built — a `Providing`.
 *
 * One capability per conversion: a ZIO environment is typed by Tags
 * per member, ours by a context-function chain, and neither side
 * composes for the other — `++` on theirs, `and` on ours.
 */
object ZioLayers {

  /** a module as a layer: the scope is ZIO's, the release is the module's */
  def toLayer[A: Tag](m: Module[[X] =>> A ?=> X]): ZLayer[Any, Throwable, A] =
    ZLayer.scoped(
      ZIO.acquireRelease(ZIO.attempt(Resource.open(m { wire[A] })))((_, close) => ZIO.succeed(close()))
        .map(_._1))

  /** a layer as a module: built in a scope of its own at acquisition, closed at release */
  def fromLayer[A: Tag](layer: ZLayer[Any, Throwable, A],
                        runtime: Runtime[Any] = Runtime.default): Module[[X] =>> A ?=> X] =
    Unsafe.unsafe { implicit u =>
      def run[B](z: ZIO[Any, Throwable, B]): B = runtime.unsafe.run(z).getOrThrowFiberFailure()
      val scope = run(Scope.make)
      module[A](run(layer.build(scope).map(_.get[A])))(_ => run(scope.close(Exit.unit)))
    }

  /** a built environment is a Providing */
  def fromEnvironment[A: Tag](env: ZEnvironment[A]): Providing[[X] =>> A ?=> X] =
    providing[A](env.get[A])
}
