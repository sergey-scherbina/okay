package okay.zio

import _root_.zio.{Runtime, Task, Unsafe, ZEnvironment, ZIO}
import okay.{via}
import okay.freer.{+}
import okay.freer.perform
import okay.freer.{!}
import okay.freer.Row.bind
import okay.given
import okay.freer.given

/**
 * ZIO as an effect of the tree (specs/foreign-effects-in-tree.md, stages 1 and 2): one member per step, and the
 * handlers read `R` and `E` off the row the way ZIO's own `flatMap` joins them.
 */
class TestZioMembers extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  trait Db { def n: Int }
  trait Log { def tag: String }
  sealed trait AppErr
  final case class DbErr(m: String) extends AppErr
  final case class LogErr(m: String) extends AppErr

  def unsafeRun[E, A](z: ZIO[Any, E, A]): Either[E, A] =
    Unsafe.unsafe(implicit u => Runtime.default.unsafe.run(z.either).getOrThrowFiberFailure())

  val env: ZEnvironment[Db & Log] = ZEnvironment[Db, Log](new Db { def n = 2 }, new Log { def tag = "abc" })
  val z1: ZIO[Db, DbErr, Int] = ZIO.serviceWith[Db](_.n)
  val z2: ZIO[Log, LogErr, Int] = ZIO.serviceWith[Log](_.tag.length)
  val p: Int ! (ZIO[Db, DbErr, *] + ZIO[Log, LogErr, *]) = z1.perform.bind(a => z2.perform.map(_ + a))

  test("toZIO answers the type ZIO's own for gives") {
    val own: ZIO[Db & Log, AppErr, Int] = for a <- z1; b <- z2 yield a + b
    val ours: ZIO[Db & Log, AppErr, Int] = p.toZIO
    note(s"own=${unsafeRun(own.provideEnvironment(env))}")
    assertEquals(unsafeRun(ours.provideEnvironment(env)), unsafeRun(own.provideEnvironment(env)))
    assertEquals(unsafeRun(ours.provideEnvironment(env)), Right(5))
  }

  test("provideEnvironment: the environment gone from every step") {
    val q: Int ! ZIO[Any, AppErr, *] = p.provideEnvironment(env)
    assertEquals(unsafeRun(q.toZIO), Right(5))
  }

  test("mapError: every step's failure mapped, the first failure stops the program") {
    val down: ZIO[Db, DbErr, Int] = ZIO.fail(DbErr("down"))
    val failing: Int ! (ZIO[Db, DbErr, *] + ZIO[Log, LogErr, *]) = down.perform.bind(a => z2.perform.map(_ + a))
    val q: Int ! ZIO[Db & Log, String, *] = failing.mapError(_.toString)
    assertEquals(unsafeRun(q.toZIO.provideEnvironment(env)), Left("DbErr(down)"))
  }

  test("via[Task]: a Task lowered to okay's Async") {
    val t: Int ! Task = ZIO.attempt(41).perform
    assertEquals(t.via[Task].map(_ + 1).runWith, 42)
  }

  test("a thousand ZIO steps in one program") {
    val q: Int ! ZIO[Any, Nothing, *] = (1 to 1000).foldLeft(ZIO.succeed(0).perform)((m, _) => m.flatMap(x => ZIO.succeed(x + 1).perform))
    assertEquals(unsafeRun(q.toZIO), Right(1000))
  }
}
