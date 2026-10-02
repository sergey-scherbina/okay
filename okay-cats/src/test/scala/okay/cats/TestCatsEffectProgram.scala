package okay.cats

import _root_.cats.effect.Resource
import _root_.cats.effect.kernel.{Async, Concurrent, Outcome, Temporal}
import _root_.cats.effect.unsafe.implicits.global
import _root_.cats.syntax.all.*
import okay.!
import CatsEffect.Program
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

/**
 * Generic cats-effect code AT an okay program (specs/cats-effect-instances.md):
 * the functions below know only `F[_]: Async` / `Concurrent` / `Temporal`,
 * and are called with `F = Program`.
 */
class TestCatsEffectProgram extends munit.FunSuite with okay.testkit.Munit.Diagnosed {

  def run[A](p: Program[A]): A = CatsEffect.toIO(p).unsafeRunSync()

  /** a resource whose release is counted, used by a body that never ends,
   * the whole thing cancelled from outside */
  def releasedOnCancel[F[_]](released: AtomicInteger)(using F: Async[F]): F[Outcome[F, Throwable, Unit]] =
    val r = Resource.make(F.unit)(_ => F.delay { released.incrementAndGet(); () })
    for
      fib <- F.start(r.use(_ => F.never[Unit]))
      _ <- F.sleep(20.millis)
      _ <- fib.cancel
      oc <- fib.join
    yield oc

  test("a Resource used in a cancelled fiber releases, at F = Program") {
    val released = AtomicInteger(0)
    val oc = run(releasedOnCancel[Program](released))
    note(s"outcome $oc")
    assert(oc.isCanceled)
    assertEquals(released.get, 1)
  }

  def masked[F[_]](seen: AtomicInteger)(using F: Async[F]): F[Unit] =
    F.uncancelable(poll => F.canceled >> F.delay { seen.incrementAndGet(); () } >> poll(F.delay { seen.addAndGet(100); () }))

  test("canceled inside uncancelable is deferred to poll: the masked step runs, the polled one does not") {
    val seen = AtomicInteger(0)
    val oc = CatsEffect.toIO(masked[Program](seen)).start.flatMap(_.join).unsafeRunSync()
    assert(oc.isCanceled)
    assertEquals(seen.get, 1)
  }

  def raceAndRef[F[_]](using F: Concurrent[F], T: Temporal[F]): F[(Either[Int, String], Int)] =
    for
      ref <- F.ref(0)
      winner <- F.race(T.sleep(5.millis) >> ref.update(_ + 1).as(7), T.sleep(5.seconds).as("slow"))
      n <- ref.get
    yield (winner, n)

  test("Concurrent race and Ref at Program: the fast side wins, the Ref saw it") {
    assertEquals(run(raceAndRef[Program]), (Left(7), 1))
  }

  test("Temporal timeout at Program fails the slow program") {
    val T = summon[Temporal[Program]]
    val out = run(T.timeout(T.sleep(5.seconds).as(1), 20.millis).attempt)
    assert(out.isLeft, out)
  }

  test("an okay Await inside a Program is cancelled through its canceller") {
    val unregistered = AtomicInteger(0)
    val parked: Program[Int] = CatsEffect.lift(okay.Async.await[Int](_ => () => { unregistered.incrementAndGet(); () }))
    val F = summon[Async[Program]]
    val oc = run(F.start(parked).flatMap(f => summon[Temporal[Program]].sleep(10.millis) >> f.cancel >> f.join))
    assert(oc.isCanceled)
    assertEquals(unregistered.get, 1)
  }

  test("a plain okay program lifts in and runs; a deep one stays on the stack") {
    val p: Int ! okay.Async = (1 to 100000).foldLeft(okay.pure[okay.Async, Int](0))((acc, _) => acc.flatMap(s => okay.async(s + 1)))
    assertEquals(run(CatsEffect.lift(p)), 100000)
  }
}
