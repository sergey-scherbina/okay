package okay2.zio

import scala.annotation.unused

import _root_.zio.{ZIO, Task, Chunk, Runtime, Scope, Unsafe, Exit}
import _root_.zio.stream.ZStream
import okay2._
import okay2.Free.{Return, Inject, Bind}
import okay2.async.{Async, CanBlock, Fiber, Scheduler}

/**
 * The zio side of okay2 (specs/okay2.md, stage 2 — interop).
 *
 * - THE `Zio` ROW: an operation of it IS a `Task[A]`, so `Zio.lift`
 *   puts a ZIO in a program and `Zio.run` folds a program whose every
 *   operation has an `IntoZ` into one ZIO — the freer-monad interop,
 *   the tree interpreted in ZIO, nothing blocked.
 * - THE `Async` BRIDGE, as the Scala 3 core's (okay2-interop-async):
 *   `toZIO` runs an `Async` program under `attemptBlocking`; `fromZIO`
 *   is a ZIO as an `Async` operation, answered by callback and
 *   interrupted by the waiting side's canceller (the Scala 3 core parks
 *   a virtual thread in `unsafe.run`); `scheduler` is okay2's Scheduler
 *   on the ZIO runtime.
 * - A FOLD INTO ANY ZIO: `foldTo` with an environment and an error
 *   type, by an `IntoZ[R, Rz, E]`; the walk recurses inside ZIO's
 *   `flatMap`, which is stack-safe, so a million operations cost no
 *   stack.
 * - A WRITER PROGRAM AS A `ZStream`: `toZStream` unfolds the program
 *   one told value per step, the other effects run in ZIO between the
 *   elements. `fromZStream` is the other way: a SCOPED PULL under
 *   `Resource` — the stream's scope opened once, one chunk per `Zio`
 *   operation, and the scope closed when the program's scope ends,
 *   early or not (the Scala 3 core closes it only when its iterator
 *   runs out).
 */
object ZioInterop {

  /** the natural transformation from a row's operations into a ZIO
   * with environment Rz and error E; a union's is made of its parts */
  trait IntoZ[R <: Row, -Rz, +E] { def applyOp[X](op: Any): ZIO[Rz, E, X] }

  object IntoZ {
    /** one signature's, typed (as `CatsInterop.Into.Of`) */
    abstract class Of[F <: Row, -Rz, +E] extends IntoZ[F, Rz, E] {
      def apply[X](e: F#Op[X]): ZIO[Rz, E, X]
      final def applyOp[X](op: Any): ZIO[Rz, E, X] = apply(Split.only[F, X](op))
    }

    /** explicit, as `CatsInterop.Into.union` and for its reason */
    def union[F <: Row, G <: Row, Rz, E](implicit T: TypeableK[F], f: IntoZ[F, Rz, E], g: IntoZ[G, Rz, E], d: Distinct[F + G]): IntoZ[F + G, Rz, E] = {
      val _ = d
      new IntoZ[F + G, Rz, E] {
        def applyOp[X](op: Any): ZIO[Rz, E, X] = if (T.test(op)) f.applyOp[X](op) else g.applyOp[X](op)
      }
    }

    /** Pure has no operations: never applied */
    implicit def pure[Rz, E]: IntoZ[okay2.Pure, Rz, E] = new IntoZ[okay2.Pure, Rz, E] {
      def applyOp[X](op: Any): ZIO[Rz, E, X] = throw new IllegalStateException("an operation in a Pure program: " + op)
    }
  }

  /** interpret a program into ZIO: values by `succeed`, operations by
   * `h`, the walk inside `flatMap` */
  def foldTo[Rz, E, A, R <: Row](p: Free[R, A])(h: IntoZ[R, Rz, E]): ZIO[Rz, E, A] = Free.resume(p) match {
    case Return(a) => ZIO.succeed(a)
    case Inject(e) => h.applyOp[A](e)
    case Bind(Inject(e), k) => h.applyOp[Any](e).flatMap(x => foldTo[Rz, E, A, R](k(x))(h))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /**
   * The told values as a ZStream, the other effects run in ZIO: one
   * `unfoldZIO` step per told value, the residual operations between
   * two tells folded into the step's ZIO.
   */
  def toZStream[Rz, E, W, A, G <: Row](p: Free[Writer[W] with G, A])(implicit h: IntoZ[G, Rz, E], @unused d: Distinct[Writer[W] with G]): ZStream[Rz, E, W] =
    toZStreamAt[Rz, E, W, A, G](p)(h)

  /** `toZStream` at the handler's own shape */
  def toZStreamAt[Rz, E, W, A, G <: Row](p: Free[Writer[W] with G, A])(h: IntoZ[G, Rz, E]): ZStream[Rz, E, W] = {
    type P = A ! (Writer[W] + G)
    def step(x: P): ZIO[Rz, E, Option[(W, P)]] = Free.resume(x) match {
      case Return(_) => ZIO.none
      case Inject(e) => step(Bind(Inject[Writer[W] + G, A](e), (x: A) => Return[Writer[W] + G, A](x)))
      case Bind(Inject(e), k) =>
        Split.split[Writer[W], G, Any, ZIO[Rz, E, Option[(W, P)]]](e) {
          case Writer.Say(w) => ZIO.some((w, k(())))
        } { g =>
          h.applyOp[Any](g).flatMap(x => step(k(x)))
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    ZStream.unfoldZIO(p)(step)
  }

  /**
   * A ZStream as a Writer program, SCOPED: the stream's scope and its
   * pull are opened once, as a `Resource`, and every `Zio` operation
   * pulls one chunk and tells its elements. The release closes the
   * scope, so a consumer that stops early — or fails — runs the
   * stream's finalizers then, not at some later end of the stream.
   * (The first cut collected the whole stream in ONE operation.)
   */
  def fromZStream[W](s: ZStream[Any, Throwable, W], runtime: Runtime[Any] = Runtime.default): Unit ! (Writer[W] + Zio + Resource) = {
    type Row = Writer[W] + Zio + Resource
    def unsafe[A](z: ZIO[Any, Throwable, A]): A = Unsafe.unsafe { implicit u => runtime.unsafe.run(z).getOrThrowFiberFailure() }
    Resource.acquire[(Scope.Closeable, ZIO[Any, Option[Throwable], Chunk[W]])] {
      val scope = unsafe(Scope.make)
      (scope, unsafe(scope.extend[Any](s.toPull)))
    } { case (scope, _) => unsafe(scope.close(Exit.unit)) }.at[Row].flatMap { case (_, pull) =>
      val next: Task[Option[Chunk[W]]] = pull.map(Option(_)).catchAll {
        case None => ZIO.none
        case Some(e) => ZIO.fail(e)
      }
      def go(): Unit ! Row = Zio.lift(next).at[Row].flatMap {
        case None => pure[Row, Unit](())
        case Some(ch) => tellAll[W](ch).at[Row].flatMap(_ => go())
      }
      go()
    }
  }

  /** run an okay2 `Async` program as a ZIO: it may park, so it runs as
   * a blocking ZIO, as the Scala 3 core's `toZIO` */
  def toZIO[A](p: => Free[Async, A])(implicit cb: CanBlock): Task[A] =
    ZIO.attemptBlocking(Effects.run(Async.run[A, Pure](p)))

  /** a ZIO as an `Async` operation: started on the runtime, answered by
   * its completion, interrupted when the waiting side gives up */
  def fromZIO[A](z: Task[A], runtime: Runtime[Any] = Runtime.default): A ! Async =
    Async.await[A] { k =>
      val f = Unsafe.unsafe { implicit u => runtime.unsafe.runToFuture(z) }
      f.onComplete(t => k(t.toEither))(scala.concurrent.ExecutionContext.parasitic)
      () => { val _ = f.cancel(); () }
    }

  /** OUR Scheduler on THEIR runtime: `fork` runs the program as a
   * blocking ZIO, the fiber's completion is its future's, `cancel`
   * interrupts it */
  def scheduler(runtime: Runtime[Any] = Runtime.default)(implicit cb: CanBlock): Scheduler = new Scheduler {
    def fork[A](prog: () => A ! Async): Fiber[A] = {
      val f = Unsafe.unsafe { implicit u => runtime.unsafe.runToFuture(toZIO(prog())) }
      new Fiber[A] {
        def onComplete(k: Either[Throwable, A] => Unit): Unit =
          f.onComplete(t => k(t.toEither))(scala.concurrent.ExecutionContext.parasitic)
        def cancel(): Unit = { val _ = f.cancel(); () }
      }
    }
  }

  /** a Chunk told in order, as a program */
  def tellAll[W](ch: Chunk[W]): Unit ! Writer[W] =
    Effects.loop[Int, Unit, Writer[W]](0) { i =>
      if (i < ch.length) Writer.tell(ch(i)).map(_ => Left(i + 1))
      else pure[Writer[W], Either[Int, Unit]](Right(()))
    }
}

/**
 * The `Zio` row: an operation of it IS a `Task[A]`. `Effect.of` tests
 * by the class `zio.ZIO`, which every ZIO value is an instance of.
 */
sealed trait Zio extends Row { type Op[+A] = Task[A] }

object Zio {
  import ZioInterop.{IntoZ, foldTo}

  implicit val effect: Effect[Zio] = Effect.of[Zio]

  /** how a forwarded Task fails to a `Resource` scope: by an error, a
   * defect or an interruption — every way the residual holding the
   * scope's finalizers is abandoned — so each runs the hook first */
  implicit val failing: Failing[Zio] = new Failing[Zio] {
    def guard(e: Any, onFailure: () => Unit): Any =
      Split.over[Zio, Any](e)(t => t.onExit(ex => if (ex.isSuccess) ZIO.unit else ZIO.succeed(onFailure())))
  }

  /** a Task as an operation of the row */
  def lift[A](z: Task[A]): A ! Zio = Free.inject[Zio, A](z)

  /** the row's operations are already ZIO */
  implicit val into: IntoZ[Zio, Any, Throwable] = new IntoZ.Of[Zio, Any, Throwable] {
    def apply[X](e: Task[X]): Task[X] = e
  }

  /** fold a program into one Task: every part of the row needs an
   * `IntoZ[_, Any, Throwable]` */
  def run[A, R <: Row](p: Free[R, A])(implicit h: IntoZ[R, Any, Throwable]): Task[A] = foldTo[Any, Throwable, A, R](p)(h)
}
