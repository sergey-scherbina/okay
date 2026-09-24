package okay2.zio

import _root_.zio.{ZIO, Task, Chunk}
import _root_.zio.stream.ZStream
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * The zio side of okay2 (specs/okay2.md, stage 2 — interop).
 *
 * - THE `Zio` ROW: an operation of it IS a `Task[A]`, so `Zio.lift`
 *   puts a ZIO in a program and `Zio.run` folds a program whose every
 *   operation has an `IntoZ` into one ZIO. Where the Scala 3 core's
 *   `toZIO` runs an `Async` program under `attemptBlocking`, okay2 has
 *   no `Async` yet: this is the freer-monad interop, the tree
 *   interpreted in ZIO, nothing blocked.
 * - A FOLD INTO ANY ZIO: `foldTo` with an environment and an error
 *   type, by an `IntoZ[R, Rz, E]`; the walk recurses inside ZIO's
 *   `flatMap`, which is stack-safe, so a million operations cost no
 *   stack.
 * - A WRITER PROGRAM AS A `ZStream`: `toZStream` unfolds the program
 *   one told value per step, the other effects run in ZIO between the
 *   elements. `fromZStream` is the other way, by `runCollect`: the
 *   Scala 3 core's `fromZStream` pulls through an iterator under a
 *   runtime, and a scoped pull that survives across a program's
 *   operations needs a resource effect okay2 does not have yet
 *   (backlog `okay2-stage2`).
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
  def toZStream[Rz, E, W, A, G <: Row](p: Free[Writer[W] with G, A])(implicit h: IntoZ[G, Rz, E]): ZStream[Rz, E, W] =
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

  /** a ZStream as a Writer program: collected in ONE `Zio` operation,
   * then told one by one (see the object's note on why not a pull) */
  def fromZStream[W](s: ZStream[Any, Throwable, W]): Unit ! (Writer[W] + Zio) = {
    type Row = Writer[W] + Zio
    Zio.lift(s.runCollect).at[Row].flatMap { ch =>
      Effects.loop[Int, Unit, Row](0) { i =>
        if (i < ch.length) Writer.tell(ch(i)).at[Row].map(_ => Left(i + 1))
        else pure[Row, Either[Int, Unit]](Right(()))
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
