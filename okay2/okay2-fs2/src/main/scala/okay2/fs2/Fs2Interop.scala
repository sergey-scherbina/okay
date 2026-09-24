package okay2.fs2

import scala.annotation.unused

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

import _root_.fs2.{Stream, Chunk}
import _root_.cats.effect.IO
import _root_.cats.effect.std.Queue
import _root_.cats.effect.unsafe.IORuntime
import okay2._
import okay2.Free.{Return, Inject, Bind}
import okay2.cats.{Io, CatsInterop}
import CatsInterop.Into

/**
 * The fs2 side of okay2 (specs/okay2.md, stage 2 — interop).
 *
 * A `Writer[W]` program IS a stream (Writer.scala): telling emits, and
 * the program's other effects are what happens between the elements.
 * `toFs2` reads it as an `fs2.Stream[F, W]`, the residual row's
 * operations run in `F` by an `Into`; `fromFs2` reads an
 * `fs2.Stream[IO, W]` as a Writer program that takes ONE CHUNK per
 * `Io` operation under a `Resource` scope, so a consumer that stops
 * early stops the stream and runs its finalizers.
 */
object Fs2Interop {

  /**
   * The told values as a stream, the other effects run in F. Every
   * step is a `++` or a `flatMap` on the stream, both lazy, so the
   * program is walked as the stream is pulled and a million tells cost
   * no stack.
   */
  def toFs2[F[_], W, A, G <: Row](p: Free[Writer[W] with G, A])(implicit h: Into[G, F], @unused d: Distinct[Writer[W] with G]): Stream[F, W] =
    toFs2At[F, W, A, G](p)(h)

  /** `toFs2` at the handler's own shape */
  def toFs2At[F[_], W, A, G <: Row](p: Free[Writer[W] with G, A])(h: Into[G, F]): Stream[F, W] = {
    def go(x: Free[Writer[W] with G, A]): Stream[F, W] = Free.resume(x) match {
      case Return(_) => Stream.empty
      case Inject(e) => go(Bind(Inject[Writer[W] + G, A](e), (x: A) => Return[Writer[W] + G, A](x)))
      case Bind(Inject(e), k) =>
        Split.split[Writer[W], G, Any, Stream[F, W]](e) {
          case Writer.Say(w) => Stream.emit(w) ++ go(k(()))
        } { g =>
          Stream.eval(h.applyOp[Any](g)).flatMap(x => go(k(x)))
        }
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    go(p)
  }

  /**
   * An IO stream as a Writer program, SCOPED (okay2-interop-async): the
   * fs2 stream runs on its own runtime into a bounded queue, chunk for
   * chunk — `offer` suspends THEIR fiber when the queue is full, which is
   * the backpressure — and the program takes one chunk per `Io`
   * operation and tells its elements. The running stream is a RESOURCE:
   * when the scope ends, early or not, the release cancels the fs2 fiber
   * and waits for it, so the stream's own finalizers (a `bracket`, an
   * open file) run then — which the Scala 3 core's `fromFs2` leaves to
   * the stream reaching its end.
   *
   * The first cut here pulled `uncons1` and `compile`d each step: every
   * step closed the stream's scope, so a stream holding a resource would
   * have had it released under the tail still being read.
   */
  def fromFs2[W](s: Stream[IO, W], capacity: Int = 64)(implicit rt: IORuntime): Unit ! (Writer[W] + Io + Resource) = {
    type Row = Writer[W] + Io + Resource
    type Item = Option[Either[Throwable, Chunk[W]]]
    Resource.acquire[(Queue[IO, Item], () => Future[Unit])] {
      val q = Queue.bounded[IO, Item](capacity).unsafeRunSync()
      val run = s.chunks.evalMap(ch => q.offer(Some(Right(ch)))).compile.drain
        .flatMap(_ => q.offer(None))
        .handleErrorWith(e => q.offer(Some(Left(e))))
      (q, run.unsafeRunCancelable())
    } { case (_, cancel) => Await.result(cancel(), Duration.Inf) }.at[Row].flatMap { case (q, _) =>
      def go(): Unit ! Row = Io.lift(q.take).at[Row].flatMap {
        case None => pure[Row, Unit](())
        case Some(Left(e)) => Io.lift(IO.raiseError[Unit](e)).at[Row]
        case Some(Right(ch)) => tellAll[W](ch).at[Row].flatMap(_ => go())
      }
      go()
    }
  }

  /** a chunk told in order */
  def tellAll[W](ch: Chunk[W]): Unit ! Writer[W] =
    Effects.loop[Int, Unit, Writer[W]](0) { i =>
      if (i < ch.size) Writer.tell(ch(i)).map(_ => Left(i + 1))
      else pure[Writer[W], Either[Int, Unit]](Right(()))
    }
}
