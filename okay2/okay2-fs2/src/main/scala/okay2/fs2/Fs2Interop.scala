package okay2.fs2

import _root_.fs2.{Stream, Pull}
import _root_.cats.effect.IO
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
 * `fs2.Stream[IO, W]` as a Writer program that pulls ONE element per
 * `Io` operation, so a consumer that stops early never runs the rest
 * of the stream. Where the Scala 3 core's `Fs2Interop` moves `Chunks`,
 * okay2 has no chunked source yet: elements, one at a time, is the
 * honest shape (backlog `okay2-stage2`, Stream/Fold).
 */
object Fs2Interop {

  /**
   * The told values as a stream, the other effects run in F. Every
   * step is a `++` or a `flatMap` on the stream, both lazy, so the
   * program is walked as the stream is pulled and a million tells cost
   * no stack.
   */
  def toFs2[F[_], W, A, G <: Row](p: Free[Writer[W] with G, A])(implicit h: Into[G, F]): Stream[F, W] =
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
   * An IO stream as a Writer program: one element pulled per `Io`
   * operation, told, then the rest. The pull is `uncons1` compiled to
   * its first step — the tail is the stream after that element, so
   * nothing before it runs twice and nothing after it runs until asked.
   */
  def fromFs2[W](s: Stream[IO, W]): Unit ! (Writer[W] + Io) = {
    type Row = Writer[W] + Io
    def step(s: Stream[IO, W]): IO[Option[(W, Stream[IO, W])]] =
      s.pull.uncons1.flatMap {
        case Some((w, tl)) => Pull.output1((w, tl))
        case None => Pull.done
      }.stream.compile.last
    def go(s: Stream[IO, W]): Unit ! Row =
      Io.lift(step(s)).at[Row].flatMap {
        case None => pure[Row, Unit](())
        case Some((w, tl)) => Writer.tell(w).at[Row].flatMap(_ => go(tl))
      }
    go(s)
  }
}
