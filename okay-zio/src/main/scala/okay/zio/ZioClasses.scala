package okay.zio

import _root_.zio.ZIO
import _root_.zio.stream.ZStream

/**
 * okay's class ladder over ZIO's types (specs/interop-classes.md).
 * ZIO core has no type classes of its own — `zipPar`, `validatePar` are
 * methods on the data type — so this direction is the whole of it: the
 * monad of ZioMonad.scala, a stream monad, and the PARALLEL applicative
 * cats calls `Parallel` and ZIO calls `zipWithPar`.
 */

/** a ZStream under okay's Monad: `flatMap` is ZStream's (each element's
 * stream in turn), so `okay.traverse` over streams is their cartesian
 * product, the list monad's reading */
given zstreamMonad[R, E]: okay.Monad[[A] =>> ZStream[R, E, A]] with
  def pure[A](a: A): ZStream[R, E, A] = ZStream.succeed(a)
  override def fmap[A, B](a: ZStream[R, E, A], f: A => B): ZStream[R, E, B] = a.map(f)
  extension [A](a: ZStream[R, E, A])
    def flatMap[B](f: A => ZStream[R, E, B]): ZStream[R, E, B] = a.flatMap(f)

/** ZStream's `flatMap` builds a stream and calls nothing, so the
 * `flatMap` recursion is its loop (specs/eager-carrier-depth.md) */
given zstreamTailRecM[R, E]: okay.TailRecM[[A] =>> ZStream[R, E, A]] = okay.TailRecM.deferring

object ZioClasses:

  /**
   * `app` by `zipWithPar`: both leaves run at once, the first failure
   * interrupts the other. NOT a given: beside `zioMonad` it would make
   * every `Applicative[ZIO]` search ambiguous, and the choice between
   * sequential and parallel belongs at the call site —
   * `okay.traverse(xs)(f)(using ZioClasses.parApplicative)`.
   */
  def parApplicative[R, E]: okay.Applicative[[A] =>> ZIO[R, E, A]] = new:
    def pure[A](a: A): ZIO[R, E, A] = ZIO.succeed(a)
    override def fmap[A, B](a: ZIO[R, E, A], f: A => B): ZIO[R, E, B] = a.map(f)
    extension [A, B](f: ZIO[R, E, A => B])
      def app(a: ZIO[R, E, A]): ZIO[R, E, B] = f.zipWithPar(a)(_(_))
