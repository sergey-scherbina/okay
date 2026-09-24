package okay2.cats

import _root_.cats.{Monad, MonadError, StackSafeMonad, ~>}
import _root_.cats.effect.IO
import _root_.cats.free.{Free => CFree}
import okay2._
import okay2.Free.{Return, Inject, Bind}

/**
 * The cats side of okay2 (specs/okay2.md, stage 2 — interop).
 *
 * Three things, each the counterpart of what okay-cats gives the
 * Scala 3 core where okay2 has the machinery for it:
 *
 * - INSTANCES: every program row is a `StackSafeMonad` (the tree's
 *   flatMap is a node, so `tailRecM` through it costs no stack), and a
 *   row with `Throws[E]` at its head is a `MonadError` (`raiseError` is
 *   `raise`, `handleErrorWith` is `recover`).
 * - A FOLD INTO ANY MONAD: `foldTo` interprets a program into `M` by an
 *   `Into[R, M]` — the natural transformation from the row's operations
 *   — through `Monad.tailRecM`, so it is stack-safe wherever `M`'s
 *   `tailRecM` is. `Into` composes along `+` as `Handler.union` does.
 * - THE `Io` ROW: a signature whose operations ARE `IO` values
 *   (`type Op[+A] = IO[A]`), so `Io.lift(io)` is an operation of the
 *   row and `Io.run` folds a program whose every operation has an
 *   `Into[_, IO]` into one `IO`. Where the Scala 3 core's `toIO` runs an
 *   `Async` program under `IO.blocking`, okay2 has no `Async` yet: this
 *   is the freer-monad interop instead — the tree interpreted in the
 *   target, nothing blocked.
 * - `cats.free.Free` BOTH WAYS: `toCats` is the tree re-expressed over
 *   cats' own `Free`, `fromCats` folds cats' back into ours by the
 *   `StackSafeMonad` above.
 */
object CatsInterop extends CatsInteropLow {

  /** the natural transformation from a row's operations into M — what
   * `foldTo` needs, and what a union's is made of, member by member.
   * It takes the operation as `Any`, as `Handler` does (a row's `#Op`
   * is not a type to read at, Row.scala); one signature's is written
   * typed, by `Into.Of`. Invariant, as `Handler` is and for its reason:
   * a union's own implicit must not answer for one of its parts. */
  trait Into[R <: Row, M[_]] { def applyOp[X](op: Any): M[X] }

  object Into {
    /** one signature's, typed — sound because the row admits only F's
     * operations to it */
    abstract class Of[F <: Row, M[_]] extends Into[F, M] {
      def apply[X](e: F#Op[X]): M[X]
      final def applyOp[X](op: Any): M[X] = apply(Split.only[F, X](op))
    }

    /** a union is interpreted by its parts: split by the F test,
     * exactly as `Handler.union` — and, like it, an EXPLICIT combinator:
     * `implicit val h: Into[Produce + Io, IO] = Into.union[Produce, Io, IO]`.
     * An implicit rule over `F + G` cannot take an intersection apart
     * (stage 8: it matches every type and the search diverges) */
    def union[F <: Row, G <: Row, M[_]](implicit T: TypeableK[F], f: Into[F, M], g: Into[G, M]): Into[F + G, M] =
      new Into[F + G, M] {
        def applyOp[X](op: Any): M[X] = if (T.test(op)) f.applyOp[X](op) else g.applyOp[X](op)
      }

    /** Pure has no operations: never applied */
    implicit def pure[M[_]]: Into[okay2.Pure, M] = new Into[okay2.Pure, M] {
      def applyOp[X](op: Any): M[X] = throw new IllegalStateException("an operation in a Pure program: " + op)
    }
  }

  /**
   * Interpret a program into M: values by `pure`, operations by `h`,
   * the tree walked by `tailRecM` so that a million operations cost M's
   * `tailRecM` and no stack. A lone operation is the Bind with a pure
   * continuation, as in every okay2 handler.
   */
  def foldTo[M[_], A, R <: Row](p: Free[R, A])(h: Into[R, M])(implicit M: Monad[M]): M[A] =
    M.tailRecM[A ! R, A](p) { x =>
      Free.resume(x) match {
        case Return(a) => M.pure(Right(a))
        case Inject(e) => M.map(h.applyOp[A](e))(a => Right(a))
        case Bind(Inject(e), k) => M.map(h.applyOp[Any](e))(v => Left(k(v)))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
    }

  /** the same, into IO, for the row whose parts all have an `Into[_, IO]` */
  def toIO[A, R <: Row](p: Free[R, A])(implicit h: Into[R, IO]): IO[A] = foldTo[IO, A, R](p)(h)

  /** the tree over cats' own `Free`: the same nodes, cats' `Suspend`
   * and `FlatMapped` for our `Inject` and `Bind`. For ONE signature F:
   * cats' `Free` is over a functor, and a row of several has no single
   * operation type to name (Row.scala) */
  def toCats[R <: Row, A](p: Free[R, A]): CFree[R#Op, A] = Free.resume(p) match {
    case Return(a) => CFree.pure(a)
    case Inject(e) => CFree.liftF[R#Op, A](Split.only[R, A](e))
    case Bind(Inject(e), k) => CFree.liftF[R#Op, Any](Split.only[R, Any](e)).flatMap(x => toCats[R, A](k(x)))
    case other => throw new IllegalStateException("resume left a non-head form: " + other)
  }

  /** cats' `Free` folded back into ours: every `Suspend` re-injected,
   * every `FlatMapped` a `Bind` — `foldMap` at the program monad */
  def fromCats[R <: Row, A](c: CFree[R#Op, A]): A ! R = {
    type Prog[X] = X ! R
    c.foldMap[Prog](new (R#Op ~> Prog) {
      def apply[X](e: R#Op[X]): X ! R = Free.inject[R, X](e)
    })(instances.programMonad[R])
  }

}

/**
 * The cats instances for programs — `import okay2.cats.instances._`,
 * the cats convention, since a program's type is the core's `Free`
 * and this module cannot put anything in its companion.
 */
object instances extends CatsInteropLow {
  /**
   * A row with `Throws[E]` at its head is a `MonadError`: `raiseError`
   * performs the failure, `handleErrorWith` answers it inside the row
   * (`recover`), so what follows neither knows nor cares. More specific
   * than `programMonad`, so a `Monad` query for such a row finds this.
   */
  implicit def programMonadError[E, F <: Row]: MonadError[({ type L[A] = A ! (Throws[E] + F) })#L, E] =
    new MonadError[({ type L[A] = A ! (Throws[E] + F) })#L, E] with StackSafeMonad[({ type L[A] = A ! (Throws[E] + F) })#L] {
      def pure[A](a: A): A ! (Throws[E] + F) = okay2.pure(a)
      def flatMap[A, B](fa: Free[Throws[E] with F, A])(f: A => B ! (Throws[E] + F)): B ! (Throws[E] + F) = fa.flatMap(f)
      def raiseError[A](e: E): A ! (Throws[E] + F) = Throws.raise[E, A](e).at[Throws[E] + F]
      def handleErrorWith[A](fa: Free[Throws[E] with F, A])(f: E => A ! (Throws[E] + F)): A ! (Throws[E] + F) = new ThrowsOps[A, E, F](fa).recover(f)
    }
}

trait CatsInteropLow {
  /** every program row is a monad, stack-safe through the tree */
  implicit def programMonad[R <: Row]: StackSafeMonad[({ type L[A] = A ! R })#L] =
    new StackSafeMonad[({ type L[A] = A ! R })#L] {
      def pure[A](a: A): A ! R = okay2.pure(a)
      def flatMap[A, B](fa: Free[R, A])(f: A => B ! R): B ! R = fa.flatMap(f)
    }
}

/**
 * The `Io` row: an operation of it IS an `IO[A]`. `Effect.of` tests by
 * the class `cats.effect.IO`, which every `IO` value is an instance
 * of, so the row splits like any other signature.
 */
sealed trait Io extends Row { type Op[+A] = IO[A] }

object Io {
  import CatsInterop.{Into, foldTo}

  implicit val effect: Effect[Io] = Effect.of[Io]

  /** an IO as an operation of the row */
  def lift[A](io: IO[A]): A ! Io = Free.inject[Io, A](io)

  /** the row's operations are already IO */
  implicit val into: Into[Io, IO] = new Into.Of[Io, IO] {
    def apply[X](e: IO[X]): IO[X] = e
  }

  /** fold a program into one IO: every part of the row needs an
   * `Into[_, IO]` — `Io`'s is above, a handled effect's is whatever
   * answers it in IO, and `Pure`'s is never applied */
  def run[A, R <: Row](p: Free[R, A])(implicit h: Into[R, IO]): IO[A] = foldTo[IO, A, R](p)(h)
}
