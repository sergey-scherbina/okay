package okay.freer

/**
 * THE ROW OF A BLOCK'S PROGRAM TYPE, recovered by the compiler:
 * `[X] =>> Free[R, X]` gives back R (reader-env, 2026-09-16).
 */
trait RowOf[F[_]]:
  type R[+_]
object RowOf:
  given [R0[+_]]: RowOf[[X] =>> Free[R0, X]] with
    type R[+A] = R0[A]
