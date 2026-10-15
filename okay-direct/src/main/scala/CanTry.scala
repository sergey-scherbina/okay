package okay

import scala.util.Try
import scala.annotation.implicitNotFound

/**
 * The seam direct-try stands on for a block whose carrier is NOT a
 * program: how that carrier CATCHES a JVM throw from the computation's
 * own code. (A block over programs `A ! Es` needs none: its `try` is
 * `Direct.tryProgram`, the kernel's `recover`.) Strict carriers (Option,
 * Either, List) run at construction, so a try around the value covers
 * everything. A context function (`E ?=> X`, direct-try-ctx) is lazy —
 * a closure over its environment, not run until applied — so its
 * instance defers the try to APPLICATION time (`ctxFn` below). The
 * instances are NAMED, not a catch-all: a lazy carrier given the STRICT
 * instance would try the CONSTRUCTION and never the run, and the catch
 * would silently never fire — so an F without an instance is a compile
 * error that says so, and a strict carrier of your own declares itself
 * in one line: `given CanTry[M] = CanTry.strict`.
 *
 * okay-direct's own since okay-freer's was retired from the platform
 * (its `rows` instance guarded okay-freer's tree, which no block emits).
 */
@implicitNotFound("no CanTry[${F}]: `try` in a direct block needs to know how ${F} catches a throw.\nStrict monads (Option, Either, List, Vector, Try), programs (A ! Es), and context functions (E ?=> X)\nhave instances; for a strict monad of your own declare `given CanTry[${F}] = CanTry.strict` — a\nCont-shaped LAZY monad has no honest instance: its body runs after the try, catch in the run instead.")
trait CanTry[F[_]]:
  def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A]

object CanTry:
  /** strict carriers: the whole computation happens at construction */
  def strict[F[_]]: CanTry[F] = new:
    def tryIn[A](fa: => F[A])(h: Throwable => F[A]): F[A] =
      try fa catch case e: Throwable => h(e)

  given option: CanTry[Option] = strict
  given either: [E] => CanTry[[X] =>> Either[E, X]] = strict
  given list: CanTry[List] = strict
  given vector: CanTry[Vector] = strict
  given tries: CanTry[Try] = strict

  /** direct-try-ctx: a context function is lazy in its environment
   * (a closure, not run until applied), so the try must defer to
   * APPLICATION time, not construction. Also sidesteps the dotty 3.7.4
   * erasure crash ("bad adapt for M\$proxy2.pure(a)") the STRICT shape
   * hit when tried here during the 2026-09-02 audit */
  given ctxFn: [E] => CanTry[[X] =>> E ?=> X] = new:
    def tryIn[A](fa: => (E ?=> A))(h: Throwable => (E ?=> A)): E ?=> A =
      (e: E) ?=> (try fa(using e) catch case ex: Throwable => h(ex)(using e))

  /** programs: the kernel's `recover`, as a block's `try` over them is (`Direct.tryProgram`) */
  given programs: [Es[_]] => CanTry[[X] =>> X ! Es] = new:
    def tryIn[A](fa: => A ! Es)(h: Throwable => A ! Es): A ! Es = Direct.tryProgram[A, Es](fa)(h)
