package okay


import okay.freer.*


import scala.annotation.implicitNotFound
import scala.concurrent.Future

/**
 * A FOREIGN VALUE THAT CROSSES INTO OKAY (specs/interop-compose.md):
 * a cats `IO`, a ZIO `Task`, a kyo computation, a `Future` — as one
 * `Async` program, under ONE name, `asOkay`.
 *
 * ONE NAME, ONE CLASS, because the first cut had one `asOkay` extension
 * per interop module and they did not overload: a file importing all
 * three saw only the last import's, and kyo's — whose receiver `A < S`
 * every plain value converts to — claimed values that were not kyo's at
 * all. Here the extension is defined once and each interop module adds
 * an instance behind its own given import. The instance is chosen by the
 * value's WHOLE type, not by an `M[_]` hole, so kyo's `A < S` (value
 * first) is found as easily as `IO[A]`.
 *
 * Every value crosses as `A ! Async`. A typed ZIO error or environment
 * is a row of its own and crosses by the `direct` mark instead
 * (`ForeignEffect`, specs/zio-typed-row.md).
 */
@implicitNotFound("no ToOkay[${T}, ${A}]: nothing turns a ${T} into an okay program here.\nThe interop modules bring theirs with the given import: `import okay.cats.given`, `okay.zio.given`, `okay.kyo.given`.")
trait ToOkay[-T, +A]:
  def apply(t: T): A ! Async

object ToOkay:
  /** a Future is an Await on its completion — [[ForeignEffect.future]] */
  given future[A]: ToOkay[Future[A], A] = f => ForeignEffect.future.lift(f)

extension [T, A](t: T)(using c: ToOkay[T, A])
  /** this foreign value as an okay program */
  def asOkay: A ! Async = c(t)

extension [X, T, A](f: X => T)(using c: ToOkay[T, A])
  /** this foreign function as an okay one — what `>=>` composes across
   * libraries: `parse.asOkay >=> double.asOkay >=> show` */
  def asOkay: X => A ! Async = x => c(f(x))
