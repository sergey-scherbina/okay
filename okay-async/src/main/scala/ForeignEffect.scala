package okay

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.parasitic

/**
 * A FOREIGN effect's value as an okay program (specs/direct-foreign-mark.md).
 * With a given `ForeignEffect[M]` in scope, `m.?` / `m.reflect` on an
 * `M[A]` inside a `direct` block binds `lift(m)` — when `G` is a member of
 * the block's row, and is refused naming the row otherwise.
 *
 * The class names no foreign library: each library's instance lives in
 * its own interop module (okay-zio, okay-cats), behind that module's given
 * import. `Future` is the standard library's, so its instance is here.
 */
trait ForeignEffect[M[_]]:
  /** the okay effect the value becomes — a MEMBER, not a parameter: the
   * macro searches `ForeignEffect[M]` knowing only M, and a wildcard for
   * a higher-kinded parameter finds nothing (observed, the first cut) */
  type G[+X]
  def lift[A](m: M[A]): A ! G

object ForeignEffect:
  /** a Future is an Await on its completion — no parked thread under the
   * callback runner; a Future cannot be cancelled, so the canceller only
   * drops the callback's effect (the drive ignores a late answer) */
  given future: ForeignEffect[Future] with
    type G[+X] = Async[X]
    def lift[A](m: Future[A]): A ! Async =
      Async.await[A] { k =>
        m.onComplete(t => k(t.toEither))(using parasitic)
        () => ()
      }
