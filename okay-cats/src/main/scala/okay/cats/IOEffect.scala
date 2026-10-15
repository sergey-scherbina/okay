package okay.cats

import okay.{!, +, Async, Within}
import _root_.cats.effect.IO

/**
 * CATS-EFFECT'S `IO` AS AN EFFECT OF THE PROGRAM (specs/foreign-effects-in-tree.md). The effect is `IO` itself —
 * `IO(1).perform : Int ! IO` — so an IO is kept as one operation and the runtime is chosen when the program is
 * handled, not when it is built: `p.toIO` runs it in ONE fiber (cats-effect's masking and cancellation, as
 * [[CatsEffect.toIO]]), `p.via[IO]` lowers each IO to okay's `Async` (okay's runners), and any okay handler can
 * answer the IO operations instead (a stub in a test).
 */
extension [A, R[_]](p: A ! R)
  /** a program over `IO` and `Async` as one IO: an IO operation is itself, an `Async` one is IO's own
   * (`IO.delay` / `IO.async`), a bind is one `flatMap` */
  def toIO(using Within[R, IO + Async]): IO[A] = IOEffect.run(p.at[IO + Async])

object IOEffect:
  /** the program walked, for the reason [[CatsEffect.toIO]] gives */
  private[cats] def run[A](p: A ! IO + Async): IO[A] = Walking.toIO[IO, A](p)([X] => (io: IO[X]) => io)
