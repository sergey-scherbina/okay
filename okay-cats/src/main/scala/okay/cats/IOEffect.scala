package okay.cats

import okay.{Async, TypeableK}

import okay.freer.{split}
import okay.freer.{+}
import okay.freer.{!, effect}
import okay.freer.!.*
import okay.freer.Row.up
import _root_.cats.effect.IO

/**
 * CATS-EFFECT'S `IO` AS AN EFFECT OF THE TREE (specs/foreign-effects-in-tree.md). The row member is `IO` itself —
 * `IO(1).perform : Int ! IO` — so an IO is kept as one operation and the runtime is chosen when the program is
 * handled, not when it is built: `p.toIO` runs it in ONE fiber (cats-effect's masking and cancellation, as
 * [[CatsEffect.toIO]]), `p.via[IO]` lowers each IO to okay's `Async` (okay's runners), and any okay handler can
 * answer the IO operations instead (a stub in a test).
 */
given ioTypeable: TypeableK[IO] = TypeableK.derived[IO]

extension [A, R[+_]](p: A ! R)
  /** a program over `IO` and `Async` as one IO: an IO operation is itself, an `Async` one is IO's own
   * (`IO.delay` / `IO.async`), a bind is one `flatMap` */
  def toIO(using okay.freer.Row.Sub[R, IO + Async]): IO[A] = IOEffect.run(p.up[IO + Async])

object IOEffect:
  /** a walk, not a fold, for the reason [[CatsEffect.toIO]] gives (a law about where binds sit) */
  private[cats] def run[A](p: A ! IO + Async): IO[A] = (p.resume: @unchecked) match
    case Return(a) => IO.pure(a)
    case Inject(e) => step(e)
    case Bind(Inject(e), k) => step(e).flatMap(x => run(k(x)))

  private def step[X](e: (IO + Async)[X]): IO[X] =
    split[IO, Async](e)(io => io)(a => CatsEffect.toIO(CatsEffect(effect[CatsFx + Async, X](a))))
