package okay

import okay.Row.*

/**
 * specs/shift-effect.md: `shift`/`reset` as an effect in `A ! F`. A
 * capture is `Shift % R` in the row; `reset` is its handler. Two
 * implementations behind one API, so one suite runs on both.
 */
final case class Shift[R, +A](body: (A => Any) => Any) derives Effect

trait ShiftApi:
  /** capture to the nearest `reset` of answer `R`; `k` re-installs it, the body runs outside it */
  def shift[R, A, F[+_]](f: (A => R ! F) => R ! F): A ! Shift % R + F
  /** delimit, and handle every capture of answer `R` */
  def reset[R, F[+_]](body: R ! Shift % R + F)(using Distinct[Shift % R + F]): R ! F

object ShiftFx:

  /**
   * THE ONE CLAIM: a body is stored at an erased row and read back at the row
   * of the `reset` that handles it. The row is that `reset`'s by the typing:
   * a capture of answer `R` reaches only a `reset` of `R` (Distinct refuses a
   * second one in the row), and its `k` and body are typed in that row.
   */
  private[okay] def body[R, A, F[+_]](f: (A => R ! F) => R ! F): (A => Any) => Any =
    f.asInstanceOf[(A => Any) => Any]
  private[okay] def call[R, A, F[+_]](s: Shift[R, A], k: A => R ! F): R ! F =
    s.body(k).asInstanceOf[R ! F]

  /** (a) an ordinary effect; `reset` a deep handler over `Effects[Free].handle` */
  object Handled extends ShiftApi:
    def shift[R, A, F[+_]](f: (A => R ! F) => R ! F): A ! Shift % R + F =
      effect[Shift % R + F, A](Shift[R, A](body(f)))
    def reset[R, F[+_]](body: R ! Shift % R + F)(using Distinct[Shift % R + F]): R ! F =
      Effects[Free].handle[Shift % R, F](body)(pure(_)):
        [X] => s => okay.shift[X, R ! F, R ! F](k => call(s, k))

  /** (b) on Delim's machine: one shared prompt, the innermost installed one answers */
  object OnDelim extends ShiftApi:
    private val P: Prompt[Any] = new Prompt[Any]("Shift", "ShiftFx.scala")
    private def p[R]: Prompt[R] = P.asInstanceOf[Prompt[R]]
    // the same claim as `body`/`call`, at Delim's row: a `Shift % R` program is a
    // `Delim` program at the same erasure (only the machine reads `Cont0`)
    private def in[A, R, F[+_]](q: A ! Shift % R + F): A ! Delim + F = q.asInstanceOf[A ! Delim + F]
    private def out[A, R, F[+_]](q: A ! Delim + F): A ! Shift % R + F = q.asInstanceOf[A ! Shift % R + F]
    private def clause[R, A, F[+_]](f: (A => R ! F) => R ! F): (A => R ! Delim + F) => R ! Delim + F =
      f.asInstanceOf[(A => R ! Delim + F) => R ! Delim + F]

    def shift[R, A, F[+_]](f: (A => R ! F) => R ! F): A ! Shift % R + F =
      out[A, R, F](Delim.shift0[R, A, F](p[R])(clause(f)))
    def reset[R, F[+_]](body: R ! Shift % R + F)(using Distinct[Shift % R + F]): R ! F =
      Delim.run[R, F](Delim.push[R, F](p[R])(in(body)))

  /** level 2: the same program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F[+_]](api: ShiftApi)(q: A ! Shift % R + F): Cont[A, R ! F, R ! F] =
    okay.shift[A, R ! F, R ! F](k => api.reset[R, F](q.flatMap(a => k(a).plus[Shift % R])))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F[+_]](api: ShiftApi)(c: Cont[A, R ! F, R ! F]): A ! Shift % R + F =
    api.shift[R, A, F](k => c / k)
