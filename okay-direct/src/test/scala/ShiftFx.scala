package okay

import okay.Row.*

/**
 * specs/shift-effect.md: `shift`/`reset` as an effect in `A ! F`. A
 * capture is `Shift % R` in the row; `reset` is its handler. It carries
 * its answer type's key, so captures of different answer types share a
 * row and each `reset` takes only its own. Two implementations behind
 * one API, so one suite runs on both.
 */
final case class Shift[R, +A](key: String, under: Boolean, body: (A => Any) => Any)

object Shift:
  /** the test reads the key, so `Shift % Int + Shift % String` is a good row */
  given typeableK[R](using k: Key[R]): TypeableK.ByValue[Shift % R] = new:
    def test(x: Any): Boolean = x match
      case s: Shift[?, ?] => s.key == k.id
      case _ => false

trait ShiftApi:
  /** Danvy-Filinski's: the body runs under the `reset`, so it may capture to it again; `k` re-installs it */
  def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using Key[R]): A ! Shift % R + F
  /** the body runs outside the `reset`; `k` re-installs it */
  def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using Key[R]): A ! Shift % R + F
  /** delimit, and handle every capture of answer `R` */
  def reset[R, F[+_]](body: R ! Shift % R + F)(using Key[R], Distinct[Shift % R + F], Nesting[F]): R ! F

object ShiftFx:

  /**
   * THE ONE CLAIM: a body is stored at an erased row and read back at the row
   * of the `reset` that handles it. The row is that `reset`'s by the typing:
   * a capture of answer `R` reaches only the `reset` with `R`'s key, and its
   * `k` and body are typed in that row.
   */
  private def erase[A, X, Y](f: (A => X) => Y): (A => Any) => Any = f.asInstanceOf[(A => Any) => Any]
  private def call[R, A, F[+_]](s: Shift[R, A], k: A => R ! F): R ! F =
    s.body(k).asInstanceOf[R ! F]
  private def callUnder[R, A, F[+_]](s: Shift[R, A], k: A => R ! Shift % R + F): R ! Shift % R + F =
    s.body(k).asInstanceOf[R ! Shift % R + F]

  /** (a) an ordinary effect; `reset` a deep handler over `Effects[Free].handle` */
  object Handled extends ShiftApi:
    def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Key[R]): A ! Shift % R + F =
      effect[Shift % R + F, A](Shift[R, A](k.id, true, erase(f)))
    def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Key[R]): A ! Shift % R + F =
      effect[Shift % R + F, A](Shift[R, A](k.id, false, erase(f)))
    def reset[R, F[+_]](body: R ! Shift % R + F)(using key: Key[R], d: Distinct[Shift % R + F], n: Nesting[F]): R ! F =
      Effects[Free].handle[Shift % R, F](body)(pure(_)):
        [X] => s =>
          if s.under then okay.shift[X, R ! F, R ! F](k => reset[R, F](callUnder[R, X, F](s, x => k(x).plus[Shift % R])))
          else okay.shift[X, R ! F, R ! F](k => call(s, k))

  /** (b) on Delim's machine: a prompt per answer type, the innermost installed one answers */
  object OnDelim extends ShiftApi:
    private val prompts = scala.collection.concurrent.TrieMap.empty[String, Prompt[Any]]
    // one prompt per key, so the cast only re-states the key's type
    private def p[R](k: Key[R]): Prompt[R] =
      prompts.getOrElseUpdate(k.id, new Prompt[Any](s"Shift[${k.id}]", "ShiftFx.scala")).asInstanceOf[Prompt[R]]
    // the same claim as `call`, at Delim's row: a `Shift % R` program is a
    // `Delim` program at the same erasure (only the machine reads `Cont0`)
    private def in[A, R, F[+_]](q: A ! Shift % R + F): A ! Delim + F = q.asInstanceOf[A ! Delim + F]
    private def out[A, R, F[+_]](q: A ! Delim + F): A ! Shift % R + F = q.asInstanceOf[A ! Shift % R + F]
    private def inner[R, F[+_]](q: R ! Delim + F): R ! F = q.asInstanceOf[R ! F]
    private def clause[R, A, F[+_], G[+_]](f: (A => R ! G) => R ! G): (A => R ! Delim + F) => R ! Delim + F =
      f.asInstanceOf[(A => R ! Delim + F) => R ! Delim + F]

    def shift[R, A, F[+_]](f: (A => R ! Shift % R + F) => R ! Shift % R + F)(using k: Key[R]): A ! Shift % R + F =
      out[A, R, F](Delim.shift[R, A, F](p(k))(clause(f)))
    def shift0[R, A, F[+_]](f: (A => R ! F) => R ! F)(using k: Key[R]): A ! Shift % R + F =
      out[A, R, F](Delim.shift0[R, A, F](p(k))(clause(f)))
    def reset[R, F[+_]](body: R ! Shift % R + F)(using k: Key[R], d: Distinct[Shift % R + F], n: Nesting[F]): R ! F =
      val pushed = Delim.push[R, F](p(k))(in(body))
      // an inner reset's row still holds a Shift: the outer reset's machine runs it
      if n.inner then inner(pushed) else Delim.run[R, F](pushed)

  /** level 2: the same program as a `Cont` whose answers are programs: `c / k` is `reset(q >>= k)` */
  def cont[A, R, F[+_]](api: ShiftApi)(q: A ! Shift % R + F)(using Key[R], Distinct[Shift % R + F], Nesting[F]): Cont[A, R ! F, R ! F] =
    okay.shift[A, R ! F, R ! F](k => api.reset[R, F](q.flatMap(a => k(a).plus[Shift % R])))

  /** level 2: a whole `Cont` as one capture */
  def embed[A, R, F[+_]](api: ShiftApi)(c: Cont[A, R ! F, R ! F])(using Key[R]): A ! Shift % R + F =
    api.shift0[R, A, F](k => c / k)
