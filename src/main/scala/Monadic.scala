package okay

/**
 * Monadic reflection (Filinski, "Representing Monads", POPL 1994):
 * with delimited control, ANY monad runs in direct style — `reflect`
 * delivers the A of an F[A] as a plain value, `reify` delimits a
 * block back into F. Answer-type modification types the construction
 * precisely: a reflected F[A] is Cont[A, F[B], F[B]] — "A now, F[B]
 * eventually" — and multi-shot comes for free, because the captured
 * continuation is a pure closure that F's own flatMap may call once
 * (Option), many times (List, Logic), or not at all (None is an
 * abort).
 *
 * The names are Filinski's, and they live in an object because the
 * package-level `reflect`/`reify` (Effects.scala) already name the
 * encoding round-trip — a different construction that happens to
 * deserve the same words.
 */
object Monadic:

  extension [F[_] : Monad, A](m: F[A])
    /** μ: the monadic value as a direct value — one definition, both
     * spellings: `m.reflect` and `reflect(m)` (an extension is a
     * method; the prefix form is its desugared call) */
    inline def reflect[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))
    /** the symbolic μ: `m.?` — Rust's postfix question, generalized
     * from Result to any monad (`!` is taken by the program type) */
    inline def ?[B]: Cont[A, F[B], F[B]] =
      shift(k => m.flatMap(k))

  /** the delimiter: a direct-style block back into its monad */
  inline def reify[F[_], A, B](p: Cont[A, F[A], F[B]])(using M: Monad[F]): F[B] =
    p / (a => M.pure(a))
