package okay

/**
 * LAYERED MONADIC REFLECTION: several monads in one direct block, each
 * reflected value reaching its OWN reify (specs/layered-reflection.md).
 *
 * Filinski's construction for one monad (`Monadic`, POPL 1994) is
 * `reflect = shift(k => m.flatMap(k))` under `reify`. "Representing
 * layered monads" (POPL 1999) gives each of several layers its own
 * reflect/reify. Brachthäuser, Boruch-Gruszecki & Odersky
 * ("Representing monads with capabilities", 2020) observe that NATIVE
 * MULTI-PROMPT control is enough: each `reify` installs its own
 * delimiter and hands out a capability, and a `reflect` through that
 * capability captures up to the right delimiter, past any inner ones.
 * `Delim` is multi-prompt, so a layer is a prompt and `Reflect` is the
 * capability that names it.
 *
 * WHY `Layer[M]` AND NOT `Monad[M]`. Here the captured continuation
 * is a PROGRAM (`X => M[R] ! G`): it holds the inner layers' frames and
 * may reflect into outer layers, which only the running machine can
 * answer. So a layer's bind has to sequence programs, which is a monad
 * TRANSFORMER over whatever runs outside the layer. `Monad.flatMap`
 * does not, and the fibre road in the capabilities paper can use it
 * only because its continuation is an impure function. For Option,
 * Either and List the transformer is a traversal in order, and those
 * are the instances below.
 *
 * THE ORDER OF THE `reify` BLOCKS IS THE ORDER OF THE LAYERS, the way
 * the order of handlers is: `reify[Option](reify[List](b))` answers
 * `Option[List[R]]` and a `None` anywhere empties it all, and
 * `reify[List](reify[Option](b))` answers `List[Option[R]]` with a
 * `None` per branch that failed (TestLayered pins both).
 *
 * `reflect` is a `shift0`: the bind runs outside the layer's own
 * delimiter, and `k` re-installs it. That is λ$'s reading,
 * `μ(m) = S0 k. m >>= k` with `[e] = η $ e` (Materzok & Biernacki,
 * APLAS 2012; specs/shift0-dollar.md).
 */
object Layered:

  /** a monad as a transformer over the programs that run outside it:
   * `bind` sequences the continuation's PROGRAMS */
  trait Layer[M[_]]:
    def pure[A](a: A): M[A]
    def bind[A, B, G[+_]](m: M[A])(k: A => M[B] ! G): M[B] ! G

  object Layer:
    given option: Layer[Option] with
      def pure[A](a: A): Option[A] = Some(a)
      def bind[A, B, G[+_]](m: Option[A])(k: A => Option[B] ! G): Option[B] ! G = m match
        case Some(a) => k(a)
        case None => okay.pure(None)

    given either[E]: Layer[[A] =>> Either[E, A]] with
      def pure[A](a: A): Either[E, A] = Right(a)
      def bind[A, B, G[+_]](m: Either[E, A])(k: A => Either[E, B] ! G): Either[E, B] ! G = m match
        case Right(a) => k(a)
        case Left(e) => okay.pure(Left(e))

    /** every element's continuation, left to right, results
     * concatenated: the continuation runs once per element */
    given list: Layer[List] with
      def pure[A](a: A): List[A] = List(a)
      def bind[A, B, G[+_]](m: List[A])(k: A => List[B] ! G): List[B] ! G =
        m.foldRight(okay.pure(List.empty[B]): List[B] ! G)((a, rest) =>
          k(a).flatMap(bs => rest.map(bs ++ _)))

  /** the capability a `reify` hands its body: which delimiter this
   * layer is, and its bind. The constructor is private, so holding one
   * means being inside the `reify` that made it (or having let it
   * escape, which fails with `NoPrompt` when used) */
  final class Reflect[M[_], R] private[Layered] (val prompt: Prompt[M[R]], val layer: Layer[M])

  /**
   * A layer: install a delimiter answering `M[R]`, run the body with the
   * capability in scope, and wrap its value in `M`. INSTALLS ONLY, so
   * layers nest. The machine is run by whoever runs `Delim` outside the
   * outermost one (`Delim.run`).
   */
  def reify[M[_], R, F[+_]](body: Reflect[M, R] ?=> R ! Delim + F)(using L: Layer[M], at: At): M[R] ! Delim + F =
    val p = Delim.prompt[M[R]]
    Delim.push(p)(body(using new Reflect[M, R](p, L)).map(L.pure))

  extension [M[_], X](m: M[X])
    /** μ: the layer's value as a plain value, for the rest of the block
     * up to that layer's `reify`. `M` is read off the RECEIVER, so
     * `Some(2).reflect` looks for a `Reflect[Some, R]` and finds none:
     * write `Option(2)`, or ascribe (the same trap as `.some` in cats). */
    def reflect[R, F[+_]](using r: Reflect[M, R], at: At): X ! Delim + F =
      Delim.shift0[M[R], X, F](r.prompt)(k => r.layer.bind(m)(k))
