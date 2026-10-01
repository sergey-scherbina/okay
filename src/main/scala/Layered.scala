package okay

/**
 * LAYERED MONADIC REFLECTION (Filinski POPL 1999; Brachthäuser et al. 2020): each `reify` installs its
 * own prompt, each `reflect` captures to its layer's, so several monads share one direct block.
 */
object Layered:

  /** a monad as a layer: `bind` sequences the continuation's programs */
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

    /** every element's continuation, results concatenated */
    given list: Layer[List] with
      def pure[A](a: A): List[A] = List(a)
      def bind[A, B, G[+_]](m: List[A])(k: A => List[B] ! G): List[B] ! G =
        m.foldRight(okay.pure(List.empty[B]): List[B] ! G)((a, rest) =>
          k(a).flatMap(bs => rest.map(bs ++ _)))

  /** a layer's capability: its prompt and its monad */
  final class Reflect[M[_], R] private[Layered] (val prompt: Prompt[M[R]], val layer: Layer[M])

  /** a layer: `pure $ body` at a fresh prompt */
  def reify[M[_], R, F[+_]](body: Reflect[M, R] ?=> R ! Delim + F)(using L: Layer[M], at: At): M[R] ! Delim + F =
    val p = Delim.prompt[M[R]]
    Delim.dollar[R, M[R], F](p)(r => okay.pure(L.pure(r)))(body(using new Reflect[M, R](p, L)))

  extension [M[_], X](m: M[X])
    /** reflect into the layer whose capability is in scope */
    def reflect[R, F[+_]](using r: Reflect[M, R], at: At): X ! Delim + F =
      Delim.shift0[M[R], X, F](r.prompt)(k => r.layer.bind(m)(k))

  /** the same over the typed prompt stack: a capability kept past its `reify` does not compile */
  object Stacked:
    import Delim.Stacked.{In, Stack, Under, Has}

    /** a stacked layer */
    def reify[M[_], R, F[+_]](using st: Stack[?])
                             (body: (l: In[M[R], st.S]) => Under[F, R, l.p.type *: st.S])
                             (using L: Layer[M], at: At): Under[F, M[R], st.S] =
      Delim.Stacked.dollar[R, M[R], F]((r: R) => Freer.Return(L.pure(r)))(body)

    extension [M[_], X](m: M[X])
      /** reflect, the layer's prompt proven on the stack */
      def reflect[R, F[+_]](layer: In[M[R], ?])(using st: Stack[?])[B <: Tuple]
                           (using Has.Aux[st.S, layer.p.type, B], Layer[M], At): Under[F, X, st.S] =
        Delim.Stacked.shift0[M[R], X, F](layer.p)(k =>
          Delim.Stacked.at[F, M[R], B](summon[Layer[M]].bind(m)(x => Delim.Stacked.erase(k(x)))))

