package okay.freer

import okay.*

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
        case None => okay.freer.pure(None)

    given either[E]: Layer[[A] =>> Either[E, A]] with
      def pure[A](a: A): Either[E, A] = Right(a)
      def bind[A, B, G[+_]](m: Either[E, A])(k: A => Either[E, B] ! G): Either[E, B] ! G = m match
        case Right(a) => k(a)
        case Left(e) => okay.freer.pure(Left(e))

    /** every element's continuation, results concatenated */
    given list: Layer[List] with
      def pure[A](a: A): List[A] = List(a)
      def bind[A, B, G[+_]](m: List[A])(k: A => List[B] ! G): List[B] ! G =
        m.foldRight(okay.freer.pure(List.empty[B]): List[B] ! G)((a, rest) =>
          k(a).flatMap(bs => rest.map(bs ++ _)))

  /** a layer's capability: its prompt and its monad */
  final class Reflect[M[_], R] private[Layered] (val prompt: Prompt[M[R]], val layer: Layer[M])

  /** a layer: `pure $ body` at a fresh prompt */
  def reify[M[_], R, F[+_]](body: Reflect[M, R] ?=> R ! Shift % ? + F)(using L: Layer[M], at: At): M[R] ! Shift % ? + F =
    val p = Shift.prompt[M[R]]
    Shift.dollar[R, M[R], F](p)(r => okay.freer.pure(L.pure(r)))(body(using new Reflect[M, R](p, L)))

  extension [M[_], X](m: M[X])
    /** reflect into the layer whose capability is in scope */
    def reflect[R, F[+_]](using r: Reflect[M, R], at: At): X ! Shift % ? + F =
      Shift.shift0[M[R], X, F](r.prompt)(k => r.layer.bind(m)(k))

  /** the same with the layer KEYED in the row (`Shift % l.type`, shift-prompt-key): a capability kept past its
   * `reify` leaves its key in a row nothing handles, and does not compile where it is run */
  object Stacked:
    import Shift.Stacked.Reset

    /** a keyed layer: the body sees its delimiter, and its reflections carry its key */
    def reify[M[_], R, F[+_]](body: (l: Reset[M[R], F]) => R ! Shift % l.type + F)
                             (using L: Layer[M], m: Shift.Machine[F], at: At): M[R] ! F =
      Shift.Stacked.dollar[R, M[R], F]((r: R) => okay.freer.pure(L.pure(r)))(body)

    extension [M[_], X](m: M[X])
      /** reflect into the layer `layer`: the continuation is bound inside the layer, at the row outside it */
      def reflect[R, F[+_]](layer: Reset[M[R], F])(using Layer[M], At): X ! Shift % layer.type + F =
        Shift.Stacked.shift0(layer)[X](k => summon[Layer[M]].bind(m)(k))

