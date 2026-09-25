package okay2

/**
 * LAYERED MONADIC REFLECTION for the Scala 2 core — the twin of the
 * Scala 3 core's `Layered` (specs/layered-reflection.md, okay2-layered).
 *
 * Filinski's construction for one monad (POPL 1994) is `reflect =
 * shift(k => m.flatMap(k))` under `reify`; "Representing layered
 * monads" (POPL 1999) gives each of several layers its own pair.
 * Brachthäuser, Boruch-Gruszecki & Odersky (2020) observe that native
 * multi-prompt control is enough: each `reify` installs its own
 * delimiter and hands out a capability, and a `reflect` through it
 * captures up to the right delimiter, past any inner ones. `Delim` is
 * multi-prompt, so a layer is a prompt and `Reflect` names it.
 *
 * `Layer[M]`, not a monad: the captured continuation is a PROGRAM that
 * holds the inner layers' frames and may reflect into outer ones, so a
 * layer's bind sequences programs — a transformer over what runs
 * outside it. Option, Either and List are traversals in order.
 *
 * `reify` is `η $ e` (λ$'s reading, the unit as the delimiter's return
 * function) and `reflect` is a `shift0`, as in the Scala 3 core. Scala 2
 * has no context functions, so a body RECEIVES its capability
 * (`reify[Option, Int, P] { opt => Option(2).reflect(opt) }`) where the
 * Scala 3 body summons it. The stacked layers are `Layered.Stacked`.
 */
object Layered {

  /** a monad as a transformer over the programs that run outside it:
   * `bind` sequences the continuation's PROGRAMS */
  trait Layer[M[_]] {
    def pure[A](a: A): M[A]
    def bind[A, B, G <: Row](m: M[A])(k: A => M[B] ! G): M[B] ! G
  }

  object Layer {
    implicit val option: Layer[Option] = new Layer[Option] {
      def pure[A](a: A): Option[A] = Some(a)
      def bind[A, B, G <: Row](m: Option[A])(k: A => Option[B] ! G): Option[B] ! G = m match {
        case Some(a) => k(a)
        case None => okay2.pure[G, Option[B]](None)
      }
    }

    implicit def either[E]: Layer[({ type L[A] = Either[E, A] })#L] = new Layer[({ type L[A] = Either[E, A] })#L] {
      def pure[A](a: A): Either[E, A] = Right(a)
      def bind[A, B, G <: Row](m: Either[E, A])(k: A => Either[E, B] ! G): Either[E, B] ! G = m match {
        case Right(a) => k(a)
        case Left(e) => okay2.pure[G, Either[E, B]](Left(e))
      }
    }

    /** every element's continuation, left to right, results
     * concatenated: the continuation runs once per element */
    implicit val list: Layer[List] = new Layer[List] {
      def pure[A](a: A): List[A] = List(a)
      def bind[A, B, G <: Row](m: List[A])(k: A => List[B] ! G): List[B] ! G =
        m.foldRight(okay2.pure[G, List[B]](List.empty[B]))((a, rest) => k(a).flatMap(bs => rest.map(bs ++ _)))
    }
  }

  /** the capability a `reify` hands its body: which delimiter this
   * layer is, and its bind. The constructor is private, so holding one
   * means being inside the `reify` that made it (or having let it
   * escape, which fails with `NoPrompt` when used) */
  final class Reflect[M[_], R] private[Layered] (val prompt: Prompt[M[R]], val layer: Layer[M])

  /**
   * A layer: install a delimiter answering `M[R]`, run the body with the
   * capability, and wrap its value in `M`. INSTALLS ONLY, so layers
   * nest; the machine is run by whoever runs `Delim` outside the
   * outermost one (`Delim.run`).
   */
  def reify[M[_], R, F <: Row](body: Reflect[M, R] => R ! (Delim + F))(implicit L: Layer[M], at: At): M[R] ! (Delim + F) = {
    val p = Delim.prompt[M[R]]
    Delim.dollar[R, M[R], F](p)(r => okay2.pure[Delim + F, M[R]](L.pure(r)))(body(new Reflect[M, R](p, L)))
  }

  /** μ: the layer's value as a plain value, for the rest of the block up
   * to that layer's `reify`. `M` is read off the RECEIVER, so
   * `Some(2).reflect(opt)` looks for a `Reflect[Some, R]` and finds none:
   * write `Option(2)`, or ascribe. */
  implicit class ReflectOps[M[_], X](private val m: M[X]) extends AnyVal {
    def reflect[R, F <: Row](r: Reflect[M, R])(implicit at: At): X ! (Delim + F) =
      Delim.shift0[M[R], X, F](r.prompt)(k => r.layer.bind[X, R, Delim + F](m)(k))

    /** μ, stacked: `m.reflectAt[F](st, layer)`, a `shift0` to the layer,
     * which must be on the stack in force (`st`) — a layer kept past its
     * `reify` does not compile. `F` (the rest of the row) is the one type
     * argument the call cannot infer, so it is the one written. */
    def reflectAt[F <: Row]: ReflectAt[M, X, F] = new ReflectAt[M, X, F](m)
  }

  final class ReflectAt[M[_], X, F <: Row] private[Layered] (m: M[X]) {
    def apply[R, S2 <: Delim.Stacked.Stk, S <: Delim.Stacked.Stk](st: Delim.Stacked.Stack[S2], layer: Delim.Stacked.In[M[R], S])
             (implicit ev: Delim.Stacked.Has[S2, layer.p.type], L: Layer[M], at: At): X ! (Delim + F) =
      st.shift0[M[R], X, F](layer.p)(ev, at).apply(_ => k => L.bind[X, R, Delim + F](m)(k))
  }

  /**
   * THE STACKED LAYERS: a layer is a stacked `dollar` whose return
   * function is the monad's unit, and its capability is the `In` that
   * dollar hands its body; `m.reflectAt[F](st, layer)` asks that the
   * layer is on the stack in force, so a capability kept past its
   * `reify` is a compile error where the unstacked door throws
   * `NoPrompt`. `reify[M, R, F](st)(body)`: the three written type
   * arguments are the ones the call cannot infer; the stack's comes
   * from `st`.
   */
  object Stacked {
    import Delim.Stacked.{In, Stack, Stk}

    final class Reify[M[_], R, F <: Row] private[Layered] () {
      def apply[S <: Stk](st: Stack[S])(body: In[M[R], S] => R ! (Delim + F))(implicit L: Layer[M], at: At): M[R] ! (Delim + F) =
        st.dollar[R, M[R], F](r => okay2.pure[Delim + F, M[R]](L.pure(r)))(body)
    }
    def reify[M[_], R, F <: Row]: Reify[M, R, F] = new Reify[M, R, F]()
  }
}
