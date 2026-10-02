package okay.kyo

import _root_.kyo.{<, Abort, Flat}
import _root_.kyo.kernel.internal.WeakFlat

/**
 * okay's class ladder over kyo's `A < S` (specs/interop-classes.md).
 * kyo has no type classes — its combinators are functions over `<` —
 * so this direction is the whole of it.
 *
 * `WeakFlat` IS BYPASSED, and that is the one caveat. `A < S` is
 * `A | Kyo[A, S]`: a value that is itself a kyo computation is not a new
 * layer under `pure`, it IS the computation. kyo refuses such an `A` at
 * concrete call sites with `WeakFlat`/`Flat`; a generic instance cannot
 * ask for that evidence, so the laws hold for every `A` that is not a
 * `<` — the same contract kyo's own generic code lives under.
 */

/**
 * `A < S` as a one-hole type constructor, for the call site.
 *
 * kyo puts the VALUE first, `<[+A, -S]`, and Scala's inference of an
 * `F[_]` fills the LAST parameter: `okay.traverse(xs)(i => Env.use(...))`
 * infers `F = [S] =>> Int < S` and finds no instance. Naming the hole
 * says which one varies: `okay.traverse[Pending[Env[Int]], Int, Int]`.
 * (cats meets kyo the same way.)
 */
type Pending[S] = [A] =>> A < S

/** `A < S` under okay's Monad, for every effect set `S` */
given kyoMonad[S]: okay.Monad[[A] =>> A < S] with
  import WeakFlat.unsafe.bypass
  def pure[A](a: A): A < S = a
  override def fmap[A, B](a: A < S, f: A => B): B < S = _root_.kyo.kernel.`<`.map(a)(x => f(x))
  extension [A](a: A < S)
    // through the companion, not `a.flatMap`: inside this extension
    // the name would resolve to the extension being defined
    def flatMap[B](f: A => B < S): B < S = _root_.kyo.kernel.`<`.flatMap(a)(x => f(x))

/**
 * kyo's own `Loop` as okay's `TailRecM` (specs/eager-carrier-depth.md).
 * NOT `TailRecM.deferring`: a pure kyo value maps at once — `<`'s
 * `flatMap` calls its continuation before returning — so the `flatMap`
 * recursion would nest exactly like `Option`'s.
 */
given kyoTailRecM[S]: okay.TailRecM[[A] =>> A < S] with
  // E221 is kyo's, not ours: `Loop.apply` is inline and its own @tailrec
  // `loop(i1)(v = run(i1))` recurses with that default argument; the
  // warning surfaces at whatever call site inlines it
  @annotation.nowarn("id=E221")
  def tailRecM[A, B](a: A)(f: A => Either[A, B] < S): B < S =
    import WeakFlat.unsafe.bypass
    _root_.kyo.Loop[A, B, S](a)(s => _root_.kyo.kernel.`<`.map(f(s)) {
      case Left(next) => _root_.kyo.Loop.continue[A, B, S](next)
      case Right(b) => _root_.kyo.Loop.done[A, B](b)
    })

object KyoClasses:

  /**
   * `app` by `Async.parallel`: both leaves forked, the pair failing on
   * either. NOT a given, for the reason ZioClasses.parApplicative is
   * not: the choice of the parallel reading belongs at the call site.
   */
  def parApplicative[E]: okay.Applicative[[A] =>> A < (Abort[E] & _root_.kyo.Async)] = new:
    import WeakFlat.unsafe.bypass
    def pure[A](a: A): A < (Abort[E] & _root_.kyo.Async) = a
    override def fmap[A, B](a: A < (Abort[E] & _root_.kyo.Async), f: A => B): B < (Abort[E] & _root_.kyo.Async) =
      _root_.kyo.kernel.`<`.map(a)(x => f(x))
    extension [A, B](f: (A => B) < (Abort[E] & _root_.kyo.Async))
      def app(a: A < (Abort[E] & _root_.kyo.Async)): B < (Abort[E] & _root_.kyo.Async) =
        // the same bypass as `pure`'s, for kyo's stricter `Flat`
        given Flat[A] = Flat.unsafe.bypass
        given Flat[A => B] = Flat.unsafe.bypass
        _root_.kyo.kernel.`<`.map(_root_.kyo.Async.parallel[E, A => B, A, Any](f, a))((g, x) => g(x))
