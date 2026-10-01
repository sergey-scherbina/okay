package okay

import okay.Freer.{Return, Inject, Bind}

/**
 * THE CONTINUATION MACHINE BEHIND AN INTERFACE (specs/delimited.md).
 *
 * Dybvig, Peyton Jones and Sabry's `MonadDelimitedCont` ("A monadic
 * framework for delimited continuations", JFP 2007) in our variant: a
 * few primitives over a carrier `M[S, R, A]` — the freer tree's
 * indexes, a program from answer `R` to `S` producing `A` — every
 * control operator derived over them, and the frame machine ONE
 * instance of the trait (`Delimited.machine`).
 *
 *   our primitive        DPJS           what it is
 *   `delimiter`          `newPrompt`    a fresh name (identity is allocation: no program)
 *   `dollar`             `pushPrompt`   `ret $ body` (λ$); `pushPrompt` is `pure $`
 *   `shift0`             `withSubCont`  capture to the delimiter: `k` WITH it and its `ret`
 *   `resume`             `pushSubCont`  run a COMPUTATION inside `k`; `k(a)` is `resume(k)(pure(a))`
 *
 * OUR VARIANT, decided (specs/delimited.md): DPJS's capture leaves the
 * prompt out of `k` (it is `control0`) and derive `shift0` by pushing
 * it back; with `$` the delimiter carries `ret`, which that derivation
 * would have to hand back beside `k`. Ours keeps it in `k`, λ$'s
 * `($/S0)` rule — the machine as it is.
 */
trait Delimited[M[_, _, _]]:

  /** a delimiter's name: its answer `Y`, and the index `I` it is installed at */
  type Delimiter[Y, I]

  /** a captured stack: from `A` at `T` through the delimiter to its answer
   * `Z` at `S` — and a function, applied as one: `k(a)` resumes */
  type SubCont[A, S, T, Z] <: A => M[S, T, Z]

  /** a fresh delimiter, labelled with the line that asked for it */
  def delimiter[Y, I](using At): Delimiter[Y, I]

  /** a value */
  def pure[R, A](a: A): M[R, R, A]

  /** `ret $ body`: the body under the delimiter, `ret` run OUTSIDE it on
   * the body's value, and carried by a capture to it */
  def dollar[Y, A, T, R](d: Delimiter[Y, T])(ret: A => M[T, T, Y])(body: M[T, R, A]): M[T, R, Y]

  /** capture to `d`: `f` gets the stack up to it WITH it (and its `ret`),
   * and its answer stands in the delimiter's place, at its index */
  def shift0[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X]

  /** the computation `m` run inside `k`: its value fed to `k` */
  def resume[A, S, T, R, Z](k: SubCont[A, S, T, Z])(m: M[T, R, A]): M[S, R, Z]

  // ---- derived, over any instance

  /** `⟨body⟩`: `pure $ body` — λ$'s own definition of the plain delimiter */
  def reset[T, R, A](d: Delimiter[A, T])(body: M[T, R, A]): M[T, R, A] =
    dollar[A, A, T, R](d)(a => pure[T, A](a))(body)

  /** `shift`: `shift0` whose body runs under a fresh plain delimiter of
   * the same name — APLAS 2012's `S k.e = S0 k.⟨e⟩` */
  def shift[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X] =
    shift0[Y, I, T, R, X](d)(k => reset[I, R, Y](d)(f(k)))

  /** leave the delimiter with a value: a `shift0` that drops `k` */
  def abort[Y, T, X](d: Delimiter[Y, T])(value: Y)(using At): M[T, T, X] =
    shift0[Y, T, T, T, X](d)(_ => pure[T, Y](value))

object Delimited:

  /**
   * THE FRAME MACHINE as an instance: its programs are the freer tree
   * over `Cont0` beside `F`, its names `Cont0.Delimiter`, its captured
   * stacks `Stack`. The primitives are the operations `Frames.run`
   * already interprets, and `resume(k)(m)` is `Bind(m, k)` — a bind
   * whose continuation is a `Stack` is the machine's resumption rule,
   * so resuming with a whole computation needs no operation of its own.
   */
  final class Machine[F[_, _, +_]] private[Delimited] () extends Delimited[[S, R, A] =>> Freer[Cont0.Row[F], S, R, A]]:
    type Delimiter[Y, I] = Cont0.Delimiter[Y, I]
    type SubCont[A, S, T, Z] = Stack[F, A, S, T, Z]

    def delimiter[Y, I](using at: At): Cont0.Delimiter[Y, I] = Cont0.delimiter(Cont0.prompt[Y])

    def pure[R, A](a: A): Freer[Cont0.Row[F], R, R, A] = Return(a)

    def dollar[Y, A, T, R](d: Cont0.Delimiter[Y, T])(ret: A => Freer[Cont0.Row[F], T, T, Y])
                          (body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, Y] =
      Inject[Cont0.Row[F], T, R, Y](Cont0.Dollar0[F, Y, A, T, R](d, ret, body))

    /** the plain delimiter with a `ret` that captures nothing — one object
     * per call site, where the trait's default closes over the instance */
    override def reset[T, R, A](d: Cont0.Delimiter[A, T])(body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, A] =
      Inject[Cont0.Row[F], T, R, A](Cont0.Dollar0[F, A, A, T, R](d, (a: A) => Return[Cont0.Row[F], T, A](a), body))

    def shift0[Y, I, T, R, X](d: Cont0.Delimiter[Y, I])(f: Stack[F, X, I, T, Y] => Freer[Cont0.Row[F], I, R, Y])
                             (using at: At): Freer[Cont0.Row[F], T, R, X] =
      Inject[Cont0.Row[F], T, R, X](Cont0.Shift0[F, Y, I, T, R, X](d, f, at.where))

    def resume[A, S, T, R, Z](k: Stack[F, A, S, T, Z])(m: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], S, R, Z] =
      Bind[Cont0.Row[F], S, T, R, A, Z](m, k)

  /** ONE MACHINE FOR EVERY `F`: it holds nothing, so the signature is
   * phantom and one object serves every one of them, as `Frames.noFrames`
   * serves every segment type — the cast is that sentence, and a call
   * site pays no allocation for the interface */
  private val theMachine: Machine[[S, R, X] =>> Nothing] = Machine()
  def machine[F[_, _, +_]]: Machine[F] = theMachine.asInstanceOf[Machine[F]]
