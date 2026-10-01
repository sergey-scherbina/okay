package okay

import okay.Freer.{Return, Inject, Bind}

/**
 * THE MACHINE'S INTERFACE: Dybvig, Peyton Jones & Sabry's `MonadDelimitedCont` (JFP 2007) in λ$'s variant.
 * Primitives: `delimiter` (newPrompt), `dollar` (pushPrompt, with `ret`), `shift0` (withSubCont, but `k`
 * keeps the delimiter and `ret`), `resume` (pushSubCont: a computation inside `k`). Derived: `reset`,
 * `shift`, `abort`. Instances: `Delimited.machine` and the tests' reference. `Control` is the one-prompt
 * user level, built on this.
 */
trait Delimited[M[_, _, _]]:

  /** a delimiter's name: answer `Y`, installed at index `I` */
  type Delimiter[Y, I]

  /** a captured stack, applied as a function */
  type SubCont[A, S, T, Z] <: A => M[S, T, Z]

  /** a fresh delimiter */
  def delimiter[Y, I](using At): Delimiter[Y, I]

  /** a value */
  def pure[R, A](a: A): M[R, R, A]

  /** sequencing */
  def bind[A, B, S, T, R](m: M[T, R, A])(f: A => M[S, T, B]): M[S, R, B]

  /** run to a value (`runCC`); a capture without its delimiter is `NoPrompt` */
  def run[A](m: M[A, A, A]): A

  /** `ret $ body`: `ret` runs outside the delimiter and rides in `k` */
  def dollar[Y, A, T, R](d: Delimiter[Y, T])(ret: A => M[T, T, Y])(body: M[T, R, A]): M[T, R, Y]

  /** capture to `d`, `k` with it; the body takes its place */
  def shift0[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X]

  /** run `m` inside `k`; `k(a)` is `resume(k)(pure(a))` */
  def resume[A, S, T, R, Z](k: SubCont[A, S, T, Z])(m: M[T, R, A]): M[S, R, Z]


  /** `pure $ body` */
  def reset[T, R, A](d: Delimiter[A, T])(body: M[T, R, A]): M[T, R, A] =
    dollar[A, A, T, R](d)(a => pure[T, A](a))(body)

  /** `shift0` with the body under `reset` (S k.e = S0 k.<e>) */
  def shift[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X] =
    shift0[Y, I, T, R, X](d)(k => reset[I, R, Y](d)(f(k)))

  /** leave `d` with a value */
  def abort[Y, T, X](d: Delimiter[Y, T])(value: Y)(using At): M[T, T, X] =
    shift0[Y, T, T, T, X](d)(_ => pure[T, Y](value))

object Delimited:

  /** the frame machine: `dollar`/`shift0` are its operations, `resume(k)(m)` is `Bind(m, k)` */
  final class Machine[F[_, _, +_]] private[Delimited] () extends Delimited[[S, R, A] =>> Freer[Cont0.Row[F], S, R, A]]:
    type Delimiter[Y, I] = Cont0.Delimiter[Y, I]
    type SubCont[A, S, T, Z] = Stack[F, A, S, T, Z]

    def delimiter[Y, I](using at: At): Cont0.Delimiter[Y, I] = Cont0.delimiter(Cont0.prompt[Y])

    def pure[R, A](a: A): Freer[Cont0.Row[F], R, R, A] = Return(a)

    def bind[A, B, S, T, R](m: Freer[Cont0.Row[F], T, R, A])(f: A => Freer[Cont0.Row[F], S, T, B]): Freer[Cont0.Row[F], S, R, B] =
      Bind(m, f)

    /** under the barrier; any head form but a value is an unhandled operation of `F` */
    def run[A](m: Freer[Cont0.Row[F], A, A, A]): A =
      Frames.run[F, A, A, A](reset[A, A, A](Cont0.boundary[A, A])(m)) match
        case Return(a) => a
        case _ => throw IllegalStateException("Delimited.machine.run: an operation of F was left unhandled")

    def dollar[Y, A, T, R](d: Cont0.Delimiter[Y, T])(ret: A => Freer[Cont0.Row[F], T, T, Y])
                          (body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, Y] =
      Inject[Cont0.Row[F], T, R, Y](Cont0.Dollar0[F, Y, A, T, R](d, ret, body))

    /** a `ret` that captures nothing: one object per call site */
    override def reset[T, R, A](d: Cont0.Delimiter[A, T])(body: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], T, R, A] =
      Inject[Cont0.Row[F], T, R, A](Cont0.Dollar0[F, A, A, T, R](d, (a: A) => Return[Cont0.Row[F], T, A](a), body))

    def shift0[Y, I, T, R, X](d: Cont0.Delimiter[Y, I])(f: Stack[F, X, I, T, Y] => Freer[Cont0.Row[F], I, R, Y])
                             (using at: At): Freer[Cont0.Row[F], T, R, X] =
      Inject[Cont0.Row[F], T, R, X](Cont0.Shift0[F, Y, I, T, R, X](d, f, at.where))

    def resume[A, S, T, R, Z](k: Stack[F, A, S, T, Z])(m: Freer[Cont0.Row[F], T, R, A]): Freer[Cont0.Row[F], S, R, Z] =
      Bind[Cont0.Row[F], S, T, R, A, Z](m, k)

  /** one stateless machine for every `F` (phantom signature) */
  private val theMachine: Machine[[S, R, X] =>> Nothing] = Machine()
  def machine[F[_, _, +_]]: Machine[F] = theMachine.asInstanceOf[Machine[F]]
