package okay.freer


import okay.{ParaMonad}


/**
 * THE λ$ INTERFACE, as a TEST ORACLE (specs/cont-atm.md): the λ$ machine it once described was deleted when Shift
 * and the handler frames moved onto `Delimited`; what stays is the interface — Dybvig, Peyton Jones & Sabry's
 * `MonadDelimitedCont` (JFP 2007) in λ$'s variant — so the laws (TestDelimited), the differential oracle
 * (TestDelimitedDifferential) and the depth programs (TestDelimitedDepth) run the reference (DelimitedReference)
 * against SHIFT ON THE MACHINE (`LambdaDollar.machine`). `M[S, R, A]` is a parameterised monad; the Shift instance
 * reads its indexes as phantom, as every `Free` program is at `Unit`.
 *
 * Primitives: `delimiter` (newPrompt), `dollar` (pushPrompt, with `ret`), `shift0` (withSubCont, but `k` keeps the
 * delimiter and `ret`), `resume` (pushSubCont: a computation inside `k`), `runHead` (a nested run, a door).
 * Derived: `reset`, `shift`, `abort`.
 */
trait LambdaDollar[M[_, _, _]] extends ParaMonad[[A, S, R] =>> M[S, R, A]]:

  /** a delimiter's name: answer `Y`, installed at index `I` */
  type Delimiter[Y, I]

  /** a captured stack, applied as a function */
  type SubCont[A, S, T, Z] <: A => M[S, T, Z]

  /** a fresh delimiter */
  def delimiter[Y, I](using At): Delimiter[Y, I]

  // a value: `pure[A, R](a): M[R, R, A]`, ParaMonad's own (the order Atkey writes, the value first)

  /** sequencing */
  def bind[A, B, S, T, R](m: M[T, R, A])(f: A => M[S, T, B]): M[S, R, B]

  /** ParaMonad's `flatMap` is `bind` */
  extension [A, S, R](m: M[S, R, A])
    def flatMap[B, S2](f: A => M[S2, S, B]): M[S2, R, B] = bind[A, B, S2, S, R](m)(f)

  /**
   * THE DOOR (delimited-one-door, the operator's "одна дверь в машину"): run `m` to its HEAD FORM — a value,
   * or the first operation the machine does not answer (an operation of the carrier's own signature, or a
   * capture to a delimiter `m` does not hold, going out), with the rest of `m` as its continuation. Every run
   * of the machine is this: `run` below, a nested run, a resumed strict `k`. Observationally the identity:
   * `runHead(m)` computes what `m` computes, in any context (the reference's own `runHead` is `m` itself,
   * and the differential oracle drives both through it).
   */
  def runHead[T, R, A](m: M[T, R, A]): M[T, R, A]

  /** the same door entered from a captured stack: `runHeadAt(k)(a)` is `runHead(k(a))` without building `k(a)`
   * (a resumption node per call: +92 KB and 1.15x on statePara through the strict-`k` bridge, measured) */
  def runHeadAt[A, S, T, Z](k: SubCont[A, S, T, Z])(a: A): M[S, T, Z]

  /** run to a value (`runCC`): `runHead` under a boundary; a capture without its delimiter is `NoPrompt` */
  def run[A](m: M[A, A, A]): A

  /** `ret $ body`: `ret` runs outside the delimiter and rides in `k` */
  def dollar[Y, A, T, R](d: Delimiter[Y, T])(ret: A => M[T, T, Y])(body: M[T, R, A]): M[T, R, Y]

  /** capture to `d`, `k` with it; the body takes its place */
  def shift0[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X]

  /** run `m` inside `k`; `k(a)` is `resume(k)(pure(a))` */
  def resume[A, S, T, R, Z](k: SubCont[A, S, T, Z])(m: M[T, R, A]): M[S, R, Z]


  /** `pure $ body` */
  def reset[T, R, A](d: Delimiter[A, T])(body: M[T, R, A]): M[T, R, A] =
    dollar[A, A, T, R](d)(a => pure[A, T](a))(body)

  /** `shift0` with the body under `reset` (S k.e = S0 k.<e>) */
  def shift[Y, I, T, R, X](d: Delimiter[Y, I])(f: SubCont[X, I, T, Y] => M[I, R, Y])(using At): M[T, R, X] =
    shift0[Y, I, T, R, X](d)(k => reset[I, R, Y](d)(f(k)))

  /** leave `d` with a value */
  def abort[Y, T, X](d: Delimiter[Y, T])(value: Y)(using At): M[T, T, X] =
    shift0[Y, T, T, T, X](d)(_ => pure[Y, T](value))

object LambdaDollar:

  /** a program of Shift's row, its indexes phantom */
  type OnShift[S, R, A] = A ! Shift % ? + Pure

  /** Shift on the machine through the interface */
  val machine: LambdaDollar[OnShift] = new LambdaDollar[OnShift]:
    type Delimiter[Y, I] = Prompt[Y]
    type SubCont[A, S, T, Z] = Shift.Resumption[A, Z, Pure]

    def delimiter[Y, I](using at: At): Prompt[Y] = Shift.prompt[Y]
    def pure[A, R](a: A): A ! Shift % ? + Pure = okay.freer.pure(a)
    def bind[A, B, S, T, R](m: A ! Shift % ? + Pure)(f: A => B ! Shift % ? + Pure): B ! Shift % ? + Pure = m.flatMap(f)

    /** a nested run of the row: stepped into by the machine running it, no barrier — a capture inside it reaches
     * the prompts around it */
    def runHead[T, R, A](m: A ! Shift % ? + Pure): A ! Shift % ? + Pure = Shift.runNested[A, Shift % ? + Pure](m)
    def runHeadAt[A, S, T, Z](k: Shift.Resumption[A, Z, Pure])(a: A): Z ! Shift % ? + Pure = runHead[S, T, Z](k(a))

    def run[A](m: A ! Shift % ? + Pure): A = !.run(Shift.run[A, Pure](m))

    def dollar[Y, A, T, R](d: Prompt[Y])(ret: A => Y ! Shift % ? + Pure)(body: A ! Shift % ? + Pure): Y ! Shift % ? + Pure =
      Shift.dollar[A, Y, Pure](d)(ret)(body)

    def shift0[Y, I, T, R, X](d: Prompt[Y])(f: Shift.Resumption[X, Y, Pure] => Y ! Shift % ? + Pure)
                             (using At): X ! Shift % ? + Pure =
      Shift.withSubCont[Y, X, Pure](d)(f)

    def resume[A, S, T, R, Z](k: Shift.Resumption[A, Z, Pure])(m: A ! Shift % ? + Pure): Z ! Shift % ? + Pure = k.resumeWith(m)
