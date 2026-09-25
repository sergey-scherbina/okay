package okay

/**
 * HANDLER INSTANCES AS PROMPTS (specs/lexical-instances.md).
 *
 * A row routes an operation by its CLASS, so a row holds one handler
 * per signature and a handler cannot tell whose operation it was
 * forwarded. Here a handler INSTALLATION is a fresh prompt, and the
 * body reaches it through the instance value the installation hands
 * it: Biernacki, Piróg, Polesiuk & Sieczkowski, "Binders by day,
 * labels by night: effect instances via lexically scoped handlers"
 * (POPL 2020). Two instances of one effect are two names. An operation
 * addressed to an outer instance passes through an inner one of the
 * same effect untouched, because it names its prompt: there is no
 * accidental handling (Zhang & Myers, POPL 2019).
 *
 * The effect `F` never enters the row. A program using instances is
 * `A ! Delim + G`, so there is no `TypeableK` and no `Distinct`, and
 * nothing to misroute.
 *
 * EVERY STRATEGY IS A PRIMITIVE YOU CAN NAME (the operator's rule for
 * this arc). `deep` runs the clauses with shift0 under a `dollar`
 * whose return function is the return clause, the FSCD 2019
 * correspondence. `shallow` runs them with control0 under a `push`,
 * and the clause decides whether to re-install. `row` (today's
 * handlers) and `tail` (evidence passing, stage 1) are the others. A
 * default that picks among them comes later and never replaces the
 * manual choice. The price of `deep`/`shallow` against `row` on State
 * is 3.8x/4.7x (handlers-as-dollar): they are for what `row` cannot
 * do, not for what it does.
 */
object Lexical:

  /** an installed handler of `F`, answering `R`, in a program whose
   * other effects are `G`: the value a body performs operations
   * through. Only an installation makes one. */
  final class Inst[F[+_], R, G[+_]] private[Lexical] (
      val prompt: Prompt[R],
      run: [X] => F[X] => X ! Delim + G):
    /** the operation, to THIS installation and no other */
    def perform[X](e: F[X]): X ! Delim + G = run(e)

  /** a deep handler's operation clauses: `k` resumes the body WITH the
   * handler re-installed */
  trait Ops[F[+_], R, G[+_]]:
    def op[X](e: F[X], k: X => R ! Delim + G): R ! Delim + G

  /** a deep handler: its operation clauses and its return clause */
  trait Clauses[F[+_], A, R, G[+_]] extends Ops[F, R, G]:
    def ret(a: A): R ! Delim + G

  /** a shallow handler's clauses: `k` is BARE (the handler is gone
   * after this operation); `again` re-installs it around a program */
  trait ShallowClauses[F[+_], A, R, G[+_]]:
    def ret(a: A): R ! Delim + G
    def op[X](e: F[X], k: X => R ! Delim + G, again: (R ! Delim + G) => R ! Delim + G): R ! Delim + G

  /**
   * DEEP: `ret $ body`, and every operation a `shift0` to this
   * installation whose clause gets `k` with the handler in it (a
   * shift0 to a `dollar` takes the return function along, `$/S0`).
   */
  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, R, G] => A ! Delim + G)
                              (using at: At): R ! Delim + G =
    val p = Delim.prompt[R]
    val i = new Inst[F, R, G](p, [X] => (e: F[X]) =>
      Delim.shift0[R, X, G](p)(k => c.op(e, k))(using at))
    Delim.dollar[A, R, G](p)(c.ret)(body(i))

  /**
   * SHALLOW: every operation a `control0` to this installation, whose
   * clause gets the BARE continuation. The return clause rides inside a
   * plain `push` as a map, so the bare segment already answers `R`,
   * which is why no typed control0-to-dollar is needed
   * (specs/shift0-dollar.md, stage 3).
   */
  def shallow[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, R, G] => A ! Delim + G)
                                 (using at: At): R ! Delim + G =
    val p = Delim.prompt[R]
    val again: (R ! Delim + G) => R ! Delim + G = prog => Delim.push[R, G](p)(prog)
    val i = new Inst[F, R, G](p, [X] => (e: F[X]) =>
      Delim.control0[R, X, G](p)(k => c.op(e, k, again))(using at))
    Delim.push[R, G](p)(body(i).flatMap(c.ret))

  /** State as instances: the worked example, both strategies */
  object State:
    /** the answer of a state handler over a body answering `A`: a
     * state-passing function */
    type Ans[S, A, G[+_]] = S => (S, A) ! Delim + G

    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, Ans[S, A, G], G] => A ! Delim + G)
                         (using At): (S, A) ! Delim + G =
      Lexical.deep(new Clauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! Delim + G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! Delim + G): Ans[S, A, G] ! Delim + G = e match
          case okay.State.Get() => okay.pure((s: S) => k(s).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => k(s1).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    def shallow[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, Ans[S, A, G], G] => A ! Delim + G)
                            (using At): (S, A) ! Delim + G =
      Lexical.shallow(new ShallowClauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! Delim + G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! Delim + G,
                  again: (Ans[S, A, G] ! Delim + G) => Ans[S, A, G] ! Delim + G): Ans[S, A, G] ! Delim + G = e match
          case okay.State.Get() => okay.pure((s: S) => again(k(s)).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => again(k(s1)).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    extension [S, R, G[+_]](i: Inst[okay.State % S, R, G])
      def get: S ! Delim + G = i.perform(okay.State.Get[S, S]())
      def set(s: S): S ! Delim + G = i.perform(okay.State.Set[S, S](s))
