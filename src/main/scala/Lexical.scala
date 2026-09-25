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

  /** an installed handler of `F` in a program whose other effects are
   * `G`: the value a body performs operations through. Only an
   * installation makes one. The strategy is fixed by the installation,
   * so a body written against `Inst[F, G]` runs under any of them:
   * changing strategy is changing one word at the installation. */
  final class Inst[F[+_], G[+_]] private[Lexical] (run: [X] => F[X] => X ! Delim + G):
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
  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, G] => A ! Delim + G)
                              (using at: At): R ! Delim + G =
    val p = Delim.prompt[R]
    val i = new Inst[F, G]([X] => (e: F[X]) =>
      Delim.shift0[R, X, G](p)(k => c.op(e, k))(using at))
    Delim.dollar[A, R, G](p)(c.ret)(body(i))

  /**
   * SHALLOW: every operation a `control0` to this installation, whose
   * clause gets the BARE continuation. The return clause rides inside a
   * plain `push` as a map, so the bare segment already answers `R`,
   * which is why no typed control0-to-dollar is needed
   * (specs/shift0-dollar.md, stage 3).
   */
  def shallow[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, G] => A ! Delim + G)
                                 (using at: At): R ! Delim + G =
    val p = Delim.prompt[R]
    val again: (R ! Delim + G) => R ! Delim + G = prog => Delim.push[R, G](p)(prog)
    val i = new Inst[F, G]([X] => (e: F[X]) =>
      Delim.control0[R, X, G](p)(k => c.op(e, k, again))(using at))
    Delim.push[R, G](p)(body(i).flatMap(c.ret))

  /** a TAIL-RESUMPTIVE handler with state `S`: each clause answers in
   * place with the new state and the resumption value. It cannot drop,
   * store or repeat the continuation, which is what makes answering
   * without a capture possible. */
  trait TailClauses[F[+_], S]:
    def op[X](e: F[X], s: S): (S, X)

  /** a tail installation's body was resumed more than once by a
   * capture from OUTSIDE it: the cell cannot be in both branches */
  final class MultiShotAcrossTail(at: String)
    extends IllegalStateException(
      s"$at: a `tail` handler's body was resumed twice by a capture from outside it, so its state cell would be shared by both branches. Install this handler with `deep`, which keeps the state in the continuation (specs/lexical-instances.md)")

  /**
   * TAIL: evidence passing (Xie, Brachthäuser, Hillerström, Schuster &
   * Leijen, "Effect handlers, evidently", ICFP 2020). An operation
   * calls its clause IN PLACE through the instance. Nothing is
   * captured, and the handler's state lives in a cell that the
   * installation makes fresh on every run of the program.
   *
   * THE PRECONDITION, AND WHY IT IS CHECKED RATHER THAN ASSUMED. A cell
   * and a continuation-carried state agree whenever the body runs once
   * per installation. A multi-shot capture INSIDE the body (to a prompt
   * installed within it) threads the cell through its branches in order,
   * which is also what `deep` does, since `deep`'s state capture crosses
   * that inner prompt. They differ only when a capture from OUTSIDE the
   * installation resumes its body twice. Then both branches would share
   * the cell. That case is detected: the installation is a `dollar` on a
   * private prompt, and its return function runs once per resumption,
   * so a second run throws `MultiShotAcrossTail` instead of answering
   * wrongly. The price of the check is one delimiter per INSTALLATION,
   * not per operation.
   */
  def tail[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! Delim + G)
                              (using at: At): (S, A) ! Delim + G =
    Free.delay { () =>
      var cell = s0
      var returned = false
      val i = new Inst[F, G]([X] => (e: F[X]) => Free.delay { () =>
        val (s1, x) = c.op(e, cell)
        cell = s1
        okay.pure[Delim + G, X](x)
      })
      val guard = Delim.prompt[(S, A)]
      Delim.dollar[A, (S, A), G](guard)(a => Free.delay { () =>
        if returned then throw MultiShotAcrossTail(at.where)
        returned = true
        okay.pure[Delim + G, (S, A)]((cell, a))
      })(body(i))
    }

  /**
   * THE DEFAULT (stage 3): pick the strategy from what the clauses ARE.
   * Every strategy it picks from stays callable by name (`tail`,
   * `deep`, `shallow`), so the default only saves writing the name.
   *
   *   - tail-resumptive clauses (`TailClauses`) → `tail`: in place, one
   *     guard delimiter per installation. It either answers exactly as
   *     `deep` would, or throws `MultiShotAcrossTail` in the one shape
   *     where a cell cannot (a capture from outside resuming the body
   *     twice). It never answers wrongly.
   *   - general deep clauses (`Clauses`) → `deep`.
   *   - shallow clauses (`ShallowClauses`) → `shallow`.
   *
   * The row (`State.handle` and friends) is not in this list. It is the
   * library's default for ONE handler of a kind, and instances are for
   * when that is not enough.
   */
  def handle[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! Delim + G)
                                (using At): (S, A) ! Delim + G = tail(s0)(c)(body)
  def handle[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, G] => A ! Delim + G)
                                (using At): R ! Delim + G = deep(c)(body)
  def handle[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, G] => A ! Delim + G)
                                (using At): R ! Delim + G = shallow(c)(body)

  /** State as instances: the worked example, all three strategies */
  object State:
    /** the answer of a state handler over a body answering `A`: a
     * state-passing function */
    type Ans[S, A, G[+_]] = S => (S, A) ! Delim + G

    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! Delim + G)
                         (using At): (S, A) ! Delim + G =
      Lexical.deep(new Clauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! Delim + G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! Delim + G): Ans[S, A, G] ! Delim + G = e match
          case okay.State.Get() => okay.pure((s: S) => k(s).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => k(s1).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    def shallow[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! Delim + G)
                            (using At): (S, A) ! Delim + G =
      Lexical.shallow(new ShallowClauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! Delim + G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! Delim + G,
                  again: (Ans[S, A, G] ! Delim + G) => Ans[S, A, G] ! Delim + G): Ans[S, A, G] ! Delim + G = e match
          case okay.State.Get() => okay.pure((s: S) => again(k(s)).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => again(k(s1)).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    /** the DEFAULT for State: its clauses are tail-resumptive, so `tail` */
    def apply[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! Delim + G)(using At): (S, A) ! Delim + G =
      tail(s0)(body)

    /** TAIL, for State: the state in the installation's cell */
    def tail[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! Delim + G)(using At): (S, A) ! Delim + G =
      Lexical.tail[okay.State % S, S, A, G](s0)(new TailClauses[okay.State % S, S]:
        def op[X](e: okay.State[S, X], s: S): (S, X) = e match
          case okay.State.Get() => (s, s)
          case okay.State.Set(s1) => (s1, s1)
      )(body)

    extension [S, G[+_]](i: Inst[okay.State % S, G])
      def get: S ! Delim + G = i.perform(okay.State.Get[S, S]())
      def set(s: S): S ! Delim + G = i.perform(okay.State.Set[S, S](s))

  /**
   * STACKED INSTANCES (stage 2): the same strategies over
   * `Delim.Stacked`, so an instance used outside its installation does
   * not compile. The instance IS the installation's delimiter (a
   * subclass of `Delim.Stacked.In`), so its prompt is the singleton on
   * the stack, and `perform` asks for the same `Has` evidence a stacked
   * `shift0` does. `import i.given` puts the installation's stack in
   * force for the body, exactly as for `Delim.Stacked.reset`. `shallow`
   * is not stacked, for the reason `control0` is not
   * (specs/shift0-dollar.md, stage 2).
   */
  object Stacked:
    import Delim.Stacked.{In, Stack, Under, Has}

    /** a stacked DEEP instance: the delimiter on the stack, and its clauses */
    final class Deep[F[+_], R, G[+_], S <: Tuple] private[Lexical] (p0: Prompt[R], ops: Ops[F, R, G])
        extends In[R, S](p0):
      /** the operation, to THIS installation, which must be on the stack */
      def perform[X](e: F[X])(using st: Stack[?])[B <: Tuple](using Has.Aux[st.S, p.type, B], At): Under[G, X, st.S] =
        Delim.Stacked.shift0[R, X, G](p)(k => Prog.diag[B, Delim + G, R](ops.op(e, x => k(x).free)))

    def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(using st: Stack[?])
                                (body: (i: Deep[F, R, G, st.S]) => Under[G, A, i.p.type *: st.S])
                                (using at: At): Under[G, R, st.S] =
      val i = new Deep[F, R, G, st.S](Delim.prompt[R], c)
      Prog.diag[st.S, Delim + G, R](Delim.dollar[A, R, G](i.p)(c.ret)(body(i).free))

    /** a stacked TAIL instance: it answers in place, and the stack check
     * is what makes holding it safe */
    final class Tail[F[+_], S0, A, G[+_], S <: Tuple] private[Lexical] (
        p0: Prompt[(S0, A)], answer: [X] => F[X] => X ! Delim + G) extends In[(S0, A), S](p0):
      def perform[X](e: F[X])(using st: Stack[?])[B <: Tuple](using Has.Aux[st.S, p.type, B]): Under[G, X, st.S] =
        Prog.diag[st.S, Delim + G, X](answer(e))

    /** `Lexical.tail`, stacked: the cell and the guard per run, the
     * guard's delimiter being the instance itself */
    def tail[F[+_], S0, A, G[+_]](s0: S0)(c: TailClauses[F, S0])(using st: Stack[?])
                                 (body: (i: Tail[F, S0, A, G, st.S]) => Under[G, A, i.p.type *: st.S])
                                 (using at: At): Under[G, (S0, A), st.S] =
      Prog.diag[st.S, Delim + G, (S0, A)](Free.delay { () =>
        var cell = s0
        var returned = false
        val i = new Tail[F, S0, A, G, st.S](Delim.prompt[(S0, A)], [X] => (e: F[X]) => Free.delay { () =>
          val (s1, x) = c.op(e, cell)
          cell = s1
          okay.pure[Delim + G, X](x)
        })
        Delim.dollar[A, (S0, A), G](i.p)(a => Free.delay { () =>
          if returned then throw MultiShotAcrossTail(at.where)
          returned = true
          okay.pure[Delim + G, (S0, A)]((cell, a))
        })(body(i).free)
      })
