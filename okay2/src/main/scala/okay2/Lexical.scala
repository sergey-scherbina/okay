package okay2

import scala.annotation.tailrec

/**
 * HANDLER INSTANCES AS PROMPTS — the Scala 3 core's `Lexical`
 * (specs/lexical-instances.md) for the Scala 2 core (okay2-lexical).
 *
 * A row routes an operation by its CLASS, so a row holds one handler
 * per signature. Here a handler INSTALLATION is a fresh prompt, and the
 * body reaches it through the instance value the installation hands
 * it: Biernacki, Piróg, Polesiuk & Sieczkowski, "Binders by day, labels
 * by night" (POPL 2020). Two instances of one effect are two names, and
 * an operation addressed to an outer instance passes through an inner
 * one of the same effect untouched. The effect `F` never enters the
 * row: nothing to split, nothing to misroute.
 *
 * EVERY STRATEGY IS A PRIMITIVE YOU CAN NAME: `deep` (shift0 under a
 * `dollar` whose return function is the return clause), `shallow`
 * (control0 under a `push`), `tail` (the clause answers in place through
 * a cell; guarded against a multi-shot from OUTSIDE by `dollarResumed`),
 * and `tailPure` for a row without `Delim`, where nothing can capture
 * and the close is a walk. The Scala 3 core picks between the two tail
 * closes by whether the row has `Delim` (a `Closing` given found by
 * `NotGiven`); Scala 2 has no `NotGiven`, so here they are two names and
 * the row is written in full (`Delim + G`) where the strategy needs the
 * machine. `walk` runs over `Instances` and `Stacked` holds its `In`
 * (okay2-lexical-walk-stacked).
 */
object Lexical {

  /** an installed handler of `F` in a program whose row is `G`: the
   * value a body performs operations through. Only an installation
   * makes one. */
  abstract class Inst[F <: Row, G <: Row] private[Lexical] () {
    /** the operation, to THIS installation and no other */
    def perform[X](e: F#Op[X]): X ! G
  }

  /** a deep handler's operation clauses over the row `G`: `k` resumes
   * the body WITH the handler re-installed */
  trait Ops[F <: Row, R, G <: Row] {
    def op[X](e: F#Op[X], k: X => R ! G): R ! G
  }

  /** a deep handler: its operation clauses and its return clause */
  trait Clauses[F <: Row, A, R, G <: Row] extends Ops[F, R, G] {
    def ret(a: A): R ! G
  }

  /** a shallow handler's clauses: `k` is BARE (the handler is gone after
   * this operation); `again` re-installs it around a program */
  trait ShallowClauses[F <: Row, A, R, G <: Row] {
    def ret(a: A): R ! G
    def op[X](e: F#Op[X], k: X => R ! G, again: (R ! G) => R ! G): R ! G
  }

  /** a TAIL-RESUMPTIVE handler with state `S`: each clause answers in
   * place with the new state and the resumption value */
  trait TailClauses[F <: Row, S] {
    def op[X](e: F#Op[X], s: S): (S, X)
  }

  /** a tail installation's body was resumed more than once by a capture
   * from outside it: the cell cannot be in both branches */
  final class MultiShotAcrossTail(at: String)
    extends IllegalStateException(
      s"$at: a `tail` handler's body was resumed twice by a capture from outside it, so its state cell would be shared by both branches. Install this handler with `deep`, which keeps the state in the continuation (specs/lexical-instances.md)")

  /** DEEP: `ret $ body`, every operation a `shift0` to this installation
   * whose clause gets `k` with the handler in it (`$/S0`) */
  def deep[F <: Row, A, R, G <: Row](c: Clauses[F, A, R, Delim + G])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                    (implicit at: At): R ! (Delim + G) = {
    val p = Delim.prompt[R]
    val i = new Inst[F, Delim + G] {
      def perform[X](e: F#Op[X]): X ! (Delim + G) = Delim.shift0[R, X, G](p)(k => c.op(e, k))
    }
    Delim.dollar[A, R, G](p)(c.ret)(body(i))
  }

  /** SHALLOW: every operation a `control0` to this installation, whose
   * clause gets the BARE continuation; the return clause rides inside a
   * plain `push` as a flatMap, so the bare segment already answers `R` */
  def shallow[F <: Row, A, R, G <: Row](c: ShallowClauses[F, A, R, Delim + G])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                       (implicit at: At): R ! (Delim + G) = {
    val p = Delim.prompt[R]
    val again: (R ! (Delim + G)) => R ! (Delim + G) = prog => Delim.push[R, G](p)(prog)
    val i = new Inst[F, Delim + G] {
      def perform[X](e: F#Op[X]): X ! (Delim + G) = Delim.control0[R, X, G](p)(k => c.op(e, k, again))
    }
    Delim.push[R, G](p)(body(i).flatMap(c.ret))
  }

  /** the cell an installation threads its state through, one per run */
  private def cellInst[F <: Row, S, G <: Row](c: TailClauses[F, S], cell: Cell[S]): Inst[F, G] = new Inst[F, G] {
    def perform[X](e: F#Op[X]): X ! G = Free.delay { () =>
      val (s1, x) = c.op(e, cell.s)
      cell.s = s1
      pure[G, X](x)
    }
  }
  private final class Cell[S](var s: S)

  /**
   * TAIL on a row that HAS `Delim`: the clause answers in place through
   * the cell, and the guard is a `dollarResumed` that refuses the second
   * RUN of a captured context containing this installation — whether or
   * not the first one returned (the Scala 3 core's `Closing.guarded`,
   * lexical-tail-guard-abort). `Free.delay` makes the cell and the count
   * per run of the program.
   */
  def tail[F <: Row, S, A, G <: Row](s0: S)(c: TailClauses[F, S])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                    (implicit at: At): (S, A) ! (Delim + G) =
    Free.delay { () =>
      val cell = new Cell(s0)
      val guard = Delim.prompt[(S, A)]
      Delim.dollarResumed[A, (S, A), G](guard)(
        a => pure[Delim + G, (S, A)]((cell.s, a)),
        n => if (n > 1) throw new MultiShotAcrossTail(at.where))(body(cellInst[F, S, Delim + G](c, cell)))
    }

  /** the close of a tail installation on a row WITHOUT `Delim`: nothing
   * in the body can capture, so no guard and no machine — WALK the body
   * (a `map` at the root would re-associate every step,
   * lexical-tail-allocs), forwarding every operation with the walk as
   * its continuation */
  private def walkClose[S, A, G <: Row](cell: Cell[S], body: A ! G): (S, A) ! G = {
    // a resumption from a forwarded operation re-enters here: a call
    // inside a closure is not a tail call (State.handleAt's `_loop`)
    def again(x: A ! G): (S, A) ! G = walk(x)
    @tailrec def walk(x: A ! G): (S, A) ! G = Free.resume(x) match {
      case Free.Return(a) => Free.Return((cell.s, a))
      case Free.Inject(e) => walk(Free.Bind(Free.Inject[G, A](e), (a: A) => Free.Return[G, A](a)))
      case Free.Bind(Free.Inject(e), k) => Free.Inject[G, Any](e).flatMap(y => again(k(y)))
      case other => throw new IllegalStateException("resume left a non-head form: " + other)
    }
    walk(body)
  }

  /** TAIL on a row WITHOUT `Delim`: the cell, and the walk as the close */
  def tailPure[F <: Row, S, A, G <: Row](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G): (S, A) ! G =
    Free.delay { () =>
      val cell = new Cell(s0)
      walkClose(cell, body(cellInst[F, S, G](c, cell)))
    }

  /**
   * WALK, an OPTIONAL strategy (the Scala 3 core's lexical-tagged-walk,
   * never the default), built on `Instances`: the installation makes a
   * fresh `Instances.Handle` and walks its body the way a row handler
   * does, with the state threaded purely — no cell, no guard, no capture,
   * no `Delay` per operation. An operation is an inert
   * `Inject(Instances.Op(handle, e))`; the walk answers its own handle's
   * operations and forwards the rest, other instances of the same effect
   * included; `Instances.exhausted` at the top turns an escaped operation
   * into an `IllegalStateException` ("survived"). THE SPINE RULE holds as
   * in the Scala 3 core: an operation inside a `Delim` delimiter's body
   * reaches the machine, not the walk, unless `Delim.run` is INSIDE the
   * walk (TestLexicalWalk).
   */
  def walk[F <: Row, S, A, G <: Row](s0: S)(c: TailClauses[F, S])
                                    (body: Inst[F, Instances[F] + G] => A ! (Instances[F] + G)): (S, A) ! (Instances[F] + G) =
    walkWith[F, S, A, G, Inst[F, Instances[F] + G]](s0)(c)(h => new Inst[F, Instances[F] + G] {
      def perform[X](e: F#Op[X]): X ! (Instances[F] + G) = Free.Inject[Instances[F] + G, X](Instances.Op[F, X](h, e))
    })(body)

  /** the walk over an instance `mk` makes for the handle (State's has typed doors) */
  private def walkWith[F <: Row, S, A, G <: Row, I <: Inst[F, Instances[F] + G]](s0: S)(c: TailClauses[F, S])
                                                (mk: Instances.Handle => I)
                                                (body: I => A ! (Instances[F] + G)): (S, A) ! (Instances[F] + G) = {
    type R = Instances[F] + G
    Free.delay { () =>
      val handle = Instances.handle("walk")
      def again(s: S)(x: A ! R): (S, A) ! R = loop(s)(x)
      @tailrec def loop(s: S)(x: A ! R): (S, A) ! R = Free.resume(x) match {
        case Free.Return(a) => Free.Return((s, a))
        case Free.Inject(e) => loop(s)(Free.Bind(Free.Inject[R, A](e), (a: A) => Free.Return[R, A](a)))
        case Free.Bind(Free.Inject(o: Instances.Op[_, _]), k) if o.at eq handle =>
          // THE ONE CAST: okay2's `Instances.Op` erases its operation
          // (`op: Any`, by its own design), and the handle identity is
          // what says this one is F's — the Scala 3 core keeps it typed
          val (s1, y) = c.op[Any](o.op.asInstanceOf[F#Op[Any]], s)
          loop(s1)(k(y))
        case Free.Bind(Free.Inject(e), k) => Free.Inject[R, Any](e).flatMap(y => again(s)(k(y)))
        case other => throw new IllegalStateException("resume left a non-head form: " + other)
      }
      loop(s0)(body(mk(handle)))
    }
  }

  /** THE DEFAULT: pick the strategy from what the clauses ARE; every
   * strategy stays callable by name */
  def handle[F <: Row, S, A, G <: Row](s0: S)(c: TailClauses[F, S])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                      (implicit at: At): (S, A) ! (Delim + G) = tail(s0)(c)(body)
  def handle[F <: Row, A, R, G <: Row](c: Clauses[F, A, R, Delim + G])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                      (implicit at: At): R ! (Delim + G) = deep(c)(body)
  def handle[F <: Row, A, R, G <: Row](c: ShallowClauses[F, A, R, Delim + G])(body: Inst[F, Delim + G] => A ! (Delim + G))
                                      (implicit at: At): R ! (Delim + G) = shallow(c)(body)

  /** State as instances: the worked example, every strategy */
  object State {
    /** the answer of a deep or shallow state handler over a body
     * answering `A`: a state-passing function */
    type Ans[S, A, G <: Row] = S => (S, A) ! G

    /**
     * A State instance with TYPED doors. Scala 2 does not refine `X`
     * from `Get[S] <: Op[S, S]` in a match (the Scala 3 core's clauses
     * do exactly that), so `get`/`set`/`put` are the API a body writes,
     * built by each strategy without a clause, and `perform` is the
     * generic road through ONE cast, isolated here and true by the
     * declarations `Get[S] extends Op[S, S]`, `Set[S] extends Op[S, S]`.
     */
    abstract class Inst[S, G <: Row] private[Lexical] () extends Lexical.Inst[okay2.State[S], G] {
      def get: S ! G
      def set(s: S): S ! G
      /** `set` as a STATEMENT, answering `Unit` */
      def put(s: S): Unit ! G = set(s).map(_ => ())
      def perform[X](e: okay2.State.Op[S, X]): X ! G = (e match {
        case okay2.State.Get() => get
        case okay2.State.Set(s1) => set(s1)
      }).asInstanceOf[X ! G]
    }

    def deep[S, A, G <: Row](s0: S)(body: Inst[S, Delim + G] => A ! (Delim + G))
                            (implicit at: At): (S, A) ! (Delim + G) = {
      type R = Ans[S, A, Delim + G]
      val p = Delim.prompt[R]
      val i = new Inst[S, Delim + G] {
        def get: S ! (Delim + G) = Delim.shift0[R, S, G](p)(k => pure[Delim + G, R]((s: S) => k(s).flatMap(f => f(s))))
        def set(s1: S): S ! (Delim + G) = Delim.shift0[R, S, G](p)(k => pure[Delim + G, R]((_: S) => k(s1).flatMap(f => f(s1))))
      }
      Delim.dollar[A, R, G](p)(a => pure[Delim + G, R]((s: S) => pure[Delim + G, (S, A)]((s, a))))(body(i)).flatMap(f => f(s0))
    }

    def shallow[S, A, G <: Row](s0: S)(body: Inst[S, Delim + G] => A ! (Delim + G))
                               (implicit at: At): (S, A) ! (Delim + G) = {
      type R = Ans[S, A, Delim + G]
      val p = Delim.prompt[R]
      val again: (R ! (Delim + G)) => R ! (Delim + G) = prog => Delim.push[R, G](p)(prog)
      val i = new Inst[S, Delim + G] {
        def get: S ! (Delim + G) = Delim.control0[R, S, G](p)(k => pure[Delim + G, R]((s: S) => again(k(s)).flatMap(f => f(s))))
        def set(s1: S): S ! (Delim + G) = Delim.control0[R, S, G](p)(k => pure[Delim + G, R]((_: S) => again(k(s1)).flatMap(f => f(s1))))
      }
      Delim.push[R, G](p)(body(i).flatMap(a => pure[Delim + G, R]((s: S) => pure[Delim + G, (S, A)]((s, a))))).flatMap(f => f(s0))
    }

    /** the cell-backed doors every tail strategy shares */
    private def cellInst[S, G <: Row](cell: Cell[S]): Inst[S, G] = new Inst[S, G] {
      def get: S ! G = Free.delay(() => pure[G, S](cell.s))
      def set(s1: S): S ! G = Free.delay { () => cell.s = s1; pure[G, S](s1) }
    }

    /** the DEFAULT for State: its clauses are tail-resumptive, so `tail` */
    def apply[S, A, G <: Row](s0: S)(body: Inst[S, Delim + G] => A ! (Delim + G))(implicit at: At): (S, A) ! (Delim + G) =
      tail(s0)(body)

    /** TAIL, for State, on a row with `Delim`: the state in the cell, guarded by `dollarResumed` */
    def tail[S, A, G <: Row](s0: S)(body: Inst[S, Delim + G] => A ! (Delim + G))(implicit at: At): (S, A) ! (Delim + G) =
      Free.delay { () =>
        val cell = new Cell(s0)
        val guard = Delim.prompt[(S, A)]
        Delim.dollarResumed[A, (S, A), G](guard)(
          a => pure[Delim + G, (S, A)]((cell.s, a)),
          n => if (n > 1) throw new MultiShotAcrossTail(at.where))(body(cellInst[S, Delim + G](cell)))
      }

    /** TAIL, for State, on a row WITHOUT `Delim`: no guard, no machine */
    def tailPure[S, A, G <: Row](s0: S)(body: Inst[S, G] => A ! G): (S, A) ! G =
      Free.delay { () =>
        val cell = new Cell(s0)
        walkClose(cell, body(cellInst[S, G](cell)))
      }

    private def stateClauses[S]: TailClauses[okay2.State[S], S] = new TailClauses[okay2.State[S], S] {
      // the walk hands the operation back erased (see `walkWith`); the
      // answer is read off the operation's class, as `perform` does
      def op[X](e: okay2.State.Op[S, X], s: S): (S, X) = (e match {
        case okay2.State.Get() => (s, s)
        case okay2.State.Set(s1) => (s1, s1)
      }).asInstanceOf[(S, X)]
    }

    /** WALK, for State (optional, never the default): typed doors that
     * emit `Instances.Op` at the walk's handle */
    def walk[S, A, G <: Row](s0: S)(body: Inst[S, Instances[okay2.State[S]] + G] => A ! (Instances[okay2.State[S]] + G))
        : (S, A) ! (Instances[okay2.State[S]] + G) = {
      type R = Instances[okay2.State[S]] + G
      Lexical.walkWith[okay2.State[S], S, A, G, Inst[S, R]](s0)(stateClauses[S])(h => new Inst[S, R] {
        def get: S ! R = Free.Inject[R, S](Instances.Op[okay2.State[S], S](h, okay2.State.Get[S]()))
        def set(s1: S): S ! R = Free.Inject[R, S](Instances.Op[okay2.State[S], S](h, okay2.State.Set[S](s1)))
      })(body)
    }
  }

  /**
   * STACKED INSTANCES: the same strategies over `Delim.Stacked`, so an
   * instance used outside its installation does not compile. okay2's
   * `In` is final and its stack is a VALUE, so an instance HOLDS its
   * `In` and every door takes the stack in force as an argument
   * (`a.get(st)`), asking `Has[S2, in.p.type]` the way a stacked
   * `shift0` does; the body gets the installation's stack as `i.in.stack`.
   * `shallow` is not stacked, for the reason `control0` is not.
   */
  object Stacked {
    import Delim.Stacked.{In, Stack, Has, Stk}

    // EVERY DOOR IS A BUILDER, `deep[A, G](st)(…)`: Scala 2 takes all
    // type arguments or none, and what the arguments cannot fix — the
    // body's answer `A` (a lambda's result is typed after its parameter)
    // and the rest of the row `G` — is written; `apply` infers the rest
    // (`S` from the stack, `S0` from the seed, `F`/`R` from clauses).

    /** a stacked DEEP instance over user clauses */
    final class Deep[F <: Row, R, G <: Row, S <: Stk] private[Lexical] (val in: In[R, S], ops: Ops[F, R, Delim + G]) {
      def perform[S2 <: Stk, X](st: Stack[S2])(e: F#Op[X])(implicit ev: Has[S2, in.p.type], at: At): X ! (Delim + G) =
        st.shift0[R, X, G](in.p)(ev, at).apply(_ => k => ops.op(e, k))
    }
    final class DeepDoor[G <: Row] private[Lexical] () {
      def apply[F <: Row, A, R, S <: Stk](st: Stack[S])(c: Clauses[F, A, R, Delim + G])(body: Deep[F, R, G, S] => A ! (Delim + G))(implicit at: At): R ! (Delim + G) =
        st.dollar[A, R, G](c.ret)(in => body(new Deep[F, R, G, S](in, c)))
    }
    def deep[G <: Row]: DeepDoor[G] = new DeepDoor[G]()

    /** a stacked TAIL instance over user clauses: answers in place; the
     * stack check is what makes holding it safe */
    final class Tail[F <: Row, S0, A, G <: Row, S <: Stk] private[Lexical] (val in: In[(S0, A), S], c: TailClauses[F, S0], cell: Cell[S0]) {
      def perform[S2 <: Stk, X](st: Stack[S2])(e: F#Op[X])(implicit ev: Has[S2, in.p.type]): X ! (Delim + G) = {
        val _ = (st, ev)
        Free.delay { () =>
          val (s1, x) = c.op(e, cell.s)
          cell.s = s1
          pure[Delim + G, X](x)
        }
      }
    }
    final class TailDoor[A, G <: Row] private[Lexical] () {
      def apply[F <: Row, S0, S <: Stk](st: Stack[S])(s0: S0)(c: TailClauses[F, S0])(body: Tail[F, S0, A, G, S] => A ! (Delim + G))(implicit at: At): (S0, A) ! (Delim + G) =
        Free.delay { () =>
          val cell = new Cell(s0)
          st.dollarResumed[A, (S0, A), G](
            a => pure[Delim + G, (S0, A)]((cell.s, a)),
            n => if (n > 1) throw new MultiShotAcrossTail(at.where))(in => body(new Tail[F, S0, A, G, S](in, c, cell)))
        }
    }
    def tail[A, G <: Row]: TailDoor[A, G] = new TailDoor[A, G]()

    /** State, stacked, with typed doors (the GADT gap, as unstacked) */
    object State {
      type Ans[S0, A, G <: Row] = S0 => (S0, A) ! (Delim + G)

      final class Deep[S0, A, G <: Row, S <: Stk] private[Lexical] (val in: In[Ans[S0, A, G], S]) {
        private type R = Ans[S0, A, G]
        def get[S2 <: Stk](st: Stack[S2])(implicit ev: Has[S2, in.p.type], at: At): S0 ! (Delim + G) =
          st.shift0[R, S0, G](in.p)(ev, at).apply(_ => k => pure[Delim + G, R]((s: S0) => k(s).flatMap(f => f(s))))
        def set[S2 <: Stk](st: Stack[S2])(s1: S0)(implicit ev: Has[S2, in.p.type], at: At): S0 ! (Delim + G) =
          st.shift0[R, S0, G](in.p)(ev, at).apply(_ => k => pure[Delim + G, R]((_: S0) => k(s1).flatMap(f => f(s1))))
      }
      final class DeepDoor[A, G <: Row] private[Lexical] () {
        def apply[S0, S <: Stk](st: Stack[S])(s0: S0)(body: Deep[S0, A, G, S] => A ! (Delim + G))(implicit at: At): (S0, A) ! (Delim + G) =
          st.dollar[A, Ans[S0, A, G], G](a => pure[Delim + G, Ans[S0, A, G]]((s: S0) => pure[Delim + G, (S0, A)]((s, a))))(in =>
            body(new Deep[S0, A, G, S](in))).flatMap(f => f(s0))
      }
      def deep[A, G <: Row]: DeepDoor[A, G] = new DeepDoor[A, G]()

      final class Tail[S0, A, G <: Row, S <: Stk] private[Lexical] (val in: In[(S0, A), S], cell: Cell[S0]) {
        def get[S2 <: Stk](st: Stack[S2])(implicit ev: Has[S2, in.p.type]): S0 ! (Delim + G) =
          { val _ = (st, ev); Free.delay(() => pure[Delim + G, S0](cell.s)) }
        def set[S2 <: Stk](st: Stack[S2])(s1: S0)(implicit ev: Has[S2, in.p.type]): S0 ! (Delim + G) =
          { val _ = (st, ev); Free.delay { () => cell.s = s1; pure[Delim + G, S0](s1) } }
      }
      final class TailDoor[A, G <: Row] private[Lexical] () {
        def apply[S0, S <: Stk](st: Stack[S])(s0: S0)(body: Tail[S0, A, G, S] => A ! (Delim + G))(implicit at: At): (S0, A) ! (Delim + G) =
          Free.delay { () =>
            val cell = new Cell(s0)
            st.dollarResumed[A, (S0, A), G](
              a => pure[Delim + G, (S0, A)]((cell.s, a)),
              n => if (n > 1) throw new MultiShotAcrossTail(at.where))(in => body(new Tail[S0, A, G, S](in, cell)))
          }
      }
      def tail[A, G <: Row]: TailDoor[A, G] = new TailDoor[A, G]()
    }
  }
}
