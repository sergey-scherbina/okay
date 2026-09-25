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
 * machine. `walk` (over `Instances`) and the stacked instances are not
 * ported yet (okay2/backlog.d).
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
  }
}
