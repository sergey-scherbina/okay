package okay

import okay.Row.up

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

  /** an installed handler of `F` in a program whose row is `G` (the
   * WHOLE row the body uses, `Delim` included when the strategy needs
   * it): the value a body performs operations through. Only an
   * installation makes one. A body written against `Inst[F, G]` runs
   * under any strategy that accepts its row. */
  abstract class Inst[F[+_], G[+_]] private[Lexical] ():
    /** the operation, to THIS installation and no other */
    def perform[X](e: F[X]): X ! G

  /** a deep handler's operation clauses over the row `G`: `k` resumes
   * the body WITH the handler re-installed */
  trait Ops[F[+_], R, G[+_]]:
    def op[X](e: F[X], k: X => R ! G): R ! G

  /** a deep handler: its operation clauses and its return clause */
  trait Clauses[F[+_], A, R, G[+_]] extends Ops[F, R, G]:
    def ret(a: A): R ! G

  /** a shallow handler's clauses: `k` is BARE (the handler is gone
   * after this operation); `again` re-installs it around a program */
  trait ShallowClauses[F[+_], A, R, G[+_]]:
    def ret(a: A): R ! G
    def op[X](e: F[X], k: X => R ! G, again: (R ! G) => R ! G): R ! G

  /** `Delim + G` read as `G` when `G` already has `Delim`: the union
   * collapses, and `liftCo` over `x | G` builds the witness from the
   * membership alone, so no cast (the `Row.Sub` shape, Row.scala) */
  private def collapse[G[+_]](ev: Delim[Any] <:< G[Any]): Row.Sub[Delim + G, G] =
    ev.liftCo[[x] =>> x | G[Any]]

  /**
   * DEEP: `ret $ body`, and every operation a `shift0` to this
   * installation whose clause gets `k` with the handler in it (a
   * shift0 to a `dollar` takes the return function along, `$/S0`).
   * The row must hold `Delim`: every operation is a capture.
   */
  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, G] => A ! G)
                              (using ev: Delim[Any] <:< G[Any], at: At): R ! G =
    given Row.Sub[Delim + G, G] = collapse(ev)
    val p = Delim.prompt[R]
    val i = new Inst[F, G]:
      def perform[X](e: F[X]): X ! G =
        Delim.shift0[R, X, G](p)(k => c.op(e, x => k(x).up[G]).up[Delim + G])(using at).up[G]
    Delim.dollar[A, R, G](p)(a => c.ret(a).up[Delim + G])(body(i).up[Delim + G]).up[G]

  /**
   * SHALLOW: every operation a `control0` to this installation, whose
   * clause gets the BARE continuation. The return clause rides inside a
   * plain `push` as a map, so the bare segment already answers `R`
   * (specs/shift0-dollar.md, stage 3).
   */
  def shallow[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, G] => A ! G)
                                 (using ev: Delim[Any] <:< G[Any], at: At): R ! G =
    given Row.Sub[Delim + G, G] = collapse(ev)
    val p = Delim.prompt[R]
    val again: (R ! G) => R ! G = prog => Delim.push[R, G](p)(prog.up[Delim + G]).up[G]
    val i = new Inst[F, G]:
      def perform[X](e: F[X]): X ! G =
        Delim.control0[R, X, G](p)(k => c.op(e, x => k(x).up[G], again).up[Delim + G])(using at).up[G]
    Delim.push[R, G](p)(body(i).flatMap(c.ret).up[Delim + G]).up[G]

  /** a TAIL-RESUMPTIVE handler with state `S`: each clause answers in
   * place with the new state and the resumption value. It cannot drop,
   * store or repeat the continuation, which is what makes answering
   * without a capture possible. */
  trait TailClauses[F[+_], S]:
    def op[X](e: F[X], s: S): (S, X)

  /** a tail installation's body was resumed more than once by a
   * capture from outside it: the cell cannot be in both branches */
  final class MultiShotAcrossTail(at: String)
    extends IllegalStateException(
      s"$at: a `tail` handler's body was resumed twice by a capture from outside it, so its state cell would be shared by both branches. Install this handler with `deep`, which keeps the state in the continuation (specs/lexical-instances.md)")

  /**
   * HOW A TAIL INSTALLATION CLOSES, decided by the ROW at compile time
   * (lexical-tail-allocs, pay-as-you-go). A body whose row has no
   * `Delim` cannot be resumed twice from outside, because nothing in it
   * can capture. It needs no guard and no machine: it ends with a plain
   * `map`, and the program stays `A ! G`, run by whatever runs `G`. A
   * body whose row HAS `Delim` gets the guard: a `dollar` whose return
   * function runs once per resumption.
   */
  sealed trait Closing[G[+_]]:
    def close[S, A](body: A ! G, finish: A => (S, A), at: String): (S, A) ! G

  object Closing:
    given unguarded[G[+_]](using scala.util.NotGiven[Delim[Any] <:< G[Any]]): Closing[G] with
      /** WALK the body rather than `map` over it. A `map` at the root is
       * a `Bind` over the whole body, and `resume` then re-associates
       * every step of it into a fresh closure and `Bind`: measured +36 B
       * per operation (lexical-tail-allocs; "never map over a residual").
       * The walk is `State.handle`'s loop with nothing to handle: the
       * tail operations are `Delay` nodes that `resume` forces in place,
       * and a foreign operation is re-emitted with the walk as its
       * continuation. */
      def close[S, A](body: A ! G, finish: A => (S, A), at: String): (S, A) ! G =
        def walk(x: A ! G): (S, A) ! G = (x.resume: @unchecked) match
          case Free.Return(a) => okay.pure(finish(a))
          case Free.Inject(e) => Free.Inject(e).flatMap(a => okay.pure(finish(a)))
          case Free.Bind(Free.Inject(e), k) => Free.Inject(e).flatMap(y => walk(k(y)))
        walk(body)

    given guarded[G[+_]](using ev: Delim[Any] <:< G[Any]): Closing[G] with
      def close[S, A](body: A ! G, finish: A => (S, A), at: String): (S, A) ! G =
        given Row.Sub[Delim + G, G] = collapse(ev)
        Free.delay { () =>
          var returned = false
          val guard = Delim.prompt[(S, A)]
          Delim.dollar[A, (S, A), G](guard)(a => Free.delay { () =>
            if returned then throw MultiShotAcrossTail(at)
            returned = true
            okay.pure[Delim + G, (S, A)](finish(a))
          })(body.up[Delim + G]).up[G]
        }

  /**
   * TAIL: evidence passing (Xie, Brachthäuser, Hillerström, Schuster &
   * Leijen, "Effect handlers, evidently", ICFP 2020). An operation
   * calls its clause IN PLACE through the instance, and the handler's
   * state lives in a cell made fresh on every run of the program.
   * Nothing is captured. The one shape where a cell and `deep`'s
   * continuation-carried state disagree (a capture from outside the
   * installation resuming its body twice) exists only if the row has
   * `Delim`, and there `Closing.guarded` throws `MultiShotAcrossTail`
   * instead of answering wrongly. Without `Delim` the installation costs
   * nothing beyond its operations.
   */
  def tail[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G)
                              (using closing: Closing[G], at: At): (S, A) ! G =
    Free.delay { () =>
      var cell = s0
      val i = new Inst[F, G]:
        def perform[X](e: F[X]): X ! G = Free.delay { () =>
          val (s1, x) = c.op(e, cell)
          cell = s1
          okay.pure[G, X](x)
        }
      closing.close(body(i), a => (cell, a), at.where)
    }

  /**
   * WALK, an OPTIONAL strategy (lexical-tagged-walk; never the default),
   * built on `Instances` (instances-unify). The installation makes a
   * fresh `Instances.Handle` and walks its body the way a row handler
   * does, with the state threaded purely: no cell, no guard, no capture,
   * no `Delay` per operation. An operation is an inert
   * `Inject(Instances(handle, e))`, typed by its signature, so there is no
   * cast. The walk answers its own handle's operations and forwards the
   * rest, other instances of the same effect included. The row carries
   * `Instances.Of[F]` once per effect signature, and `Instances.exhausted`
   * at the top turns an escaped operation into `Instances.Survived`.
   *
   * THE SPINE RULE. A walk sees the program's spine. An instance
   * operation inside a `Delim` delimiter's body (a `reset`, a `dollar`,
   * a `Layered.reify`) reaches the machine, not the walk, and survives to
   * `exhausted`, which throws. Put `Delim.run` INSIDE the walk and the
   * machine's suspended operations come back along the spine, where the
   * walk answers them in order. It never answers silently differently
   * from `deep` (TestLexicalWalk).
   */
  def walk[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])
                              (body: Inst[F, Instances.Of[F] + G] => A ! Instances.Of[F] + G)
                              (using TypeableK[Instances.Of[F]]): (S, A) ! Instances.Of[F] + G =
    type R = Instances.Of[F] + G
    Free.delay { () =>
      val handle = Instances.handle("walk")
      val i = new Inst[F, R]:
        def perform[X](e: F[X]): X ! R = Free.Inject(Instances[F, X](handle, e))
      // a resumption from a forwarded operation re-enters here: a call
      // inside a closure is not a tail call (State.handle's `_loop`)
      def again(s: S)(x: A ! R): (S, A) ! R = loop(s)(x)
      @scala.annotation.tailrec
      def loop(s: S)(x: A ! R): (S, A) ! R = (x.resume: @unchecked) match
        case Free.Return(a) => okay.pure((s, a))
        case Free.Inject(e) => split[Instances.Of[F], G](e) { in =>
            if in.at eq handle then { val (s1, a) = c.op(in.op, s); okay.pure[R, (S, A)]((s1, a)) }
            else Free.Inject(in).map(a => (s, a))
          } { g => Free.Inject(g).map(a => (s, a)) }
        case Free.Bind(Free.Inject(e), k) => split[Instances.Of[F], G](e) { in =>
            if in.at eq handle then { val (s1, y) = c.op(in.op, s); loop(s1)(k(y)) }
            else Free.Inject(in).flatMap(y => again(s)(k(y)))
          } { g => Free.Inject(g).flatMap(y => again(s)(k(y))) }
      loop(s0)(body(i))
    }

  /**
   * THE DEFAULT: pick the strategy from what the clauses ARE. Every
   * strategy stays callable by name. TailClauses → `tail` (guarded
   * only if the row can capture), Clauses → `deep`, ShallowClauses →
   * `shallow`. The row handlers stay the library's default for ONE
   * handler of a kind.
   */
  def handle[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G)
                                (using Closing[G], At): (S, A) ! G = tail(s0)(c)(body)
  def handle[F[+_], A, R, G[+_]](c: Clauses[F, A, R, G])(body: Inst[F, G] => A ! G)
                                (using Delim[Any] <:< G[Any], At): R ! G = deep(c)(body)
  def handle[F[+_], A, R, G[+_]](c: ShallowClauses[F, A, R, G])(body: Inst[F, G] => A ! G)
                                (using Delim[Any] <:< G[Any], At): R ! G = shallow(c)(body)

  /** State as instances: the worked example, every strategy */
  object State:
    /** the answer of a deep or shallow state handler over a body
     * answering `A`: a state-passing function */
    type Ans[S, A, G[+_]] = S => (S, A) ! G

    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)
                         (using Delim[Any] <:< G[Any], At): (S, A) ! G =
      Lexical.deep(new Clauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! G): Ans[S, A, G] ! G = e match
          case okay.State.Get() => okay.pure((s: S) => k(s).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => k(s1).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    def shallow[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)
                            (using Delim[Any] <:< G[Any], At): (S, A) ! G =
      Lexical.shallow(new ShallowClauses[okay.State % S, A, Ans[S, A, G], G]:
        def ret(a: A): Ans[S, A, G] ! G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! G,
                  again: (Ans[S, A, G] ! G) => Ans[S, A, G] ! G): Ans[S, A, G] ! G = e match
          case okay.State.Get() => okay.pure((s: S) => again(k(s)).flatMap(f => f(s)))
          case okay.State.Set(s1) => okay.pure((_: S) => again(k(s1)).flatMap(f => f(s1)))
      )(body).flatMap(f => f(s0))

    /** the DEFAULT for State: its clauses are tail-resumptive, so `tail` */
    def apply[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      tail(s0)(body)

    /** TAIL, for State: the state in the installation's cell */
    def tail[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      Lexical.tail[okay.State % S, S, A, G](s0)(new TailClauses[okay.State % S, S]:
        def op[X](e: okay.State[S, X], s: S): (S, X) = e match
          case okay.State.Get() => (s, s)
          case okay.State.Set(s1) => (s1, s1)
      )(body)

    /** WALK, for State (optional, never the default) */
    def walk[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, Instances.Of[okay.State % S] + G] => A ! Instances.Of[okay.State % S] + G)
        : (S, A) ! Instances.Of[okay.State % S] + G =
      Lexical.walk[okay.State % S, S, A, G](s0)(new TailClauses[okay.State % S, S]:
        def op[X](e: okay.State[S, X], s: S): (S, X) = e match
          case okay.State.Get() => (s, s)
          case okay.State.Set(s1) => (s1, s1)
      )(body)

    extension [S, G[+_]](i: Inst[okay.State % S, G])
      def get: S ! G = i.perform(okay.State.Get[S, S]())
      def set(s: S): S ! G = i.perform(okay.State.Set[S, S](s))
      /** `set` as a STATEMENT: the same operation, answering `Unit`, so a
       * marked `s.put(v).?` on its own line leaves nothing unused (what
       * `tell` is for Writer) */
      def put(s: S): Unit ! G = i.perform(okay.State.Set[S, S](s)).map(_ => ())

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
    final class Deep[F[+_], R, G[+_], S <: Tuple] private[Lexical] (p0: Prompt[R], ops: Ops[F, R, Delim + G])
        extends In[R, S](p0):
      /** the operation, to THIS installation, which must be on the stack */
      def perform[X](e: F[X])(using st: Stack[?])[B <: Tuple](using Has.Aux[st.S, p.type, B], At): Under[G, X, st.S] =
        Delim.Stacked.shift0[R, X, G](p)(k => Prog.diag[B, Delim + G, R](ops.op(e, x => k(x).free)))

    def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, Delim + G])(using st: Stack[?])
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
