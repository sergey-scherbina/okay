package okay.freer

import okay.*


import okay.freer.Row.up

/**
 * HANDLER INSTANCES AS PROMPTS (Biernacki, Piróg, Polesiuk & Sieczkowski, POPL 2020): an installation is a
 * fresh prompt, reached through its instance value, so two instances of one effect are two names.
 * Strategies by name: `deep` (`$` + `shift0`), `tail` (evidence passing, Xie et al. ICFP 2020), `walk`.
 */
object Lexical:

  /** an installed handler of `F` in a program over the row `G`; only an installation makes one */
  abstract class Inst[F[+_], G[+_]] private[Lexical] ():
    /** the operation, to this installation */
    def perform[X](e: F[X]): X ! G

  /** the program type a clause answers in */
  type Unstacked[G[+_]] = [X] =>> X ! G

  /** deep clauses: `k` resumes with the handler re-installed */
  trait Ops[F[+_], R, P[_]]:
    def op[X](e: F[X], k: X => P[R]): P[R]

  /** deep clauses with a return clause */
  trait Clauses[F[+_], A, R, P[_]] extends Ops[F, R, P]:
    def ret(a: A): P[R]

  /** `Shift % ? + G` read as `G` when `G` has `Shift` */
  private def collapse[G[+_]](ev: Shift[?, Any] <:< G[Any]): Row.Sub[Shift % ? + G, G] =
    ev.liftCo[[x] =>> x | G[Any]]

  /** DEEP: `ret $ body`, every operation a `shift0` to the installation (FSCD 2019) */
  def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, Unstacked[G]])(body: Inst[F, G] => A ! G)
                              (using ev: Shift[?, Any] <:< G[Any], at: At): R ! G =
    given Row.Sub[Shift % ? + G, G] = collapse(ev)
    val p = Shift.prompt[R]
    val i = new Inst[F, G]:
      def perform[X](e: F[X]): X ! G =
        Shift.shift0[R, X, G](p)(k => c.op(e, x => k(x).up[G]).up[Shift % ? + G])(using at).up[G]
    Shift.dollar[A, R, G](p)(a => c.ret(a).up[Shift % ? + G])(body(i).up[Shift % ? + G]).up[G]

  /** tail-resumptive clauses: answer in place with the new state */
  trait TailClauses[F[+_], S]:
    def op[X](e: F[X], s: S): (S, X)

  /**
   * how a tail installation is made, by the row: no `Shift`, a cell and a walk; with `Shift`, deep
   * (a capture from outside may run the body twice, which a cell cannot survive)
   */
  sealed trait Closing[G[+_]]:
    def install[F[+_], S, A](s0: S, c: TailClauses[F, S], body: Inst[F, G] => A ! G, at: At): (S, A) ! G

  object Closing:
    given unguarded[G[+_]](using scala.util.NotGiven[Shift[?, Any] <:< G[Any]]): Closing[G] with
      def install[F[+_], S, A](s0: S, c: TailClauses[F, S], body: Inst[F, G] => A ! G, at: At): (S, A) ! G =
        Free.delay { () =>
          var cell = s0
          val i = new Inst[F, G]:
            def perform[X](e: F[X]): X ! G = Free.delay { () =>
              val (s1, x) = c.op(e, cell)
              cell = s1
              okay.freer.pure[G, X](x)
            }
          /** walk the body instead of mapping over it: no rotation per step */
          def walk(x: A ! G): (S, A) ! G = (x.resume: @unchecked) match
            case Free.Return(a) => okay.freer.pure((cell, a))
            case Free.Inject(e) => Free.Inject(e).flatMap(a => okay.freer.pure((cell, a)))
            case Free.Bind(Free.Inject(e), k) => Free.Inject(e).flatMap(y => walk(k(y)))
          walk(body(i))
        }

    given guarded[G[+_]](using ev: Shift[?, Any] <:< G[Any]): Closing[G] with
      def install[F[+_], S, A](s0: S, c: TailClauses[F, S], body: Inst[F, G] => A ! G, at: At): (S, A) ! G =
        type Ans = S => (S, A) ! G
        Lexical.deep[F, A, Ans, G](new Clauses[F, A, Ans, Unstacked[G]]:
          def ret(a: A): Ans ! G = okay.freer.pure[G, Ans](s => okay.freer.pure[G, (S, A)]((s, a)))
          def op[X](e: F[X], k: X => Ans ! G): Ans ! G =
            okay.freer.pure[G, Ans] { s =>
              val (s1, x) = c.op(e, s)
              k(x).flatMap(f => f(s1))
            }
        )(body)(using ev, at).flatMap(f => f(s0))

  /** TAIL: operations answered in place, the state in a cell (deep when the row can capture) */
  def tail[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G)
                              (using closing: Closing[G], at: At): (S, A) ! G =
    closing.install(s0, c, body, at)

  /** WALK: the body walked like a row handler, state threaded, no cell, no capture; never the default */
  def walk[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])
                              (body: Inst[F, Instances.Of[F] + G] => A ! Instances.Of[F] + G)
                              (using TypeableK[Instances.Of[F]]): (S, A) ! Instances.Of[F] + G =
    type R = Instances.Of[F] + G
    Free.delay { () =>
      val handle = Instances.handle("walk")
      val i = new Inst[F, R]:
        def perform[X](e: F[X]): X ! R = Free.Inject(Instances[F, X](handle, e))
      // the walk, one step both faces (handler-one-step): it takes the operations of ITS instance only; another
      // instance's goes on, as the rest of the row does
      def mine: TypeableK[Instances.Of[F]] = new TypeableK[Instances.Of[F]]:
        def test(x: Any): Boolean = summon[TypeableK[Instances.Of[F]]].test(x) && (x.asInstanceOf[Instances[F, Any]].at eq handle)
      val b = body(i)
      HandleFrames.stateRun[Instances.Of[F], S, A, (S, A), R](mine, (s, a) => okay.freer.pure((s, a)))(
        (s, op) => c.op(op.asInstanceOf[Instances[F, Any]].op, s))(s0, b)
    }

  /** the default by clause kind: tail clauses run `tail`, others `deep` */
  def handle[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G)
                                (using Closing[G], At): (S, A) ! G = tail(s0)(c)(body)
  def handle[F[+_], A, R, G[+_]](c: Clauses[F, A, R, Unstacked[G]])(body: Inst[F, G] => A ! G)
                                (using Shift[?, Any] <:< G[Any], At): R ! G = deep(c)(body)

  /** State as instances, every strategy */
  object State:
    /** a deep state handler's answer: state-passing */
    type Ans[S, A, G[+_]] = S => (S, A) ! G

    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.freer.State % S, G] => A ! G)
                         (using Shift[?, Any] <:< G[Any], At): (S, A) ! G =
      Lexical.deep(new Clauses[okay.freer.State % S, A, Ans[S, A, G], Unstacked[G]]:
        def ret(a: A): Ans[S, A, G] ! G = okay.freer.pure((s: S) => okay.freer.pure((s, a)))
        def op[X](e: okay.freer.State[S, X], k: X => Ans[S, A, G] ! G): Ans[S, A, G] ! G = e match
          case okay.freer.State.Get() => okay.freer.pure((s: S) => k(s).flatMap(f => f(s)))
          case okay.freer.State.Update(g) => okay.freer.pure((s: S) => { val (b, s1) = g(s); k(b).flatMap(f => f(s1)) })
      )(body).flatMap(f => f(s0))

    /** the default: tail */
    def apply[S, A, G[+_]](s0: S)(body: Inst[okay.freer.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      tail(s0)(body)

    /** tail, for State */
    def tail[S, A, G[+_]](s0: S)(body: Inst[okay.freer.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      Lexical.tail[okay.freer.State % S, S, A, G](s0)(new TailClauses[okay.freer.State % S, S]:
        def op[X](e: okay.freer.State[S, X], s: S): (S, X) = e match
          case okay.freer.State.Get() => (s, s)
          case okay.freer.State.Update(g) => { val (b, s1) = g(s); (s1, b) }
      )(body)

    /** walk, for State */
    def walk[S, A, G[+_]](s0: S)(body: Inst[okay.freer.State % S, Instances.Of[okay.freer.State % S] + G] => A ! Instances.Of[okay.freer.State % S] + G)
        : (S, A) ! Instances.Of[okay.freer.State % S] + G =
      Lexical.walk[okay.freer.State % S, S, A, G](s0)(new TailClauses[okay.freer.State % S, S]:
        def op[X](e: okay.freer.State[S, X], s: S): (S, X) = e match
          case okay.freer.State.Get() => (s, s)
          case okay.freer.State.Update(g) => { val (b, s1) = g(s); (s1, b) }
      )(body)

    extension [S, G[+_]](i: Inst[okay.freer.State % S, G])
      def get: S ! G = i.perform(okay.freer.State.Get[S, S]())
      def set(s: S): S ! G = i.perform(okay.freer.State.Update[S, S](okay.freer.State.Put(s)))
      /** `modify` */
      def modify(f: S => S): S ! G = i.perform(okay.freer.State.Update[S, S](okay.freer.State.Modified(f)))
      /** `set` as a statement */
      def put(s: S): Unit ! G = i.perform(okay.freer.State.Update[S, Unit](_ => ((), s)))

  /**
   * KEYED INSTANCES (shift-prompt-key): the instance's prompt is its key in the row, `Shift % i.p.type`, so an
   * operation of it outside its installation leaves a key nothing handles and does not compile where it is run.
   * Clauses are typed at the row outside the installation.
   */
  object Stacked:

    /** a keyed deep instance */
    final class Deep[F[+_], R, G[+_]] private[Lexical] (val p: Prompt[R], ops: Ops[F, R, Unstacked[G]]):
      /** the operation, a `shift0` to this instance's prompt */
      def perform[X](e: F[X])(using At): X ! Shift % p.type + G =
        Shift.Stacked.shift0At[R, X, G](p)(k => ops.op(e, k))

    def deep[F[+_], A, R, G[+_]](c: Clauses[F, A, R, Unstacked[G]])(body: (i: Deep[F, R, G]) => A ! Shift % i.p.type + G)
                                (using Shift.Machine[G], At): R ! G =
      val i = new Deep[F, R, G](Shift.prompt[R], c)
      Shift.Stacked.dollarAt[A, R, G](i.p)(c.ret)(body(i))

    /** a keyed tail instance: installed deep, the state threaded */
    final class Tail[F[+_], S0, A, G[+_]] private[Lexical] (val p: Prompt[S0 => (S0, A) ! G], c: TailClauses[F, S0]):
      def perform[X](e: F[X])(using At): X ! Shift % p.type + G =
        Shift.Stacked.shift0At[S0 => (S0, A) ! G, X, G](p)(k =>
          okay.freer.pure[G, S0 => (S0, A) ! G]((s: S0) => {
            val (s1, x) = c.op(e, s)
            k(x).flatMap(f => f(s1))
          }))

    /** `tail`, keyed */
    def tail[F[+_], S0, A, G[+_]](s0: S0)(c: TailClauses[F, S0])(body: (i: Tail[F, S0, A, G]) => A ! Shift % i.p.type + G)
                                 (using Shift.Machine[G], At): (S0, A) ! G =
      val i = new Tail[F, S0, A, G](Shift.prompt[S0 => (S0, A) ! G], c)
      Shift.Stacked.dollarAt[A, S0 => (S0, A) ! G, G](i.p)(a => okay.freer.pure[G, S0 => (S0, A) ! G]((s: S0) => okay.freer.pure[G, (S0, A)]((s, a))))(body(i))
        .flatMap(f => f(s0))
