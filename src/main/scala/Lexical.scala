package okay

import okay.Row.up

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

  /** the program type a clause answers in: unstacked here, `Stacked.Below` on the stacked road */
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
              okay.pure[G, X](x)
            }
          /** walk the body instead of mapping over it: no rotation per step */
          def walk(x: A ! G): (S, A) ! G = (x.resume: @unchecked) match
            case Free.Return(a) => okay.pure((cell, a))
            case Free.Inject(e) => Free.Inject(e).flatMap(a => okay.pure((cell, a)))
            case Free.Bind(Free.Inject(e), k) => Free.Inject(e).flatMap(y => walk(k(y)))
          walk(body(i))
        }

    given guarded[G[+_]](using ev: Shift[?, Any] <:< G[Any]): Closing[G] with
      def install[F[+_], S, A](s0: S, c: TailClauses[F, S], body: Inst[F, G] => A ! G, at: At): (S, A) ! G =
        type Ans = S => (S, A) ! G
        Lexical.deep[F, A, Ans, G](new Clauses[F, A, Ans, Unstacked[G]]:
          def ret(a: A): Ans ! G = okay.pure[G, Ans](s => okay.pure[G, (S, A)]((s, a)))
          def op[X](e: F[X], k: X => Ans ! G): Ans ! G =
            okay.pure[G, Ans] { s =>
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
      // a resumption from a forwarded operation re-enters here
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

  /** the default by clause kind: tail clauses run `tail`, others `deep` */
  def handle[F[+_], S, A, G[+_]](s0: S)(c: TailClauses[F, S])(body: Inst[F, G] => A ! G)
                                (using Closing[G], At): (S, A) ! G = tail(s0)(c)(body)
  def handle[F[+_], A, R, G[+_]](c: Clauses[F, A, R, Unstacked[G]])(body: Inst[F, G] => A ! G)
                                (using Shift[?, Any] <:< G[Any], At): R ! G = deep(c)(body)

  /** State as instances, every strategy */
  object State:
    /** a deep state handler's answer: state-passing */
    type Ans[S, A, G[+_]] = S => (S, A) ! G

    def deep[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)
                         (using Shift[?, Any] <:< G[Any], At): (S, A) ! G =
      Lexical.deep(new Clauses[okay.State % S, A, Ans[S, A, G], Unstacked[G]]:
        def ret(a: A): Ans[S, A, G] ! G = okay.pure((s: S) => okay.pure((s, a)))
        def op[X](e: okay.State[S, X], k: X => Ans[S, A, G] ! G): Ans[S, A, G] ! G = e match
          case okay.State.Get() => okay.pure((s: S) => k(s).flatMap(f => f(s)))
          case okay.State.Update(g) => okay.pure((s: S) => { val (b, s1) = g(s); k(b).flatMap(f => f(s1)) })
      )(body).flatMap(f => f(s0))

    /** the default: tail */
    def apply[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      tail(s0)(body)

    /** tail, for State */
    def tail[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, G] => A ! G)(using Closing[G], At): (S, A) ! G =
      Lexical.tail[okay.State % S, S, A, G](s0)(new TailClauses[okay.State % S, S]:
        def op[X](e: okay.State[S, X], s: S): (S, X) = e match
          case okay.State.Get() => (s, s)
          case okay.State.Update(g) => { val (b, s1) = g(s); (s1, b) }
      )(body)

    /** walk, for State */
    def walk[S, A, G[+_]](s0: S)(body: Inst[okay.State % S, Instances.Of[okay.State % S] + G] => A ! Instances.Of[okay.State % S] + G)
        : (S, A) ! Instances.Of[okay.State % S] + G =
      Lexical.walk[okay.State % S, S, A, G](s0)(new TailClauses[okay.State % S, S]:
        def op[X](e: okay.State[S, X], s: S): (S, X) = e match
          case okay.State.Get() => (s, s)
          case okay.State.Update(g) => { val (b, s1) = g(s); (s1, b) }
      )(body)

    extension [S, G[+_]](i: Inst[okay.State % S, G])
      def get: S ! G = i.perform(okay.State.Get[S, S]())
      def set(s: S): S ! G = i.perform(okay.State.Update[S, S](okay.State.Put(s)))
      /** `modify` */
      def modify(f: S => S): S ! G = i.perform(okay.State.Update[S, S](okay.State.Modified(f)))
      /** `set` as a statement */
      def put(s: S): Unit ! G = i.perform(okay.State.Update[S, Unit](_ => ((), s)))

  /**
   * STACKED INSTANCES: the instance is its delimiter on the typed stack, so using it outside its
   * installation does not compile; clauses are typed at the stack below it.
   */
  object Stacked:
    import Shift.Stacked.{In, Stack, Under, Has}

    /** the program type of stacked clauses */
    type Below[G[+_], St <: Tuple] = [X] =>> Under[G, X, St]

    /** a stacked deep instance */
    final class Deep[F[+_], R, G[+_], S <: Tuple] private[Lexical] (p0: Prompt[R], ops: Ops[F, R, Below[G, S]])
        extends In[R, S](p0):
      /** the operation; `Has` proves the instance is on the stack with `S` below */
      def perform[X](e: F[X])(using st: Stack[?])(using Has.Aux[st.S, p.type, S], At): Under[G, X, st.S] =
        Shift.Stacked.shift0[R, X, G](p)(using st)(k => ops.op(e, k))

    def deep[F[+_], A, R, G[+_]](using st: Stack[?])(c: Clauses[F, A, R, Below[G, st.S]])
                                (body: (i: Deep[F, R, G, st.S]) => Under[G, A, i.p.type *: st.S])
                                (using at: At): Under[G, R, st.S] =
      val i = new Deep[F, R, G, st.S](Shift.prompt[R], c)
      Delimited.machine[Freer.Lift[G]].dollar[R, A, st.S, st.S](Cont0.delimiter(i.p))(c.ret)(Shift.Stacked.rebase(body(i)))

    /** a stacked tail instance: installed deep, the state threaded */
    final class Tail[F[+_], S0, A, G[+_], S <: Tuple] private[Lexical] (
        p0: Prompt[S0 => Under[G, (S0, A), S]], c: TailClauses[F, S0]) extends In[S0 => Under[G, (S0, A), S], S](p0):
      def perform[X](e: F[X])(using st: Stack[?])(using Has.Aux[st.S, p.type, S], At): Under[G, X, st.S] =
        Shift.Stacked.shift0[S0 => Under[G, (S0, A), S], X, G](p)(using st)(k =>
          Freer.Return((s: S0) => {
            val (s1, x) = c.op(e, s)
            k(x).flatMap(f => f(s1))
          }))

    /** `tail`, stacked */
    def tail[F[+_], S0, A, G[+_]](s0: S0)(c: TailClauses[F, S0])(using st: Stack[?])
                                 (body: (i: Tail[F, S0, A, G, st.S]) => Under[G, A, i.p.type *: st.S])
                                 (using at: At): Under[G, (S0, A), st.S] =
      val i = new Tail[F, S0, A, G, st.S](Shift.prompt[S0 => Under[G, (S0, A), st.S]], c)
      Delimited.machine[Freer.Lift[G]].dollar[S0 => Under[G, (S0, A), st.S], A, st.S, st.S](Cont0.delimiter(i.p))(
        a => Freer.Return((s: S0) => Freer.Return((s, a))))(Shift.Stacked.rebase(body(i)))
        .flatMap(f => f(s0))
