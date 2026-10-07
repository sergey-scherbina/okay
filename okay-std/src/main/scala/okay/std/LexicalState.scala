package okay.std

import okay.freer.*

/** State as `Lexical` instances, every strategy (was `Lexical.State`, moved to okay-std with State) */
object LexicalState:
  /** a deep state handler's answer: state-passing */
  type Ans[S, A, G[+_]] = S => (S, A) ! G

  def deep[S, A, G[+_]](s0: S)(body: Lexical.Inst[State % S, G] => A ! G)
                       (using Shift[?, Any] <:< G[Any], At): (S, A) ! G =
    Lexical.deep(new Lexical.Clauses[State % S, A, Ans[S, A, G], Lexical.Unstacked[G]]:
      def ret(a: A): Ans[S, A, G] ! G = pure((s: S) => pure((s, a)))
      def op[X](e: State[S, X], k: X => Ans[S, A, G] ! G): Ans[S, A, G] ! G = e match
        case State.Get() => pure((s: S) => k(s).flatMap(f => f(s)))
        case State.Update(g) => pure((s: S) => { val (b, s1) = g(s); k(b).flatMap(f => f(s1)) })
    )(body).flatMap(f => f(s0))

  /** the default: tail */
  def apply[S, A, G[+_]](s0: S)(body: Lexical.Inst[State % S, G] => A ! G)(using Lexical.Closing[G], At): (S, A) ! G =
    tail(s0)(body)

  /** tail, for State */
  def tail[S, A, G[+_]](s0: S)(body: Lexical.Inst[State % S, G] => A ! G)(using Lexical.Closing[G], At): (S, A) ! G =
    Lexical.tail[State % S, S, A, G](s0)(new Lexical.TailClauses[State % S, S]:
      def op[X](e: State[S, X], s: S): (S, X) = e match
        case State.Get() => (s, s)
        case State.Update(g) => { val (b, s1) = g(s); (s1, b) }
    )(body)

  /** walk, for State */
  def walk[S, A, G[+_]](s0: S)(body: Lexical.Inst[State % S, Instances.Of[State % S] + G] => A ! Instances.Of[State % S] + G)
      : (S, A) ! Instances.Of[State % S] + G =
    Lexical.walk[State % S, S, A, G](s0)(new Lexical.TailClauses[State % S, S]:
      def op[X](e: State[S, X], s: S): (S, X) = e match
        case State.Get() => (s, s)
        case State.Update(g) => { val (b, s1) = g(s); (s1, b) }
    )(body)

  extension [S, G[+_]](i: Lexical.Inst[State % S, G])
    def get: S ! G = i.perform(State.Get[S, S]())
    def set(s: S): S ! G = i.perform(State.Update[S, S](State.Put(s)))
    /** `modify` */
    def modify(f: S => S): S ! G = i.perform(State.Update[S, S](State.Modified(f)))
    /** `set` as a statement */
    def put(s: S): Unit ! G = i.perform(State.Update[S, Unit](_ => ((), s)))
