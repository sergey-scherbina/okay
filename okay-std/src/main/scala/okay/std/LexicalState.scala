package okay.std

import okay.{!, %, Handler, Lexical, Pure, pure}

/** State as `Lexical` instances (docs/many-instances.md, route 4): each installation a place by key on the kernel,
 * every strategy one installation of a handler form */
object LexicalState:
  /** a deep state handler's answer: state-passing */
  type Ans[S, A] = S => (S, A) ! Pure

  /** DEEP: the kernel's `Deep`, the state passed through the answers — the rest `k`, resumable any number of times */
  def deep[S, A, Es[_]](s0: S)(body: Lexical.Inst[State % S] => A ! Es): (S, A) ! Es =
    Lexical.handle[State % S, [a] =>> Ans[S, a], A, Es](Handler.deep[State % S, [a] =>> Ans[S, a]](
      [a] => (x: a) => (s: S) => pure[(S, a), Pure]((s, x)))(
      [a, X] => (e: State[S, X], k: X => Ans[S, a] ! Pure) => e match
        case State.Get() => pure[Ans[S, a], Pure]((s: S) => k(s).flatMap(f => f(s)))
        case State.Update(g) => pure[Ans[S, a], Pure]((s: S) => { val (b, s1) = g(s); k(b).flatMap(f => f(s1)) })
    ))(body).flatMap(f => f(s0).at[Es])

  /** the default: tail */
  def apply[S, A, Es[_]](s0: S)(body: Lexical.Inst[State % S] => A ! Es): (S, A) ! Es = tail(s0)(body)

  /** TAIL: State's own handler (`State(s0)`, the kernel's `Stateful`), each operation answered in place — a register
   * of the machine, so a `k` taken past it resumes with the state as it was then: across a multi-shot capture it
   * answers as `deep` does */
  def tail[S, A, Es[_]](s0: S)(body: Lexical.Inst[State % S] => A ! Es): (S, A) ! Es =
    Lexical.handle(State(s0))(body)

  extension [S](i: Lexical.Inst[State % S])
    def get: S ! Pure = i.perform(State.Get[S, S]())
    def set(s: S): S ! Pure = i.perform(State.Update[S, S](State.Put(s)))
    /** `modify` */
    def modify(f: S => S): S ! Pure = i.perform(State.Update[S, S](State.Modified(f)))
    /** `set` as a statement */
    def put(s: S): Unit ! Pure = i.perform(State.Update[S, Unit](_ => ((), s)))
