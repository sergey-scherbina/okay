package okay.matching

/**
 * The typed pen for scenario DEFINITIONS (specs/match.md,
 * match-scenarios stage 1). Definitions are built in one program, so
 * the doctrine's in-program half applies: the builder is
 * phantom-indexed by the set of DECLARED states, and a transition
 * naming an undeclared state does not COMPILE. The data form stays
 * primary — wire-loadable, validated at runtime; this is the safe
 * pen for definitions written in code.
 *
 * The phantom is a tuple-encoded set of literal string types: each
 * `state["x"]` prepends "x"; `route` demands evidence (`In`) that
 * its endpoints are members. The membership check is a match type —
 * no macros, no reflection, ~20 lines.
 */
object ScenarioBuilder {

  /** type-level list membership */
  sealed trait In[S <: String & Singleton, L <: Tuple]
  object In:
    given head: [S <: String & Singleton, T <: Tuple] => In[S, S *: T] = new In {}
    given tail: [S <: String & Singleton, H, T <: Tuple] => (In[S, T]) => In[S, H *: T] =
      new In {}

  final class Builder[States <: Tuple, Roles <: Tuple] private[ScenarioBuilder] (
    private[ScenarioBuilder] val name: String,
    private[ScenarioBuilder] val roles: Vector[String],
    private[ScenarioBuilder] val states: Vector[String],
    private[ScenarioBuilder] val terminal: Set[String],
    private[ScenarioBuilder] val transitions: Vector[Transition]):

    def role[R <: String & Singleton](r: R): Builder[States, R *: Roles] =
      new Builder(name, roles :+ r, states, terminal, transitions)

    def state[S <: String & Singleton](s: S): Builder[S *: States, Roles] =
      new Builder(name, roles, states :+ s, terminal, transitions)

    def terminalState[S <: String & Singleton](s: S): Builder[S *: States, Roles] =
      new Builder(name, roles, states :+ s, terminal + s, transitions)

    /** the typed route: endpoints and the role must be DECLARED */
    def route[F <: String & Singleton, T <: String & Singleton,
              R <: String & Singleton](
      tname: String, from: F, to: T, by: R,
      unlocks: Vector[(String, String)] = Vector.empty,
      notifies: Vector[(String, String)] = Vector.empty)(
      using In[F, States], In[T, States], In[R, Roles])
    : Builder[States, Roles] =
      new Builder(name, roles, states, terminal,
        transitions :+ Transition(tname, from, to, by, unlocks, notifies))

    /** close: the initial state must be declared too */
    def initial[S <: String & Singleton](s: S)(using In[S, States]): ScenarioDef =
      ScenarioDef(name, roles.reverse.reverse, s, states, terminal, transitions)

  def scenario(name: String): Builder[EmptyTuple, EmptyTuple] =
    new Builder(name, Vector.empty, Vector.empty, Set.empty, Vector.empty)
}
