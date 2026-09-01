package okay.matching

/**
 * Scenarios as DATA (specs/match.md, match-scenarios): the registry
 * answer, a second time. Which interaction flows a marketplace needs
 * is as unknowable in advance as which attributes — so a scenario is
 * a value, an instance is a row, and the engine is ONE method that
 * checks the role and applies the transition. The deal machine that
 * used to be enums-and-methods is now the built-in `Scenario.deal`
 * definition, running on the same engine as anything anyone defines
 * tomorrow.
 */
final case class Transition(
  name: String,
  from: String, to: String,
  /** the ROLE this transition belongs to — the generalization of
   * "respond is the asked provider's alone" */
  by: String,
  /** (viewer role, attribute): executing this transition lets that
   * role see the other parties' facts of that attribute — the
   * generalization of the contacts() unlock */
  unlocks: Vector[(String, String)] = Vector.empty,
  /** (role, template): {scenario}/{state}/{by}/{what} holes */
  notifies: Vector[(String, String)] = Vector.empty)

final case class ScenarioDef(
  name: String,
  roles: Vector[String],
  initial: String,
  states: Vector[String],
  terminal: Set[String],
  transitions: Vector[Transition])

/** a malformation, as data — validate never throws */
final case class BadScenario(where: String, what: String)

object ScenarioDef:
  def validate(d: ScenarioDef): Vector[BadScenario] =
    val states = d.states.toSet
    val roles = d.roles.toSet
    val badRefs = d.transitions.flatMap { t =>
      Vector(
        Option.unless(states(t.from))(BadScenario(t.name, s"unknown state '${t.from}'")),
        Option.unless(states(t.to))(BadScenario(t.name, s"unknown state '${t.to}'")),
        Option.unless(roles(t.by))(BadScenario(t.name, s"unknown role '${t.by}'")))
        .flatten ++
        t.unlocks.collect { case (r, _) if !roles(r) =>
          BadScenario(t.name, s"unlock names unknown role '$r'") } ++
        t.notifies.collect { case (r, _) if !roles(r) =>
          BadScenario(t.name, s"notify names unknown role '$r'") }
    }
    val exits = d.transitions.groupBy(_.from)
    val terminalExits = d.terminal.toVector.collect {
      case s if exits.contains(s) => BadScenario(s, "a terminal state with exits")
    }
    // reachability by walking transitions from the initial state
    val reached = LazyList.iterate(Set(d.initial)) { seen =>
      seen ++ d.transitions.filter(t => seen(t.from)).map(_.to)
    }.sliding(2).dropWhile { case Seq(a, b) => a != b; case _ => false }
      .next().head
    val unreachable = d.terminal.toVector.collect {
      case s if !reached(s) => BadScenario(s, "an unreachable terminal")
    }
    val noInit = Option.unless(states(d.initial))(
      BadScenario(d.name, s"unknown initial state '${d.initial}'")).toVector
    badRefs ++ terminalExits ++ unreachable ++ noInit

  /** the deal machine, re-expressed as the built-in definition — the
   * proof that yesterday's hardcode is today's data */
  val deal: ScenarioDef = ScenarioDef(
    name = "deal",
    roles = Vector("seeker", "provider"),
    initial = "asked",
    states = Vector("asked", "accepted", "declined", "withdrawn"),
    terminal = Set("accepted", "declined", "withdrawn"),
    transitions = Vector(
      Transition("accept", "asked", "accepted", by = "provider",
        unlocks = Vector("seeker" -> "contact", "provider" -> "contact"),
        notifies = Vector("seeker" -> "исполнитель согласился: {what}")),
      Transition("decline", "asked", "declined", by = "provider",
        notifies = Vector("seeker" -> "кандидат отказался: {what}")),
      Transition("withdraw", "asked", "withdrawn", by = "seeker",
        notifies = Vector("provider" -> "отбой по заказу: {what}"))))

final case class FlowId(n: Long)

/** the instance: who plays which role, where the machine stands, and
 * everything that happened — append-only */
final case class Flow(id: FlowId, scenario: String,
                      parties: Map[String, ProfileId],
                      what: String,
                      state: String,
                      history: Vector[(String, ProfileId, Long)])

/** a refusal, as data */
final case class NoAdvance(reason: String)

object Flow:
  /** the ONE engine step: the transition must exit the current
   * state, and `by` must hold its role */
  def advance(d: ScenarioDef, f: Flow, transition: String, by: ProfileId,
              now: Long): Either[NoAdvance, (Flow, Transition)] =
    d.transitions.find(t => t.name == transition && t.from == f.state) match
      case None => Left(NoAdvance(s"no transition '$transition' from '${f.state}'"))
      case Some(t) =>
        if !f.parties.get(t.by).contains(by) then
          Left(NoAdvance(s"'$transition' belongs to role '${t.by}'"))
        else Right((f.copy(state = t.to,
          history = f.history :+ (transition, by, now)), t))

  def fill(template: String, d: ScenarioDef, f: Flow, by: String): String =
    template.replace("{scenario}", d.name).replace("{state}", f.state)
      .replace("{by}", by).replace("{what}", f.what)
