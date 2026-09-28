package okay.dlm

import okay.codec.Json
import okay.codec.Json.*

/**
 * WHAT THE CALLER HAS JUST PUT IN FRONT OF THIS PERSON, AND NOT YET
 * TAKEN BACK (specs/dlm.md, "The context, as a table").
 *
 * A context-reading — «добавь» after «Добавить?» is a yes, «2» after
 * a numbered list is a choice — is a defect of reading THIS, not of
 * the router. Naming each thing that can stand is what lets a caller
 * say "every offer names a verb the confirm reader reads" as a fact
 * about the TYPE, checked once, rather than as seven bugs found one at
 * a time.
 */
enum Standing:
  /** a numbered list is on the screen; `kind` says whose reader owns
   * the number */
  case Shown(kind: String)
  /** a yes/no question that NAMES the act it invites */
  case Offered(act: String)
  /** a field asked for outside any intake */
  case Asked(field: String)
  /** their own last search, standing to be corrected, repeated or
   * followed up rather than started over */
  case Searched(what: String)

object Standing:
  /**
   * A contextual reading, named — so a report can count how often the
   * fifth row fired without re-deriving what the fifth row is from the
   * code, and so a row nobody meets is deleted by a number and not by
   * a guess. Every row fires as `Route.Fires(intent, _, Exact(rule))`,
   * and `(intent, rule)` together must be unique across the table.
   */
  final case class Row(name: String, intent: String, rule: Option[String])

  /** the rows whose `(intent, rule)` pair another row already claims —
   * asserted empty by a caller's suite, because a row added later
   * collides silently */
  def collisions(rows: Vector[Row]): Vector[(Row, Row)] =
    for
      (a, i) <- rows.zipWithIndex
      b <- rows.drop(i + 1) if a.intent == b.intent && a.rule == b.rule
    yield (a, b)

/** a question of ours is outstanding, of one of two kinds */
enum Pending:
  /** an intake is running and waits for this field — `None` before
   * its first question is asked */
  case Field(name: Option[String])
  /** a yes/no question of ours, of the caller's own kind */
  case Answer(kind: String)

/**
 * The state of one turn, as ONE value: exactly what the DECISION
 * reads and nothing else. A snapshot, taken before anything else the
 * turn does; the caller's durable state stays where it lives.
 *
 * @param who      the person a turn belongs to
 * @param lang     the language of THIS exchange, already resolved:
 *                 pinned to the person, never re-derived per message
 * @param pending  a question of ours outstanding, if one is
 * @param stuck    how many turns in a row went unclear. The menu is
 *                 for somebody stuck, and one unclear turn is not
 * @param standing what the caller has just put in front of this
 *                 person — WHICH of its things currently hold anything
 *                 for them, not their content
 */
final case class State(who: String,
                       lang: String,
                       pending: Option[Pending] = None,
                       stuck: Int = 0,
                       standing: Vector[Standing] = Vector.empty):
  /** a question of ours is outstanding, of either kind. What comes
   * next answers it rather than starting something */
  def waiting: Boolean = pending.isDefined
  /** the field an intake is waiting for, when it is waiting */
  def asked: Option[String] = pending.collect { case Pending.Field(n) => n }.flatten

/**
 * What a turn does, decided as a pure function (specs/dlm.md, "The
 * decision").
 *
 * The composition — what happens when the deterministic layers come
 * back empty, and what happens when two of them disagree — is the
 * property the whole model rests on, and in the implementation this
 * was lifted from it lived as nested conditions inside one method.
 * Every subtle defect there was a defect of composition: a courtesy
 * answered with the menu, «Yes please» not read as a yes, a stale
 * question swallowing the next message.
 *
 * The organising idea is SCOPE and PHASE: a rule lives in the
 * narrowest scope where it can be true, and a scope is consulted only
 * while it is active.
 */
object Decision:

  /**
   * What the decision is allowed to look at.
   *
   * A trait rather than a record because two of these are EXPENSIVE
   * and are computed only on the branch that needs them: the act head
   * is asked once inside an intake and once, at a different bar, for
   * a courtesy. A caller implements them lazily; a test with
   * constants.
   */
  trait Evidence:
    /** what the router said — `None` when a question of ours is
     * outstanding, because then the router was never asked */
    def route: Option[Route]
    /** what the router READ of an answer to our own question — for the
     * record only, never acted on. `None` outside an intake */
    def heard: Option[Route] = None
    /** the act head inside an intake: `None` is its abstention, which
     * means "an answer" */
    def pendingAct: Option[String]
    /** the act head at the courtesy bar, outside an intake */
    def courtesy: Boolean
    /** the act head saying "correct" with NOTHING pending: «ты меня не
     * понял», said about what the caller believes rather than about a
     * field of a form */
    def correcting: Boolean = false
    /** whether a model door exists at all */
    def hasModel: Boolean
    /** is there an authored question that separates exactly these two
     * candidates, in this person's language? A pair the domain knows
     * by itself answers true without one */
    def distinguishable(pair: Vector[String]): Boolean = false
    /** EVERYTHING the router noticed, winner first — so a second fact
     * in the message is seen even while the decision acts on the first */
    def noticed: Vector[(String, Support)] = Vector.empty
    /** a plain yes or no, read directly off the text — never inferred
     * from an intent */
    def polarity: Option[Boolean] = None
    /** an exact command, which outranks everything else in the message */
    def command: Option[String] = None

  /**
   * What the caller does about this turn. The payloads are what the
   * performing half needs and no more.
   */
  enum Action:
    /** a question of ours is outstanding and this turn answers it.
     * Carries the act head's verdict, which goes in the log beside the
     * text so a later reader can tell "we diverted this" from "we kept
     * it and could not parse it" */
    case AnswerPending(act: Option[String])
    /** the router understood: record the verdict and act on it */
    case Act(route: Route)
    /** a courtesy outside an intake. Not a failure to understand, and
     * not counted towards being stuck: the person is done, not lost */
    case Acknowledge
    /** the first unclear turn, with no model configured: ask plainly */
    case AskPlainly
    /** the first unclear turn, with a model: its one attempt */
    case AskModel
    /** the person says we got it wrong and no question of ours is
     * outstanding: show what IS recorded and ask what to fix */
    case ShowRecord
    /** the second unclear turn: the menu, once */
    case Menu
    /** the third and after: shorter and different, because repeating a
     * list somebody has just read is insisting rather than helping */
    case Shorter
    /** two candidates and something to ask that separates them */
    case Distinguish(candidates: Vector[String])

  /** the action's name, as it travels in a log: stable, because a
   * record outlives the code that wrote it */
  def name(a: Action): String = a match
    case Action.AnswerPending(_) => "answer"
    case Action.Act(_) => "act"
    case Action.Acknowledge => "acknowledge"
    case Action.ShowRecord => "record"
    case Action.AskPlainly => "ask"
    case Action.AskModel => "model"
    case Action.Menu => "menu"
    case Action.Shorter => "shorter"
    case Action.Distinguish(_) => "distinguish"

  /** does this action mean the caller failed to understand — the one
   * question the stuck counter asks. `ShowRecord` is an ANSWER, so it
   * does not climb the ladder, exactly as a pleasantry does not */
  def unclear(a: Action): Boolean = a match
    case Action.AskPlainly | Action.AskModel | Action.Menu | Action.Shorter |
         // a narrowing question is still a turn we did not understand:
         // if the answer to it is unclear too, the ladder must go on
         Action.Distinguish(_) => true
    case _ => false

  /** the intents a turn carried beyond the one acted on — the second
   * facts, which are otherwise dropped without a trace */
  def dropped(e: Evidence): Vector[String] =
    val acted = e.route.flatMap(_.named)
    e.noticed.map(_._1).filterNot(acted.contains).distinct

  /**
   * WHAT GETS WRITTEN DOWN about a turn, as a function of the action.
   *
   * Which fields a record carries is behaviour and not bookkeeping: a
   * log that says only "an answer arrived" cannot tell a later reader
   * whether the caller diverted it or failed to parse it, and those
   * two want opposite fixes. Making it a function of the action keeps
   * the decision and its record from ever disagreeing.
   *
   * @param did     the action's name
   * @param verdict the route a replay resumes from — an `Unclear` IS a
   *                verdict, and not writing it down is how every
   *                message nobody understood gets filed as the intake
   *                working
   * @param routed  what the router read of an answer to our own
   *                question: a diagnosis reads it and nothing replays it
   * @param act     the act head's verdict, beside the text
   * @param asks    the field an answer answered
   * @param also    the second facts the message carried
   */
  final case class Record(did: String,
                          verdict: Option[Route] = None,
                          routed: Option[Route] = None,
                          act: Option[String] = None,
                          asks: Option[String] = None,
                          also: Vector[String] = Vector.empty)

  object Record:
    def encode(r: Record): Json = JObj(
      Vector("did" -> JStr(r.did)) ++
        r.verdict.map(v => "verdict" -> Route.encode(v)) ++
        r.routed.map(v => "routed" -> Route.encode(v)) ++
        r.act.map(a => "act" -> JStr(a)) ++
        r.asks.map(a => "asks" -> JStr(a)) ++
        Option.when(r.also.nonEmpty)("also" -> JArr(r.also.map(JStr(_)))))

    def decode(j: Json): Option[Record] = j match
      case JObj(fs) =>
        fs.collectFirst { case ("did", JStr(d)) => d }.map(did => Record(did,
          fs.collectFirst { case ("verdict", v) => v }.flatMap(Route.decode),
          fs.collectFirst { case ("routed", v) => v }.flatMap(Route.decode),
          fs.collectFirst { case ("act", JStr(a)) => a },
          fs.collectFirst { case ("asks", JStr(a)) => a },
          fs.collectFirst { case ("also", JArr(xs)) => xs.collect { case JStr(s) => s } }.getOrElse(Vector.empty)))
      case _ => None

  def record(a: Action, asked: Option[String], route: Option[Route] = None,
             also: Vector[String] = Vector.empty): Record =
    val base = a match
      // the field it answers travels with it, and what the ROUTER read
      // beside it — not as a verdict, which would make the answer a
      // corpus gap, but as `routed`
      case Action.AnswerPending(act) => Record(name(a), act = act, asks = asked, routed = route)
      // the VERDICT beside the text: a replay that re-routes recorded
      // text rebuilds a different state the day the router changes
      case Action.Act(r) => Record(name(a), verdict = Some(r))
      // a courtesy IS placed — as a courtesy — so it is not a corpus gap
      case Action.Acknowledge => Record(name(a), act = Some("social"))
      // a correction IS placed, and the verdict travels with it because
      // the router did fail to place the text
      case Action.ShowRecord =>
        Record(name(a), act = Some("correct"), verdict = route.collect { case u: Route.Unclear => u })
      case Action.AskPlainly | Action.AskModel | Action.Menu | Action.Shorter | Action.Distinguish(_) =>
        Record(name(a), verdict = route.collect { case u: Route.Unclear => u })
    base.copy(also = also)

  /** the action a record was written by, when the record says. `None`
   * for a record written before actions had names, and that `None` is
   * the honest answer rather than a guess */
  def recall(r: Record): Option[Action] = r.did match
    case "answer" => Some(Action.AnswerPending(r.act))
    case "act" => r.verdict.map(Action.Act(_))
    case "acknowledge" => Some(Action.Acknowledge)
    case "ask" => Some(Action.AskPlainly)
    case "model" => Some(Action.AskModel)
    case "menu" => Some(Action.Menu)
    case "shorter" => Some(Action.Shorter)
    case "record" => Some(Action.ShowRecord)
    case "distinguish" => Some(Action.Distinguish(
      r.verdict.collect { case Route.Unclear(c, _) if c.length == 2 => c }.getOrElse(Vector.empty)))
    case _ => None

  /**
   * The whole composition. Read it as scope, then phase:
   *
   *   FIELD/FRAME scope — a question of ours is outstanding. Whatever
   *   comes next answers it; only the caller's own exact-command
   *   reading may interrupt, and it runs before this is asked.
   *
   *   SESSION scope — nothing is outstanding, so the router decides.
   *   Exact and typo support acts; semantic support acts on margin;
   *   below it nobody guesses.
   *
   *   SESSION fallback — in the order the cost of being wrong
   *   dictates: a courtesy first (cheap and certain), a correction
   *   next, a narrowing question where one exists, then one plain
   *   question, then the menu, then something shorter. The stuck
   *   count is the state that separates them, and it is read here
   *   rather than incremented: incrementing is performing.
   */
  def decide(st: State, e: Evidence): Action =
    if st.waiting then Action.AnswerPending(e.pendingAct)
    else e.route match
      case None => Action.AskPlainly     // unreachable: a route is asked for when nothing pends
      case Some(r) => r match
        case Route.Unclear(_, _) if e.courtesy => Action.Acknowledge
        case Route.Unclear(_, _) if e.correcting => Action.ShowRecord
        case Route.Unclear(cands, _) if cands.length == 2 && e.distinguishable(cands) =>
          Action.Distinguish(cands)
        case Route.Unclear(_, _) =>
          // the count this turn WILL have once it is recorded
          st.stuck + 1 match
            case 1 => if e.hasModel then Action.AskModel else Action.AskPlainly
            case 2 => Action.Menu
            case _ => Action.Shorter
        case other => Action.Act(other)
