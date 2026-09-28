package okay.dlm

/**
 * THE MODEL, READ AND CORRECTED — from inside by the conversation,
 * from outside by whoever `Teaching` lets (specs/dlm-learning.md).
 *
 * Every mutating door returns the ledger entry it wrote or the reason
 * it refused, and a refusal is ALSO an entry. What learning may change
 * is the memory fold and nothing else: the intents, the rules, the
 * tables and the router this holds are values it never replaces, so
 * the invariant — learning never creates a class, edits a rule or
 * moves a threshold — is a property of the type, not a discipline.
 *
 * @param router   the router the model decides with; its rules and
 *                 judge are read, never written
 * @param teaching who may teach what
 * @param sink     where entries go beside this value's own history —
 *                 a service's journal
 * @param rules    the fold's gates: since, per person, the sharing bar
 * @param initial  the memory a boot folded from its journal
 * @param now      the clock, for the entries
 * @param encoder  what embeds, for the explanations
 * @param tables   the artifacts served, by hash, for the explanations
 */
final class Governed(val router: Router,
                     teaching: Teaching = Teaching.ours,
                     sink: Ledger.Sink = Ledger.silent,
                     rules: Memory.Rules = Memory.defaults,
                     initial: Memory = Memory.empty,
                     now: () => Long = () => System.currentTimeMillis(),
                     val encoder: String = "",
                     val tables: Map[String, String] = Map.empty):
  import Ledger.Entry
  import Teaching.Channel

  def intents: Intents = router.intents

  @volatile private var held: Memory = initial
  private val history = Ledger.Recorded()
  /** the fold's own rules, with the teachers this `Teaching` names */
  private val folding = rules.copy(teacher = w => rules.teacher(w) || teaching.teacher(w))

  /** the memory as it stands */
  def memory: Memory = held

  // ---- reading -------------------------------------------------------------

  def route(text: String, who: String, lang: Option[String] = None): Route =
    router.route(text, lang, held, who)

  def explain(text: String, who: String, lang: Option[String] = None): Explanation =
    Explanation.of(router, text, who, held, lang, encoder, tables)

  def lessons(who: String): Vector[Lesson] = held.mine.getOrElse(who, Vector.empty)
  def shared: Vector[Lesson] = held.shared

  /** the audit: every entry this value wrote, oldest first */
  def ledger(since: Long = 0L): Vector[Entry] = history.entries.filter(_.at >= since)

  // ---- writing -------------------------------------------------------------

  private def write(e: Entry): Entry =
    history.append(e); sink.append(e); e

  private def refuse(by: String, who: String, what: String, why: String): Either[String, Entry] =
    write(Entry.Refused(who, what, why, now(), by)); Left(why)

  private def maySpeakFor(by: String, who: String): Boolean = by == who || teaching.steward(by)

  /**
   * A LESSON: `who`'s words `earlier` mean `intent`, said `by` — the
   * person themselves, or a steward. Refused, and the refusal
   * recorded, when the channel is off, the class does not exist, the
   * words are not teachable, or the rights are not there.
   */
  def teach(by: String, who: String, earlier: String, intent: String, offset: Long = -1L): Either[String, Entry] =
    val what = s"teach «${earlier.take(48)}» → $intent for $who"
    if !teaching.enabled(Channel.Lesson) then refuse(by, who, what, "learning is off")
    else if intents.byName(intent).isEmpty then refuse(by, who, what, s"no such class: $intent")
    else if !Memory.teachable(earlier, folding.control) then refuse(by, who, what, "not a teachable sentence")
    else if !maySpeakFor(by, who) then refuse(by, who, what, s"$by may not teach for $who")
    else if !teaching.own(who, intent) then refuse(by, who, what, s"$who may not be taught $intent")
    else synchronized {
      val at = now()
      val off = if offset >= 0 then offset else history.entries.length.toLong
      val sharedBefore = held.shared.map(l => (Memory.key(l.text), l.intent)).toSet
      held = Memory.learn(held, who, earlier, intent, off, folding)
      val learned = write(Entry.Learned(who, earlier.trim, intent, off, at, by))
      held.shared.filterNot(l => sharedBefore((Memory.key(l.text), l.intent))).foreach { l =>
        val holders = held.mine.values.flatten.count(p => Memory.key(p.text) == Memory.key(l.text) && p.intent == l.intent)
        write(Entry.Shared(l.text, l.intent, holders, at, by)): Unit
      }
      Right(learned)
    }

  /** A WITHDRAWAL: by the person, or by a steward */
  def forget(by: String, who: String, earlier: String): Either[String, Entry] =
    val what = s"forget «${earlier.take(48)}» for $who"
    if !teaching.enabled(Channel.Withdrawal) then refuse(by, who, what, "withdrawal is off")
    else if !maySpeakFor(by, who) then refuse(by, who, what, s"$by may not forget for $who")
    else synchronized {
      held = Memory.forget(held, who, earlier, folding)
      Right(write(Entry.Forgotten(who, earlier.trim, now(), by)))
    }

  /**
   * SHARING BY A TEACHER: one person's pair made everyone's on the
   * teacher's own word — which the fold does by itself when the
   * teacher teaches it, so this is `teach` in the teacher's name, and
   * a refusal for anybody else.
   */
  def share(by: String, earlier: String, intent: String): Either[String, Entry] =
    if !teaching.teacher(by) then refuse(by, by, s"share «${earlier.take(48)}» → $intent", s"$by is not a teacher")
    else teach(by, by, earlier, intent)

  /** a rebuilt table, recorded: the hash before and after and where it came from */
  def rebuilt(by: String, artifact: String, before: Option[String], after: String, corpus: String): Entry =
    write(Entry.Rebuilt(artifact, encoder, before, after, corpus, now(), by))
