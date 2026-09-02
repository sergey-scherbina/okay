package okay.demo

import okay.*
import okay.given
import okay.Condition
import okay.http.{Body, Http, Request, Response}
import okay.jetty.Jetty
import okay.llm.{Anthropic, Cut, OpenAi, Transport, Transports}
import okay.agent.{Agent, Compact, Handlers, Model as AgentModel, Provider, Tool, Turn, Context as AgentContext}
import okay.matching.{ChatLog, ChatTurn, MatchStore, MemoryMatch, SqlMatch, Tools as MatchTools}
import okay.persist.{FileStore, MemoryStore, Policy}
import okay.jdbc.JdbcSql
import okay.pg.{PgSql, PgTls}
import okay.sql.Placeholders
import okay.tls.{SslMode, TlsConfig}
import okay.conf.{Secret, Secrets}
import okay.crypto.given
import okay.codec.Json
import okay.codec.Json.*
import java.nio.charset.StandardCharsets.UTF_8

/**
 * The chat with an LLM, as a web page (specs/demo-chat.md): one JVM
 * main, no frontend build. okay-jetty streams the reply body live,
 * okay-llm turns the provider's SSE into tokens, Cut guards the
 * stream with a visible budget, and the OFFLINE MODE IS THE DEMO —
 * a scripted model streams a deterministic reply when no key is
 * set, so the application always runs and the tests prove the same
 * path the key exercises.
 *
 *   ANTHROPIC_API_KEY=...  the real model
 *   OKAY_CHAT_PORT=8090    where to listen
 *   OKAY_CHAT_MAX=512      the token budget the Cut enforces
 */
object ChatDemo {

  /** the model seam: history in, token stream out — scripted and
   * live both fit it, which is the whole doctrine */
  type Model = Seq[Anthropic.Message] => Unit ! (Writer % String + Async)

  /** offline: stream a deterministic reply, token by token, the
   * same shape the wire produces */
  def scripted: Model = messages =>
    val last = messages.lastOption.map(_.content).getOrElse("")
    val reply = s"You said: $last — and this reply is streamed token by token " +
      "by the scripted model (set ANTHROPIC_API_KEY for the real one)."
    def go(ts: List[String]): Unit ! (Writer % String + Async) = ts match
      case Nil => pure(())
      case t :: rest =>
        effect[Writer % String + Async, Unit](Writer(t + " ")).flatMap(_ => go(rest))
    go(reply.split(' ').toList)

  /** the demo's config rides the Secrets capability as `env:NAME`
   * references (demo-ctx-wiring): `main` — the process edge —
   * installs Secrets.env, a test installs Secrets.memory, and the
   * model DISPATCH below becomes testable without touching the
   * process environment */
  def secret(name: String)(using s: Secrets): Option[String] =
    s.get(Secret(s"env:$name")).toOption

  /** live: the provider's stream through okay-llm, over the AMBIENT
   * wire — a test wires a canned Transport and runs this very path
   * offline */
  def live(key: String)(using t: Transport): Model = messages =>
    Anthropic.stream(t, key, Anthropic.Request(
      model = "claude-sonnet-4-5", max_tokens = 1024,
      messages = messages.toList, stream = true))

  /** an OpenAI-compatible endpoint (the local rozum model on :8089
   * fits): the same seam, one more filling */
  def local(base: String)(using t: Transport): Model = messages =>
    val body = Json.print(JObj(Vector(
      "model" -> JStr("default"),
      "stream" -> JBool(true),
      "max_tokens" -> JNum(1024),
      "messages" -> JArr(messages.toVector.map(m => JObj(Vector(
        "role" -> JStr(m.role), "content" -> JStr(m.content))))))))
    OpenAi.stream(t, "local", body, s"$base/v1/chat/completions")

  /** which model serves — shown on the page and at startup */
  def modeName(using Secrets): String =
    if secret("ANTHROPIC_API_KEY").isDefined then "live (Anthropic)"
    else secret("OKAY_CHAT_BASE") match
      case Some(base) => s"local ($base)"
      case None => "scripted (no model — set OKAY_CHAT_BASE or ANTHROPIC_API_KEY)"

  def model(using Transport, Secrets): Model =
    secret("ANTHROPIC_API_KEY").map(live)
      .orElse(secret("OKAY_CHAT_BASE").map(local))
      .getOrElse(scripted)

  // ---- the matchmaking side (okay-match wired in) --------------------

  /** ONE marketplace for the whole server, DURABLE by default: a
   * sqlite file (OKAY_CHAT_DB; ":memory:" asks for the memory
   * engine; `postgres://user:pass@host:port/db[?sslmode=…]` puts it
   * on live Postgres over the wire driver) — the open-backend
   * principle is a connection string */
  lazy val market: MatchStore = marketOf(sys.env.getOrElse("OKAY_CHAT_DB", "okay-chat.db"))

  /** the connection string → the engine (pure in its choice, so the
   * live test can drive it without the env) */
  def marketOf(db: String): MatchStore =
    if db == ":memory:" then MemoryMatch()
    else if PgTarget.is(db) then
      // the SAME SqlMatch, its `?` renumbered to pg's `$n` — one env
      // var switches the engine (demo-pg-backend)
      val t = PgTarget.parse(db).fold(e => throw IllegalArgumentException(s"OKAY_CHAT_DB: $e"), identity)
      val conn = t.tls match
        case None => PgSql.connect(t.host, t.port, t.user, t.password, t.database)
        case Some(cfg) => PgTls.connect(t.host, t.port, t.user, t.password, t.database, cfg, Secrets.file)
      SqlMatch(!.run(Async.run[PgSql, Nothing](conn)), placeholders = Placeholders.numbered)
    else
      // under the parallel matrix DriverManager can see another
      // module's loader first; naming the driver removes the race
      // (the TestCrossing/H2 lesson, third telling)
      Class.forName("org.sqlite.JDBC")
      SqlMatch(JdbcSql(java.sql.DriverManager.getConnection(s"jdbc:sqlite:$db")))
  private val turnNo = java.util.concurrent.atomic.AtomicLong(0)

  /** the LOG comes first (demo-replay-projections): every /match turn
   * is appended to a persist topic before anything is extracted, and
   * the offset that comes back is the provenance of what the turn
   * asserts. OKAY_CHAT_LOG is a FileStore directory (":memory:" for
   * a run that keeps nothing); the store above is a projection of it */
  lazy val chatLog: ChatLog = logOf(sys.env.getOrElse("OKAY_CHAT_LOG", "okay-chat.log"))

  def logOf(path: String): ChatLog =
    val store = if path == ":memory:" then MemoryStore() else FileStore.open(java.nio.file.Path.of(path))
    ChatLog(store.topic("web-demo", 1, Policy.default))

  /** a /match turn, LOGGED: register the speaker, append the turn,
   * extract with the log offset as provenance, log the answer too */
  def matchTurnLogged(text: String, history: Seq[Anthropic.Message], log: ChatLog)
                     (using Transport, Secrets, MatchStore): String =
    val store = summon[MatchStore]
    val me = store.register(resolveEmail(text, intakePolicy))
    val off = log.append(ChatTurn(me, "user", text))
    val answer = matchTurn(text, history, off)
    log.append(ChatTurn(me, "assistant", answer))
    answer

  /** log-first made demonstrable: drop the projection, rebuild it
   * from the log through the SAME extraction the live chat used;
   * answers how many user turns it replayed */
  def replayProjections(log: ChatLog)(using Transport, Secrets, MatchStore): Long =
    val store = summon[MatchStore]
    store.reset()
    var n = 0L
    log.replay { (t, prov) =>
      if t.role == "user" then
        matchTurn(t.text, Nil, prov.offset)
        n += 1
    }
    n

  private val matchSystem =
    """You are a helpful chat assistant that ALSO runs a marketplace
      |over a structured database — ANY domain: services and repairs,
      |housing (rent or sale), jobs (seeking work or hiring). Decide
      |yourself when the tools apply: when the user OFFERS something
      |or LOOKS FOR something, work the marketplace; for anything
      |else just answer — no tools needed.
      |Candidates may be several and may decline: list them numbered,
      |let the user CHOOSE whom to ask, then match_inquire each
      |chosen one (asking several is wise — someone agrees). The
      |asked side answers with match_respond; an ACCEPTED deal
      |unlocks contacts (match_contacts) — never reveal contacts
      |before acceptance.
      |The marketplace flow: facts_register (email -> profile id)
      |first — ask for an email if none was given; registry_search
      |BEFORE registry_propose; facts_assert to record an offer or a
      |need (side "offer" or "need", value {"t":"text","s":...},
      |chat "web-demo", span = the user's words) — ALWAYS store a need
      |(side "need") before searching, so the user is notified when
      |a matching offer arrives later; find_candidates to search
      |offers, and report matches with their facts. Answer in
      |the user's language, briefly, and say what you stored or
      |found.""".stripMargin

  // ---- the reverse chain (demo-chat-async) ---------------------------
  //
  // Events arrive in EITHER order: a need stored today matches an
  // offer arriving tomorrow. The chain is structural, not the
  // model's: the tool table is WRAPPED, and every facts_assert of an
  // OFFER runs the reverse search over stored NEEDS (and vice
  // versa); a hit lands in the matched profile's inbox, which the
  // page holds open as an SSE stream (/events). Model-independent:
  // the agent and the deterministic driver go through the same wrap.

  private val lastHits =
    java.util.concurrent.ConcurrentHashMap[String, Vector[okay.matching.ProfileId]]()

  private val inboxes =
    java.util.concurrent.ConcurrentHashMap[String, Channel[String]]()

  /** the open inbox of an email (created on first use) */
  def inbox(email: String): Channel[String] =
    inboxes.computeIfAbsent(email, _ => Channel[String]())

  private def emailOf(store: MatchStore, p: okay.matching.ProfileId): Option[String] =
    store.profileOf(p).map(_.email)

  /** after a stored fact: who was WAITING for it, on the other side? */
  def reverseChain(side: okay.matching.Side, text: String)(using store: MatchStore): Unit =
    import okay.matching.*
    val other = if side == Side.Offer then Side.Need else Side.Offer
    // the floor keeps unrelated waiters quiet; how well related ones
    // score is the embedder seam's business (hashing offline — token
    // overlap; a real embedder in production understands morphology)
    val waiting = store.candidates(Query(other, text = text, k = 5))
      .filter(_.score > 0.1f)
    waiting.foreach { hit =>
      emailOf(store, hit.profile).foreach { email =>
        val what = hit.disclosed.map(f => Value.text(f.value)).mkString("; ")
        val note =
          if side == Side.Offer then s"появился исполнитель: $text (вы искали: $what)"
          else s"появился заказ: $text (вы предлагали: $what)"
        inbox(email).offer(note): Unit
      }
    }

  /** the ROUND policy (store-driven, restart-surviving — deliberately
   * NOT a fiber holding a continuation; see specs/match.md, Deals
   * decision): on an acceptance, the seeker's other Asked deals for
   * the same need are withdrawn and everyone hears the outcome */
  def onResponded(deal: okay.matching.Deal)(using store: MatchStore): Unit =
    import okay.matching.*
    val seekerMail = store.profileOf(deal.seeker).map(_.email)
    val providerMail = store.profileOf(deal.provider).map(_.email)
    deal.state match
      case DealState.Accepted =>
        val contacts = store.contacts(deal.seeker, deal.provider)
          .map(f => Value.text(f.value))
        seekerMail.foreach(m => inbox(m).offer(
          s"исполнитель согласился: ${deal.what}" +
            (if contacts.nonEmpty then s" — контакт: ${contacts.mkString(", ")}" else "")))
        // the rest of the round is withdrawn, each asked party told
        store.dealsFor(deal.seeker)
          .filter(d => d.state == DealState.Asked && d.what == deal.what)
          .foreach { d =>
            store.withdraw(d.id, deal.seeker)
            store.profileOf(d.provider).map(_.email).foreach(m =>
              inbox(m).offer(s"отбой по заказу: ${d.what} — исполнитель уже найден"))
          }
      case DealState.Declined =>
        seekerMail.foreach(m => inbox(m).offer(
          s"кандидат отказался: ${deal.what}" + providerMail.fold("")(pm => s" ($pm)")))
        // all asked declined and none accepted -> say so
        val round = store.dealsFor(deal.seeker).filter(_.what == deal.what)
        if round.nonEmpty && round.forall(d =>
          d.state == DealState.Declined || d.state == DealState.Withdrawn) then
          seekerMail.foreach(m => inbox(m).offer(
            s"все кандидаты отказались: ${deal.what} — запрос остаётся в силе, сообщу о новых"))
      case _ => ()

  // ---- the live market feed (demo-market-live) -----------------------
  //
  // /market subscribes; every market mutation pings every open page.
  // The publish points are the chainedTable wraps below — the model
  // path and the deterministic driver share them, so the feed is
  // model-independent. A closed page's channel stays registered until
  // process end — stated, not hidden: the demo's subscriber count is
  // human-scale.

  private val marketFeed =
    java.util.concurrent.CopyOnWriteArrayList[Channel[String]]()

  /** a new /market subscriber's own channel */
  def marketSub(): Channel[String] =
    val c = Channel[String]()
    marketFeed.add(c)
    c

  /** ring every open /market page: something on the market moved */
  def marketChanged(kind: String): Unit =
    marketFeed.forEach(c => c.offer(kind): Unit)

  // ---- the deal timeline (demo-deal-timeline) -------------------------
  //
  // Deal (okay-match) carries only its CURRENT state — no history.
  // This is the demo layer making the negotiation visible without
  // touching the engine: an append-only per-deal event log, each
  // event carrying the provenance of the turn that caused it (the
  // ChatLog offset threaded through as `off`, the same one
  // facts_assert already gets) — the same story `supersede` tells
  // for facts, told here for deals.

  final case class DealEvent(state: String, by: String, prov: okay.matching.Provenance)

  private val dealEvents =
    java.util.concurrent.ConcurrentHashMap[Long, java.util.concurrent.CopyOnWriteArrayList[DealEvent]]()

  private def dealEvent(deal: Long, e: DealEvent): Unit =
    dealEvents.computeIfAbsent(deal, _ => java.util.concurrent.CopyOnWriteArrayList())
      .add(e): Unit

  /** the current state plus the append-only history, for /deals/<n>;
   * None when the deal id was never asked (the "Asked" event names
   * the seeker, which is how the live Deal is found — the store has
   * no deal-by-id lookup, only dealsFor(profile)) */
  def dealTimeline(deal: Long)(using store: MatchStore): Option[(okay.matching.Deal, Vector[DealEvent])] =
    for
      events <- Option(dealEvents.get(deal))
      es = { import scala.jdk.CollectionConverters.*; events.asScala.toVector }
      asked <- es.find(_.state == "Asked")
      d <- store.dealsFor(okay.matching.ProfileId(asked.by)).find(_.id.n == deal)
    yield (d, es)

  /** the tool table with the reverse chain wrapped around asserts;
   * `off` is the ChatLog offset of the turn driving these calls
   * (default: a fresh counter tick, for callers with no log turn) */
  def chainedTable(off: Long = turnNo.incrementAndGet())
                   (using store: MatchStore): Map[String, okay.agent.ToolCall => String] =
    val base = MatchTools.table(store)
    base.updated("flow_advance", { c =>
      val out = base("flow_advance")(c)
      marketChanged("flow")
      // a successful advance fired a transition: deliver its
      // notifications to the role-holders' inboxes, templates filled
      Json.parse(out) match
        case JObj(fs) if fs.exists(_._1 == "state") =>
          val flowN = c.args match
            case JObj(a) => a.collectFirst { case ("flow", JNum(x)) => x.toLong }.getOrElse(0L)
            case _ => 0L
          for
            f <- store.flow(okay.matching.FlowId(flowN))
            d <- store.scenario(f.scenario)
            (tname, byP, _) <- f.history.lastOption
            t <- d.transitions.find(_.name == tname)
            byRole = f.parties.collectFirst { case (r, p) if p == byP => r }.getOrElse("?")
            (role, template) <- t.notifies
            target <- f.parties.get(role)
            email <- emailOf(store, target)
          do inbox(email).offer(okay.matching.Flow.fill(template, d, f, byRole)): Unit
        case _ => ()
      out
    }).updated("match_inquire", { c =>
      val out = base("match_inquire")(c)
      marketChanged("deal")
      val provider = c.args match
        case JObj(fs) => fs.collectFirst { case ("provider", JStr(x)) => x }.getOrElse("")
        case _ => ""
      val what = c.args match
        case JObj(fs) => fs.collectFirst { case ("what", JStr(x)) => x }.getOrElse("")
        case _ => ""
      val dealN = Json.parse(out) match
        case JObj(fs) => fs.collectFirst { case ("deal", JNum(n)) => n.toLong }.getOrElse(0L)
        case _ => 0L
      val seeker = c.args match
        case JObj(fs) => fs.collectFirst { case ("seeker", JStr(x)) => x }.getOrElse("")
        case _ => ""
      dealEvent(dealN, DealEvent("Asked", seeker, okay.matching.Provenance("web-demo", off, what)))
      emailOf(store, okay.matching.ProfileId(provider)).foreach(m =>
        inbox(m).offer(s"заказ: $what (сделка $dealN) — ответьте: берусь $dealN / отказываюсь $dealN"))
      out
    }).updated("match_respond", { c =>
      val out = base("match_respond")(c)
      marketChanged("deal")
      Json.parse(out) match
        case JObj(fs) =>
          val n = fs.collectFirst { case ("deal", JNum(x)) => x.toLong }.getOrElse(0L)
          store.dealsFor(okay.matching.ProfileId("")) // no-op keeps types honest
          val byId = c.args match
            case JObj(a) => a.collectFirst { case ("by", JStr(x)) => x }.getOrElse("")
            case _ => ""
          // find the deal to hand the policy (dealsFor by the responder)
          val resolved = store.dealsFor(okay.matching.ProfileId(byId)).find(_.id.n == n)
          resolved.foreach { d =>
            dealEvent(n, DealEvent(d.state.toString, byId,
              okay.matching.Provenance("web-demo", off, d.what)))
            onResponded(d)
            // WITHDRAWN stand-downs land as their own event too
            if d.state == okay.matching.DealState.Accepted then
              store.dealsFor(d.seeker)
                .filter(o => o.state == okay.matching.DealState.Withdrawn && o.what == d.what)
                .foreach(o => dealEvent(o.id.n,
                  DealEvent("Withdrawn", "", okay.matching.Provenance("web-demo", off, o.what))))
          }
        case _ => ()
      out
    }).updated("facts_assert", { c =>
      val out = base("facts_assert")(c)
      val args = c.args
      val side = args match
        case JObj(fs) => fs.collectFirst { case ("side", JStr(x)) => x }.getOrElse("offer")
        case _ => "offer"
      val text = args match
        case JObj(fs) => fs.collectFirst { case ("value", JObj(v)) =>
          v.collectFirst { case ("s", JStr(x)) => x }.getOrElse("") }.getOrElse("")
        case _ => ""
      if text.nonEmpty then reverseChain(
        if side == "need" then okay.matching.Side.Need else okay.matching.Side.Offer, text)
      marketChanged("facts")
      out
    })

  /** an agent turn over the match tools: the LLM structures the
   * chat into the store and searches it — the okay-match story */
  def agentTurn(text: String, history: Seq[Anthropic.Message],
                modelH: okay.Handler[AgentModel], off: Option[Long] = None)(using MatchStore): String =
    // the log's offset, when the turn came through the log: the model
    // is TOLD the provenance instead of inventing one
    val system = matchSystem + off.fold("")(n =>
      "\nProvenance for THIS turn: chat \"web-demo\", offset " + n + " — pass exactly these to facts_assert.")
    given okay.Handler[AgentModel] = modelH
    given okay.Handler[Tool] = Handlers.tools(chainedTable(off.getOrElse(turnNo.incrementAndGet())))
    val ctx = Handlers.context(Compact.all)._2
    given okay.Handler[AgentContext] = ctx
    given r1: okay.Handler[AgentModel + Async] = okay.Handler.union[AgentModel, Async]
    given r2: okay.Handler[AgentContext + (AgentModel + Async)] =
      okay.Handler.union[AgentContext, AgentModel + Async]
    given r3: okay.Handler[Tool + (AgentContext + (AgentModel + Async))] =
      okay.Handler.union[Tool, AgentContext + (AgentModel + Async)]
    // the DIRECT block (ui-direct's demo pass): the turn reads as
    // straight-line code — remember the system prompt, seed the
    // history, converse. The seeding loop stays a named helper: v1
    // of the macro refuses marks under a lambda, and a recursive
    // helper is the workaround it names.
    import okay.Direct.*
    def seed(ms: List[Anthropic.Message]): Unit ! okay.agent.Agent = ms match
      case Nil => pure(())
      case m :: rest =>
        Agent.remember(
          if m.role == "user" then Turn.User(m.content)
          else Turn.Assistant(m.content)).flatMap(_ => seed(rest))
    val prog = direct[[A] =>> A ! okay.agent.Agent] {
      Agent.remember(Turn.System(system)).reflect
      seed(history.toList).reflect
      Agent.converse(text, MatchTools.specs).reflect
    }
    prog.runWith

  // ---- the intake's conditions (demo-conditions) ---------------------
  //
  // The silent default was a policy decision nobody made: a phrase
  // with no email quietly became guest@demo. Now it SIGNALS — the
  // condition system's repair road — and the POLICY answers: the
  // lenient demo invokes the "guest" restart (the old behavior, now
  // chosen), a repairing policy can Resume with a corrected address
  // (the signal point is still live), and OKAY_CHAT_STRICT=1 makes
  // it Fail — an Unhandled naming the menu. One intake, three
  // outcomes, chosen at run.

  final case class BadEmail(text: String)
  given Condition.Answers[BadEmail, String] = Condition.Answers.of[BadEmail, String]

  /** the demo's default: lenient — the guest restart */
  val lenient: (Any, Vector[String]) => Condition.Decision =
    case (_: BadEmail, menu) if menu.contains("guest") =>
      Condition.Decision.Invoke("guest", ())
    case _ => Condition.Decision.Fail

  val strict: (Any, Vector[String]) => Condition.Decision =
    (_, _) => Condition.Decision.Fail

  def intakePolicy: (Any, Vector[String]) => Condition.Decision =
    if sys.env.get("OKAY_CHAT_STRICT").contains("1") then strict else lenient

  /** extract the author's email or SIGNAL; the guest frame is on the
   * menu around it */
  def emailIn(text: String): String ! Condition.Op =
    "email ([^ ]+@[^ ]+)".r.findFirstMatchIn(text).map(_.group(1)) match
      case Some(e) => pure(e)
      case None => Condition.raiseC(BadEmail(text))

  def resolveEmail(text: String,
                   policy: (Any, Vector[String]) => Condition.Decision): String =
    Condition.run[String, Pure](policy)(
      Condition.within[String, Pure]("guest")(emailIn(text))(_ => "guest@demo")
    ).runWith

  /** the deterministic offline "agent": the SAME tool table, driven
   * by two fixed phrasings — the tests' and the no-model mode's path */
  /** the demo speaks two languages, picked PER MESSAGE (demo-en-
   * phrasebook): a message carrying no Cyrillic is English. English
   * triggers pair one-for-one with the Russian ones (умею -> can:,
   * спроси -> ask, сценарий -> scenario, шаг -> step, флоу -> flow,
   * берусь/отказываюсь -> accept/decline; помощь/help already
   * paired) — content alone picks which reply template answers */
  def isEnglish(s: String): Boolean = !s.exists(c => c >= 'Ѐ' && c <= 'ӿ')

  private val russianHelp =
    """матч-режим: скажите \"умею <что>\" / \"offer: <что>\" или \"нужен <кто>\" / \"need: <что>\" (и email <адрес>); после списка кандидатов: спроси 1 2 / спроси всех; исполнителю: берусь <N> / отказываюсь <N>; сценарии: сценарий <имя> роль=email ...; шаг <N> <переход>; флоу <N>"""
  private val englishHelp =
    """match mode: say "can: <what>" / "offer: <what>" or "want: <who>" / "need: <what>" (and email <address>); after a candidate list: ask 1 2 / ask all; as a candidate: accept <N> / decline <N>; scenarios: scenario <name> role=email ...; step <N> <transition>; flow <N>"""

  def scriptedAgent(text: String, off: Long = turnNo.incrementAndGet())
                   (using store: MatchStore): String =
    import okay.codec.Json.*
    val t = chainedTable(off)
    val en = isEnglish(text)
    def call(name: String, args: (String, Json)*): String =
      t(name)(okay.agent.ToolCall("d", name, JObj(args.toVector)))
    val email = resolveEmail(text, intakePolicy)
    def profile: String =
      Json.parse(call("facts_register", "email" -> JStr(email))) match
        case JObj(fs) => fs.collectFirst { case ("profile", JStr(p)) => p }.get
        case _ => ""
    text match
      case s if s.contains("умею") || s.contains("can:") || s.contains("offer:") =>
        val skill = s.replaceAll(".*(умею|can:|offer:)\\s*", "").replaceAll("email [^ ]+", "").trim
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("skill"),
          "side" -> JStr("offer"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(skill))))
        // the contact rides as a MATCHED fact: only an accepted deal
        // will show it to anyone — the demo of the second gate
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("contact"),
          "side" -> JStr("offer"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "vis" -> JStr("matched"),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(email))))
        if en then s"""stored offer: "$skill" (profile $email)"""
        else s"""записал предложение: \"$skill\" (профиль $email)"""
      case s if s.contains("нужен") || s.contains("нужно") || s.contains("want:") || s.contains("need:") =>
        val want = s.replaceAll(".*(нужен|нужно|want:|need:)\\s*", "").replaceAll("email [^ ]+", "").trim
        // the need is STORED first — the reverse chain fires from it
        // when the matching offer arrives later
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("need"),
          "side" -> JStr("need"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(want))))
        Json.parse(call("find_candidates", "side" -> JStr("offer"),
          "text" -> JStr(want))) match
          case JArr(hits) if hits.nonEmpty =>
            val ids = hits.flatMap { case JObj(fs) =>
              fs.collectFirst { case ("profile", JStr(x)) => okay.matching.ProfileId(x) }
              case _ => None }
            lastHits.put(email, ids)
            val lines = hits.take(5).zipWithIndex.map {
              case (JObj(fs), i) =>
                val facts = fs.collectFirst { case ("facts", JArr(vs)) => vs }.getOrElse(Vector.empty)
                val texts = facts.collect { case JObj(f) =>
                  f.collectFirst { case ("value", JObj(v)) =>
                    v.collectFirst { case ("s", JStr(x)) => x }.getOrElse("") }.getOrElse("") }
                s"${i + 1}) ${texts.filter(_.nonEmpty).mkString(", ")}"
              case (_, i) => s"${i + 1}) ?"
            }
            if en then s"found ${hits.length}: ${lines.mkString("; ")} — say: ask 1 2 (or: ask all)"
            else s"нашёл ${hits.length}: ${lines.mkString("; ")} — скажите: спроси 1 2 (или: спроси всех)"
          case _ =>
            if en then "nobody yet — I remembered your request and will tell you when one shows up"
            else "пока никого не нашёл — запомнил ваш запрос и сообщу, когда исполнитель появится"
      case s if s.contains("спроси") || s.contains("ask") =>
        val mine = Option(lastHits.get(email)).getOrElse(Vector.empty)
        if mine.isEmpty then
          if en then "ask for what you need first — I'll find candidates"
          else "сначала спросите, что вам нужно — я найду кандидатов"
        else
          val me0 = store.register(email)
          val what = store.profileOf(me0)
            .flatMap(_.current.filter(_.side == okay.matching.Side.Need)
              .lastOption.map(f => okay.matching.Value.text(f.value)))
            .getOrElse("заказ")
          val chosen =
            if s.contains("всех") || s.contains("all") then mine.indices.toVector
            else "\\d+".r.findAllIn(s).map(_.toInt - 1).toVector.filter(mine.indices.contains)
          if chosen.isEmpty then
            if en then "who to ask? name the numbers, or say: ask all"
            else "кого спросить? назовите номера или скажите: спроси всех"
          else
            val me = store.register(email)
            chosen.foreach { i =>
              call("match_inquire", "seeker" -> JStr(me.uuid),
                "provider" -> JStr(mine(i).uuid), "what" -> JStr(what))
            }
            if en then s"asked ${chosen.length} candidates — I'll tell you who takes it"
            else s"спросил ${chosen.length} кандидатов — сообщу, кто возьмётся"
      case s if s.startsWith("сценарий ") || s.startsWith("scenario ") =>
        // сценарий/scenario <имя> роль=email роль=email ... — flow_start
        val body = if s.startsWith("scenario ") then s.stripPrefix("scenario ") else s.stripPrefix("сценарий ")
        val parts = body.split("\\s+").toVector
        val name = parts.headOption.getOrElse("")
        val parties = parts.tail.collect {
          case kv if kv.contains("=") =>
            val i = kv.indexOf('='); kv.take(i) -> kv.drop(i + 1)
        }
        store.scenario(name) match
          case None =>
            if en then s"no such scenario '$name' — registered ones show via scenario_get"
            else s"нет сценария '$name' — зарегистрированные видны через scenario_get"
          case Some(d) =>
            val partyIds = parties.map((r, mail) => r -> JStr(store.register(mail).uuid))
            Json.parse(call("flow_start", "scenario" -> JStr(name),
              "what" -> JStr(name),
              "parties" -> JObj(partyIds))) match
              case JObj(fs) if fs.exists(_._1 == "flow") =>
                val n = fs.collectFirst { case ("flow", JNum(x)) => x.toLong }.get
                if en then s"scenario '$name' started (flow $n, state ${d.initial}); steps: " +
                  d.transitions.map(t => s"${t.name} (${t.by})").mkString(", ") +
                  s" — command: step $n <transition>"
                else s"сценарий '$name' начат (флоу $n, состояние ${d.initial}); шаги: " +
                  d.transitions.map(t => s"${t.name} (${t.by})").mkString(", ") +
                  s" — команда: шаг $n <переход>"
              case JObj(fs) =>
                fs.collectFirst { case ("refused", JStr(r)) => if en then s"refused: $r" else s"отказ: $r" }
                  .getOrElse(if en then "refused" else "отказ")
              case _ => if en then "refused" else "отказ"
      case s if s.startsWith("шаг ") || s.startsWith("step ") =>
        // шаг/step <N> <переход> — flow_advance от лица пишущего
        val body = if s.startsWith("step ") then s.stripPrefix("step ") else s.stripPrefix("шаг ")
        val parts = body.split("\\s+").toVector
        (parts.headOption.flatMap(_.toLongOption), parts.lift(1)) match
          case (Some(n), Some(tr)) =>
            val me = store.register(email)
            Json.parse(call("flow_advance", "flow" -> JNum(n.toDouble),
              "transition" -> JStr(tr), "by" -> JStr(me.uuid))) match
              case JObj(fs) if fs.exists(_._1 == "state") =>
                val st = fs.collectFirst { case ("state", JStr(x)) => x }.get
                if en then s"transition '$tr' done — state: $st"
                else s"переход '$tr' сделан — состояние: $st"
              case JObj(fs) =>
                fs.collectFirst { case ("refused", JStr(r)) => if en then s"refused: $r" else s"отказ: $r" }
                  .getOrElse(if en then "refused" else "отказ")
              case _ => if en then "refused" else "отказ"
          case _ => if en then "format: step <flow number> <transition>" else "формат: шаг <номер флоу> <переход>"
      case s if s.startsWith("флоу ") || s.startsWith("flow ") =>
        val body = if s.startsWith("flow ") then s.stripPrefix("flow ") else s.stripPrefix("флоу ")
        body.trim.toLongOption match
          case None => if en then "format: flow <number>" else "формат: флоу <номер>"
          case Some(n) => store.flow(okay.matching.FlowId(n)) match
            case None => if en then s"no flow $n" else s"нет флоу $n"
            case Some(f) =>
              if en then s"flow $n: scenario ${f.scenario}, state ${f.state}, " +
                s"history: ${f.history.map(_._1).mkString(" -> ")}"
              else s"флоу $n: сценарий ${f.scenario}, состояние ${f.state}, " +
                s"история: ${f.history.map(_._1).mkString(" -> ")}"
      case s if s.startsWith("сценарий ") =>
        // сценарий <имя> роль=email ... — flow_start by phrase
        val parts = s.stripPrefix("сценарий ").split("\\s+").toVector
        val name = parts.headOption.getOrElse("")
        val parties = parts.tail.collect {
          case kv if kv.contains("=") && !kv.startsWith("email") =>
            val i = kv.indexOf('='); kv.take(i) -> kv.drop(i + 1)
        }
        store.scenario(name) match
          case None => s"нет сценария '$name'"
          case Some(d) =>
            val partyIds = parties.map((r, mail) => r -> JStr(store.register(mail).uuid))
            Json.parse(call("flow_start", "scenario" -> JStr(name),
              "what" -> JStr(name), "parties" -> JObj(partyIds))) match
              case JObj(fs) if fs.exists(_._1 == "flow") =>
                val n = fs.collectFirst { case ("flow", JNum(x)) => x.toLong }.get
                s"сценарий '$name' начат (флоу $n, состояние ${d.initial}); шаги: " +
                  d.transitions.map(t => s"${t.name} (${t.by})").mkString(", ") +
                  s"; команда: шаг $n <переход>"
              case JObj(fs) =>
                fs.collectFirst { case ("refused", JStr(r)) => s"отказ: $r" }.getOrElse("отказ")
              case _ => "отказ"
      case s if s.startsWith("шаг ") =>
        // шаг <N> <переход> — flow_advance от лица пишущего
        val parts = s.stripPrefix("шаг ").split("\\s+").toVector
        (parts.headOption.flatMap(_.toLongOption), parts.lift(1)) match
          case (Some(n), Some(tr)) =>
            val me = store.register(email)
            Json.parse(call("flow_advance", "flow" -> JNum(n.toDouble),
              "transition" -> JStr(tr), "by" -> JStr(me.uuid))) match
              case JObj(fs) if fs.exists(_._1 == "state") =>
                s"переход '$tr' сделан — состояние: " +
                  fs.collectFirst { case ("state", JStr(x)) => x }.get
              case JObj(fs) =>
                fs.collectFirst { case ("refused", JStr(r)) => s"отказ: $r" }.getOrElse("отказ")
              case _ => "отказ"
          case _ => "формат: шаг <номер флоу> <переход>"
      case s if s.startsWith("флоу ") =>
        s.stripPrefix("флоу ").trim.split("\\s+").head.toLongOption match
          case None => "формат: флоу <номер>"
          case Some(n) => store.flow(okay.matching.FlowId(n)) match
            case None => s"нет флоу $n"
            case Some(f) =>
              s"флоу $n: сценарий ${f.scenario}, состояние ${f.state}, " +
                s"история: ${f.history.map(_._1).mkString(" -> ")}"
      case s if s.trim == "помощь" || s.trim == "help" =>
        // the default branch IS the help — decided by the TRIGGER
        // word, not `en`: an empty string carries no Cyrillic either
        if s.trim == "help" then englishHelp else russianHelp
      case s if s.contains("берусь") || s.contains("отказываюсь") || s.contains("accept") || s.contains("decline") =>
        val accept = s.contains("берусь") || s.contains("accept")
        "\\d+".r.findFirstIn(s).map(_.toLong) match
          case None =>
            if en then "name the deal number: accept <N> / decline <N>"
            else "назовите номер сделки: берусь <N> / отказываюсь <N>"
          case Some(n) =>
            val me = store.register(email)
            Json.parse(call("match_respond", "deal" -> JNum(n.toDouble),
              "by" -> JStr(me.uuid), "accept" -> JBool(accept))) match
              case JObj(_) =>
                if en then (if accept then "accepted — the seeker got your contact" else "declined")
                else (if accept then "передал согласие — заказчик получил ваш контакт" else "передал отказ")
              case _ =>
                if en then "not your deal, or already closed"
                else "эта сделка не ваша или уже закрыта"
      case _ => if en then englishHelp else russianHelp

  /** which agent serves /match turns: real model when one is
   * configured (by the AMBIENT secrets, over the AMBIENT wire), the
   * deterministic table-driver otherwise */
  def matchTurn(text: String, history: Seq[Anthropic.Message], off: Long)
               (using t: Transport, s: Secrets, m: MatchStore): String =
    secret("ANTHROPIC_API_KEY").map { key =>
      agentTurn(text, history, Provider.anthropic(t, key, "claude-sonnet-4-5"), Some(off))
    }.orElse(secret("OKAY_CHAT_BASE").map { base =>
      agentTurn(text, history, Provider.openAi(
        t, "local", "default", s"$base/v1/chat/completions"), Some(off))
    }).getOrElse(scriptedAgent(text, off))

  // ---- the SSE reply -------------------------------------------------

  private def sse(kind: String, data: String): Chunk[Byte] =
    scala.collection.immutable.ArraySeq.unsafeWrapArray(
      (if kind == "data" then s"data: $data\n\n"
       else s"event: $kind\ndata: $data\n\n").getBytes(UTF_8))

  /** the guarded stream as SSE frames: tokens, then done — or cut */
  def reply(m: Model, budget: Int)(messages: Seq[Anthropic.Message])
  : Source[Chunk[Byte]] =
    val guarded: Either[Cut.Violation, Unit] ! (Writer % String + Async) =
      Cut.guard {
        Cut.checked(m(messages))((i, _) =>
          if i >= budget then Some(Cut.Violation("token-budget", i, s"> $budget tokens"))
          else None)
      }
    Writer.map(guarded)(t => sse("data", Json.print(JStr(t)))).flatMap {
      case Right(_) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("done", "")))
      case Left(v) => effect[Writer % Chunk[Byte] + Async, Unit](
        Writer(sse("cut", Json.print(obj("rule" -> JStr(v.rule), "at" -> JNum(v.at))))))
    }

  private def obj(fs: (String, Json)*): Json = JObj(fs.toVector)

  // ---- the routes ----------------------------------------------------

  private def messagesOf(body: Body): Seq[Anthropic.Message] =
    Json.parse(new String(body.bytes, UTF_8)) match
      case JObj(fs) => fs.collectFirst { case ("messages", JArr(ms)) => ms }
        .getOrElse(Vector.empty).flatMap {
          case JObj(m) =>
            for r <- m.collectFirst { case ("role", JStr(x)) => x }
                c <- m.collectFirst { case ("content", JStr(x)) => x }
            yield Anthropic.Message(r, c)
          case _ => None
        }
      case _ => Vector.empty

  /** the linked React app, if a link has been run (sbt
   * okayChatWebJS/fastLinkJS — the module lives in okay-demo/web);
   * absent, the vanilla page serves */
  def appJs: Option[java.nio.file.Path] =
    sys.env.get("OKAY_CHAT_APP").map(java.nio.file.Path.of(_))
      .filter(java.nio.file.Files.exists(_))
      .orElse {
        val glob = java.nio.file.Path.of("okay-demo/web/.js/target")
        if !java.nio.file.Files.exists(glob) then None
        else
          import scala.jdk.CollectionConverters.*
          java.nio.file.Files.walk(glob).iterator().asScala
            .find(p => p.getFileName.toString == "main.js" &&
              p.toString.contains("fastopt"))
      }

  def routes(m: Model, budget: Int)(using Transport, Secrets, MatchStore)
  : PartialFunction[Request, Response ! Async] =
    case r if r.method == okay.http.Method.Get && r.url == "/" =>
      val html = (if appJs.isDefined then reactPage else page)
        .replace("MODE", modeName)
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(html.getBytes(UTF_8))))
    case r if r.method == okay.http.Method.Get && r.url == "/market" =>
      val store = summon[MatchStore]
      import okay.matching.*
      // rows stay SERVER-RENDERED at load (works without JS, and the
      // gate test keeps reading plain HTML); the script below then
      // re-renders from /market.json on every feed ping
      def rowsOf(side: Side) = store.candidates(Query(side, k = 50)).map { h =>
        val texts = h.disclosed.map(f => Value.text(f.value)).filter(_.nonEmpty)
        s"<li>${texts.mkString(" · ")}</li>"
      }.mkString
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(s"""<!doctype html><meta charset="utf-8"><title>market</title>
          |<style>body{font:15px system-ui;background:#10141a;color:#e6e9ef;padding:2rem}
          |h2{color:#7a869c}
          |#facets button{font-size:.8em;background:#1d2430;border:1px solid #2a3342;color:#9fb0c8;border-radius:.6rem;padding:.3rem .7rem;margin-right:.3rem;cursor:pointer}
          |#facets button.on{background:#3563a8;color:#fff}</style>
          |<div id="facets"></div>
          |<h2>предложения</h2><ul id="offers">${rowsOf(Side.Offer)}</ul>
          |<h2>запросы</h2><ul id="needs">${rowsOf(Side.Need)}</ul>
          |<p><a style="color:#6b9fff" href="/">← в чат</a> · видно только Public — ворота держат и здесь · обновляется вживую</p>
          |<form method="post" action="/admin/replay"><button style="background:#2a3342;color:#e6e9ef;border:0;padding:.5rem .9rem;border-radius:.6rem;cursor:pointer">перестроить из лога</button>
          |<span style="color:#7a869c;font-size:.85em"> — сбросить проекцию и заново вывести её из журнала чатов</span></form>
          |<script>
          |let facet = null;
          |async function render() {
          |  const d = await (await fetch('/market.json')).json();
          |  const attrs = [...new Set([...d.offers, ...d.needs].flatMap(r => r.facts.map(f => f.attr)))];
          |  const fc = document.getElementById('facets'); fc.innerHTML = '';
          |  for (const a of attrs) {
          |    const b = document.createElement('button');
          |    b.textContent = a; if (a === facet) b.className = 'on';
          |    b.onclick = () => { facet = facet === a ? null : a; render(); };
          |    fc.appendChild(b);
          |  }
          |  const fill = (id, rows) => {
          |    const ul = document.getElementById(id); ul.innerHTML = '';
          |    for (const r of rows) {
          |      if (facet && !r.facts.some(f => f.attr === facet)) continue;
          |      const li = document.createElement('li');
          |      li.textContent = r.facts.map(f => f.text).filter(t => t).join(' · ');
          |      ul.appendChild(li);
          |    }
          |  };
          |  fill('offers', d.offers); fill('needs', d.needs);
          |}
          |new EventSource('/events/market').addEventListener('market', render);
          |render();
          |</script>
          |""".stripMargin.getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url == "/market.json" =>
      val store = summon[MatchStore]
      import okay.matching.*
      // the page's data: each disclosed fact with its ATTRIBUTE name —
      // the facet key; `disclosed` is Public-only for an anonymous
      // viewer, so the gate holds on the JSON exactly as on the HTML
      def rows(side: Side) = JArr(store.candidates(Query(side, k = 50)).map { h =>
        JObj(Vector("facts" -> JArr(h.disclosed
          .filter(f => Value.text(f.value).nonEmpty)
          .map(f => JObj(Vector(
            "attr" -> JStr(f.attr), "text" -> JStr(Value.text(f.value))))))))
      })
      pure(Response(200, Seq("content-type" -> "application/json"),
        Http.one(Json.print(JObj(Vector(
          "offers" -> rows(Side.Offer), "needs" -> rows(Side.Need)))).getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url.startsWith("/deals/") && r.url.endsWith(".json") =>
      val n = r.url.stripPrefix("/deals/").stripSuffix(".json").toLongOption.getOrElse(-1L)
      dealTimeline(n) match
        case None => pure(Response(404, Seq("content-type" -> "application/json"),
          Http.one("""{"error":"no such deal"}""".getBytes(UTF_8))))
        case Some((d, events)) =>
          val json = obj(
            "deal" -> JNum(n.toDouble), "what" -> JStr(d.what), "state" -> JStr(d.state.toString),
            "events" -> JArr(events.map(e => obj(
              "state" -> JStr(e.state),
              "chat" -> JStr(e.prov.chat), "offset" -> JNum(e.prov.offset.toDouble),
              "span" -> JStr(e.prov.span)))))
          pure(Response(200, Seq("content-type" -> "application/json"),
            Http.one(Json.print(json).getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url.startsWith("/deals/") =>
      val n = r.url.stripPrefix("/deals/").toLongOption.getOrElse(-1L)
      dealTimeline(n) match
        case None => pure(Response(404, Seq("content-type" -> "text/html; charset=utf-8"),
          Http.one("<!doctype html><meta charset=\"utf-8\"><p>нет такой сделки</p>".getBytes(UTF_8))))
        case Some((d, events)) =>
          val rows = events.map(e =>
            s"<li><b>${e.state}</b> — ${e.prov.span} <span style=\"color:#7a869c;font-size:.85em\">(chat ${e.prov.chat}, offset ${e.prov.offset})</span></li>")
            .mkString
          pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
            Http.one(s"""<!doctype html><meta charset="utf-8"><title>deal $n</title>
              |<style>body{font:15px system-ui;background:#10141a;color:#e6e9ef;padding:2rem}
              |h2{color:#7a869c}</style>
              |<h2>сделка $n: ${d.what} — ${d.state}</h2>
              |<ul>$rows</ul>
              |<p><a style="color:#6b9fff" href="/market">→ /market</a></p>
              |""".stripMargin.getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Post && r.url == "/admin/replay" =>
      // log-first in one click: the projection is dropped and rebuilt
      // from the persist log through the live extraction
      val n = replayProjections(chatLog)
      marketChanged("replay")
      val html = "<!doctype html><meta charset=\"utf-8\"><title>replay</title>" +
        "<style>body{font:15px system-ui;background:#10141a;color:#e6e9ef;padding:2rem}</style>" +
        s"<p>проекция перестроена из журнала: $n ходов</p>" +
        "<p><a style=\"color:#6b9fff\" href=\"/market\">→ /market</a></p>"
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(html.getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Get && r.url == "/app.js" && appJs.isDefined =>
      pure(Response(200, Seq("content-type" -> "text/javascript"),
        Http.one(java.nio.file.Files.readAllBytes(appJs.get))))
    case r if r.method == okay.http.Method.Get && r.url == "/events/market" =>
      // the market-wide feed — matched BEFORE the /events/<email>
      // prefix route: "market" must not parse as an email
      val src: Source[Chunk[Byte]] =
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(marketSub()))(kind =>
            sse("market", Json.print(JStr(kind)))))
      pure(Response(200, Seq("content-type" -> "text/event-stream"), src))

    case r if r.method == okay.http.Method.Get && r.url.startsWith("/events/") =>
      // the email rides the PATH: requestOf keeps the path only, a
      // query string never reaches the route (found the hard way)
      val email = java.net.URLDecoder.decode(
        r.url.stripPrefix("/events/"), "UTF-8")
      // the inbox as a LIVE stream: jetty keeps it open, a match
      // arriving tomorrow becomes a frame then
      // the hello frame flushes the headers so the subscriber's
      // request completes at once; matches follow whenever they land
      val src: Source[Chunk[Byte]] =
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(inbox(email)))(note =>
            sse("match", Json.print(JStr(note)))))
      pure(Response(200, Seq("content-type" -> "text/event-stream"), src))

    case r if r.method == okay.http.Method.Post && r.url == "/chat" =>
      val messages = messagesOf(r.body)
      val last = messages.lastOption.map(_.content).getOrElse("")
      if last.startsWith("/match") then
        // the matchmaking turn: the agent works the okay-match tools,
        // the answer streams through the same SSE framing
        val answer = matchTurnLogged(last.stripPrefix("/match").trim, messages.init, chatLog)
        def stream(ts: List[String]): Unit ! (Writer % String + Async) = ts match
          case Nil => pure(())
          case t :: rest => effect[Writer % String + Async, Unit](
            Writer(t + " ")).flatMap(_ => stream(rest))
        pure(Response(200, Seq("content-type" -> "text/event-stream"),
          reply(_ => stream(answer.split(' ').toList), budget)(messages)))
      else
        pure(Response(200, Seq("content-type" -> "text/event-stream"),
          reply(m, budget)(messages)))

  /** the whole demo as ONE value awaiting its environment
   * (demo-ctx-wiring): `main` wires production, a test wires stubs —
   * the same value both times, and a missing capability is a compile
   * error, not a container exception */
  def handler(budget: Int)
  : (Transport, Secrets, MatchStore) ?=> PartialFunction[Request, Response ! Async] =
    routes(model, budget)

  def main(args: Array[String]): Unit =
    val port = sys.env.get("OKAY_CHAT_PORT").flatMap(_.toIntOption).getOrElse(8090)
    val budget = sys.env.get("OKAY_CHAT_MAX").flatMap(_.toIntOption).getOrElse(512)
    val mode =
      if sys.env.contains("ANTHROPIC_API_KEY") then "live"
      else if sys.env.contains("OKAY_CHAT_BASE") then s"local ${sys.env("OKAY_CHAT_BASE")}"
      else "scripted"
    provide(Transports.http(), Secrets.env, market)(Resource.run[Unit, Pure](
      Jetty.serve(port)(handler(budget))().map { s =>
        println(s"chat: http://127.0.0.1:${Jetty.port(s)}  (model: $mode)")
        Thread.sleep(Long.MaxValue)   // ctrl-c ends the process and the Resource
      }).runWith)

  /** the React page: okay-ui's tree rendered by a real React (CDN
   * UMD globals), the logic cross-tested on the JVM — the frontend
   * the user asked for, still with no local build step beyond the
   * Scala.js link */
  val reactPage: String = """<!doctype html>
<meta charset="utf-8">
<title>okay chat</title>
<style>
  body { margin: 0; font: 15px/1.5 system-ui, sans-serif; background: #10141a; color: #e6e9ef; }
  #root > div { max-width: 640px; margin: 0 auto; padding: 1rem; display: flex; flex-direction: column; gap: .6rem; }
  input { padding: .6rem .8rem; border-radius: .7rem; border: 1px solid #2a3342; background: #171c25; color: inherit; width: 70%; }
  button { padding: .6rem 1rem; border-radius: .7rem; border: 0; background: #3563a8; color: white; cursor: pointer; }
</style>
<div style="max-width:640px;margin:0 auto;padding:.6rem 1rem;color:#7a869c;font-size:.85em">режим: MODE — маркетплейс: <a style="color:#6b9fff" href="/market">/market</a></div>
<div id="root"></div>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react/18.3.1/umd/react.production.min.js"></script>
<script crossorigin src="https://cdnjs.cloudflare.com/ajax/libs/react-dom/18.3.1/umd/react-dom.production.min.js"></script>
<script src="/app.js"></script>"""

  // ---- the page (no build step: the demo is of the server stack) ----

  val page: String = """<!doctype html>
<meta charset="utf-8">
<title>okay chat</title>
<style>
  body { margin: 0; font: 15px/1.5 system-ui, sans-serif; background: #10141a; color: #e6e9ef; }
  main { max-width: 640px; margin: 0 auto; padding: 1rem; display: flex; flex-direction: column; height: 100vh; box-sizing: border-box; }
  h1 { font-size: 1rem; color: #7a869c; font-weight: 600; }
  #log { flex: 1; overflow-y: auto; display: flex; flex-direction: column; gap: .6rem; padding-bottom: 1rem; }
  .m { padding: .55rem .8rem; border-radius: .7rem; white-space: pre-wrap; max-width: 85%; }
  .user { background: #2b4a7a; align-self: flex-end; }
  .bot { background: #1d2430; align-self: flex-start; }
  .cut { color: #e0a050; font-size: .85em; }
  form { display: flex; gap: .5rem; }
  input { flex: 1; padding: .6rem .8rem; border-radius: .7rem; border: 1px solid #2a3342; background: #171c25; color: inherit; }
  button { padding: .6rem 1rem; border-radius: .7rem; border: 0; background: #3563a8; color: white; cursor: pointer; }
</style>
<main>
  <h1>okay chat — streamed by okay-jetty, guarded by Cut</h1>
  <div id="status" style="color:#7a869c;font-size:.85em"></div>
  <div id="chips" style="display:flex;gap:.4rem;flex-wrap:wrap;margin:.4rem 0"></div>
  <div id="log"></div>
  <form id="f"><input id="i" autocomplete="off" placeholder="say something — or /match умею класть плитку email me@x"><button>send</button></form>
</main>
<script>
const log = document.getElementById('log'), f = document.getElementById('f'), i = document.getElementById('i');
const history = [];
let subscribed = false;
document.getElementById('status').textContent = 'режим: MODE — маркетплейс: /market';
const examples = [
  'умею класть плитку email tiler@x',
  'нужен сантехник email client@x',
  'спроси всех email client@x',
  'какая столица Франции?'];
const chips = document.getElementById('chips');
for (const ex of examples) {
  const b = document.createElement('button');
  b.type = 'button'; b.textContent = ex;
  b.style.cssText = 'font-size:.8em;background:#1d2430;border:1px solid #2a3342;color:#9fb0c8';
  b.onclick = () => { i.value = ex; i.focus(); };
  chips.appendChild(b);
}
function subscribe(email) {
  if (subscribed) return; subscribed = true;
  const es = new EventSource('/events/' + encodeURIComponent(email));
  es.addEventListener('match', ev => {
    const d = bubble('bot'); d.textContent = '🔔 ' + JSON.parse(ev.data);
    log.scrollTop = log.scrollHeight;
  });
}
function bubble(cls) { const d = document.createElement('div'); d.className = 'm ' + cls; log.appendChild(d); return d; }
f.onsubmit = async (ev) => {
  ev.preventDefault();
  const text = i.value.trim(); if (!text) return;
  i.value = '';
  bubble('user').textContent = text;
  history.push({role: 'user', content: text});
  const em = text.match(/[\w.+-]+@[\w.-]+/); if (em) subscribe(em[0]);
  const bot = bubble('bot');
  const res = await fetch('/chat', {method: 'POST', headers: {'content-type': 'application/json'},
    body: JSON.stringify({messages: history})});
  const reader = res.body.getReader(); const dec = new TextDecoder();
  let buf = '', answer = '', closed = false;
  for (;;) {
    const {done, value} = await reader.read(); if (done) break;
    buf += dec.decode(value, {stream: true});
    let idx;
    while ((idx = buf.indexOf('\n\n')) >= 0) {
      const frame = buf.slice(0, idx); buf = buf.slice(idx + 2);
      const ev = frame.match(/^event: (.*)$/m)?.[1] ?? 'data';
      const data = frame.match(/^data: (.*)$/m)?.[1] ?? '';
      if (ev === 'data') { answer += JSON.parse(data); bot.textContent = answer; }
      else if (ev === 'error') { const c = document.createElement('div'); c.className = 'cut';
        c.textContent = '⚠ ' + JSON.parse(data); bot.appendChild(c); closed = true; }
      else if (ev === 'cut') { const c = document.createElement('div'); c.className = 'cut';
        c.textContent = '✂ generation cut: ' + data; bot.appendChild(c); closed = true; }
      else if (ev === 'done') { closed = true; }
      log.scrollTop = log.scrollHeight;
    }
  }
  if (!closed) { const c = document.createElement('div'); c.className = 'cut';
    c.textContent = '⚠ поток оборвался — модель могла упасть'; bot.appendChild(c); }
  history.push({role: 'assistant', content: answer});
};
</script>"""
}

/**
 * A Postgres URL as operators write it: `postgres://user:pass@host
 * :port/db?sslmode=…&sslrootcert=…` — parsed purely so the demo's
 * one env var can be tested without a server. `sslmode` is the TLS
 * seam's ladder by its postgres names; absent means plaintext (the
 * dockerized default); `sslrootcert` is the CA for verify-ca/full.
 */
final case class PgTarget(host: String, port: Int, user: String, password: String,
                          database: String, tls: Option[TlsConfig])

object PgTarget:
  def is(s: String): Boolean = s.startsWith("postgres://") || s.startsWith("postgresql://")

  def parse(url: String): Either[String, PgTarget] =
    try
      val u = java.net.URI(url)
      if u.getHost == null then Left(s"no host in '$url'")
      else
        val userInfo: String = Option(u.getUserInfo).getOrElse("")
        val (user, pass): (String, String) =
          if userInfo.isEmpty then ("okay", "")
          else userInfo.split(":", 2) match
            case Array(un, pw) => (un, pw)
            case Array(un) => (un, "")
            case _ => ("okay", "")
        val path: String = Option(u.getPath).getOrElse("")
        val db: String = if path.stripPrefix("/").isEmpty then user else path.stripPrefix("/")
        val query: String = Option(u.getQuery).getOrElse("")
        val q: Map[String, String] = query.split("&").toVector.filter(_.nonEmpty).map { kv =>
          kv.split("=", 2) match
            case Array(k, v) => k -> v
            case _ => kv -> ""
        }.toMap
        val ca: Option[String] = q.get("sslrootcert")
        val tls: Either[String, Option[TlsConfig]] = q.get("sslmode") match
          case None | Some("disable") => Right(None)
          case Some("require") => Right(Some(TlsConfig(SslMode.Require, None, None, None)))
          case Some("verify-ca") => Right(Some(TlsConfig(SslMode.VerifyCa, ca, None, None)))
          case Some("verify-full") => Right(Some(TlsConfig(SslMode.VerifyFull, ca, None, None)))
          case Some(bad) => Left(s"sslmode '$bad' is not one of disable/require/verify-ca/verify-full")
        tls.map(t => PgTarget(u.getHost, if u.getPort < 0 then 5432 else u.getPort, user, pass, db, t))
    catch case e: Exception => Left(s"not a URL: ${e.getMessage}")
