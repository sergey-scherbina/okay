package okay.demo

import okay.*
import okay.given
import okay.Condition
import okay.http.{Http, McpHttp, Request, Response}
import okay.mcp.{Mcp, Server as McpServer}
import okay.jetty.Jetty
import okay.llm.{Anthropic, Cut, Transport, Transports}
import okay.agent.{Agent, Compact, Handlers, Model as AgentModel, Provider, Tool, Turn, Context as AgentContext}
import okay.matching.{ChatLog, ChatTurn, MatchStore, MemoryMatch, SqlMatch, Tools as MatchTools}
import okay.ops.Ops
import okay.security.Secure
import okay.security.given
import okay.admin.Admin
import okay.chat.Chat
import okay.live.{Hub, Registry}
import okay.subscription.Subscription
import okay.subscription.Subscription.Period
import okay.persist.{FileStore, MemoryStore, Policy}
import okay.jdbc.JdbcSql
import okay.pg.{PgSql, PgTarget, PgTls}
import okay.sql.Placeholders
import okay.conf.Secrets
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

  // the chat mechanics are okay-chat now (extracted 2026-09-02,
  // specs/chat.md): Chat.Model/scripted/live/local/modeName/model/
  // reply/sse/obj/fieldOf/messagesOf/appJs, a bare String uuid-free
  // seam with no opinion about MatchStore/ChatLog held here or there.

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
  lazy val chatStore: okay.persist.Store = storeOf(sys.env.getOrElse("OKAY_CHAT_LOG", "okay-chat.log"))
  lazy val chatLog: ChatLog = ChatLog(chatStore.topic("web-demo", 1, Policy.default))

  def storeOf(path: String): okay.persist.Store =
    if path == ":memory:" then MemoryStore() else FileStore.open(java.nio.file.Path.of(path))

  /** for tests that want their OWN log without touching the ambient
   * OKAY_CHAT_LOG store */
  def logOf(path: String): ChatLog =
    ChatLog(storeOf(path).topic("web-demo", 1, Policy.default))

  /** a /match turn, LOGGED: register the speaker, append the turn,
   * extract with the log offset as provenance, log the answer too.
   * A verified session (demo-sessions) is the identity of RECORD; the
   * text-parsed "email x@y" stays the fallback for scripted/offline
   * turns, which present no session at all */
  def matchTurnLogged(text: String, history: Seq[Anthropic.Message], log: ChatLog,
                      sessionEmail: Option[String] = None)
                     (using Transport, Secrets, MatchStore): String =
    val store = summon[MatchStore]
    val me = store.register(sessionEmail.getOrElse(resolveEmail(text, intakePolicy)))
    val off = log.append(ChatTurn(me, "user", text))
    val answer = matchTurn(text, history, off, sessionEmail)
    log.append(ChatTurn(me, "assistant", answer)): Unit
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
        matchTurn(t.text, Nil, prov.offset): Unit
        n += 1
    }: Unit
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

  // the per-key channel registry is okay-live now (extracted
  // 2026-09-02, specs/live.md): the SAME shape as the market feed
  // below, noticed duplicated twice, generalized once.
  private val inboxes = Registry[String, String]()

  /** the open inbox of an email (created on first use) */
  def inbox(email: String): Channel[String] = inboxes(email)

  private def emailOf(store: MatchStore, p: okay.matching.ProfileId): Option[String] =
    store.profileOf(p).map(_.email)

  // the subscription gate is okay-subscription now (extracted
  // 2026-09-02, specs/subscription.md): `Subscription.subscribed`/
  // `pay`/`backdateJoin`/`subscriptionNotice`/`paySpec`, a bare
  // profile uuid, no opinion about MatchStore held here or there.

  /** after a stored fact: who was WAITING for it, on the other side? */
  def reverseChain(side: okay.matching.Side, text: String, now: Period = Period.now())
                   (using store: MatchStore): Unit =
    import okay.matching.*
    val other = if side == Side.Offer then Side.Need else Side.Offer
    // the floor keeps unrelated waiters quiet; how well related ones
    // score is the embedder seam's business (hashing offline — token
    // overlap; a real embedder in production understands morphology).
    // A gated waiter does not participate in matching either.
    val waiting = store.candidates(Query(other, text = text, k = 5))
      .filter(_.score > 0.1f)
      .filter(h => Subscription.subscribed(h.profile.uuid, now))
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
            store.withdraw(d.id, deal.seeker): Unit
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
  // model-independent. The broadcast itself is okay-live's Hub now
  // (extracted 2026-09-02, specs/live.md) — the same pattern as the
  // inbox registry above, generalized once.

  private val marketFeed = Hub[String]()

  /** a new /market subscriber's own channel */
  def marketSub(): Channel[String] = marketFeed.subscribe()

  /** ring every open /market page: something on the market moved */
  def marketChanged(kind: String): Unit = marketFeed.publish(kind)

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

  // keyed by (store identity, deal id) — NOT bare deal id: two
  // independent MemoryMatch() instances (one per test, typically)
  // both number their deals from 1, and an unscoped map lets a
  // SECOND store's events land on the FIRST store's deal — found
  // while landing demo-subscription-gate, a pre-existing bug from
  // demo-deal-timeline (cross-test collision, not concurrency: bare
  // JUnitCore runs sequentially, but the map outlives every test)
  private val dealEvents =
    java.util.concurrent.ConcurrentHashMap[(Int, Long), java.util.concurrent.CopyOnWriteArrayList[DealEvent]]()

  private def dealEvent(deal: Long, e: DealEvent)(using store: MatchStore): Unit =
    dealEvents.computeIfAbsent((System.identityHashCode(store), deal),
      _ => java.util.concurrent.CopyOnWriteArrayList())
      .add(e): Unit

  /** the current state plus the append-only history, for /deals/<n>;
   * None when the deal id was never asked (the "Asked" event names
   * the seeker, which is how the live Deal is found — the store has
   * no deal-by-id lookup, only dealsFor(profile)) */
  def dealTimeline(deal: Long)(using store: MatchStore): Option[(okay.matching.Deal, Vector[DealEvent])] =
    for
      events <- Option(dealEvents.get((System.identityHashCode(store), deal)))
      es = { import scala.jdk.CollectionConverters.*; events.asScala.toVector }
      asked <- es.find(_.state == "Asked")
      d <- store.dealsFor(okay.matching.ProfileId(asked.by)).find(_.id.n == deal)
    yield (d, es)

  /** the tool table with the reverse chain wrapped around asserts;
   * `off` is the ChatLog offset of the turn driving these calls
   * (default: a fresh counter tick, for callers with no log turn);
   * `now` is the subscription gate's period (default: the wall
   * clock — tests advance it explicitly to simulate a month passing) */
  def chainedTable(off: Long = turnNo.incrementAndGet(), now: Period = Period.now())
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
          store.dealsFor(okay.matching.ProfileId("")): Unit // no-op keeps types honest
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
      val profile = args match
        case JObj(fs) => fs.collectFirst { case ("profile", JStr(x)) => x }.getOrElse("")
        case _ => ""
      // a GATED profile's new post does not enter matching at all —
      // it must not surface to anyone waiting on the other side
      if text.nonEmpty && Subscription.subscribed(profile, now) then reverseChain(
        if side == "need" then okay.matching.Side.Need else okay.matching.Side.Offer, text, now)
      marketChanged("facts")
      out
    }).updated("facts_register", { c =>
      val out = base("facts_register")(c)
      // the notice rides the tool result itself — the LIVE path's
      // channel back to the model, which the system prompt teaches
      // it to relay (the same way it already reads its provenance
      // instruction from the tool it was told to use)
      Json.parse(out) match
        case JObj(fs) =>
          val uuid = fs.collectFirst { case ("profile", JStr(x)) => x }.getOrElse("")
          Subscription.subscriptionNotice(uuid, now) match
            case Some(n) => Json.print(JObj(fs :+ ("notice" -> JStr(n))))
            case None => out
        case _ => out
    }).updated("find_candidates", { c =>
      // the ONE filter both paths share: the deterministic driver's
      // search and the LIVE model's tool call both land here
      Json.parse(base("find_candidates")(c)) match
        case JArr(hits) =>
          Json.print(JArr(hits.filter {
            case JObj(fs) => fs.collectFirst { case ("profile", JStr(uuid)) => uuid }
              .forall(Subscription.subscribed(_, now))
            case _ => true
          }))
        case other => Json.print(other)
    }).updated("subscription_pay", { c =>
      val uuid = c.args match
        case JObj(fs) => fs.collectFirst { case ("profile", JStr(x)) => x }.getOrElse("")
        case _ => ""
      Subscription.pay(uuid, now)
      marketChanged("subscription")
      Json.print(JObj(Vector("paid" -> JBool(true), "period" -> JStr(now.key))))
    })

  // ---- the marketplace as an MCP server (demo-mcp-market) -------------
  //
  // chainedTable is already the ONE tool table both the LLM agent
  // path and the deterministic driver drive; serving it over MCP is
  // one more caller of the same substrate — a tool call from MCP
  // fires the same wraps (reverse chain, market feed, deal timeline)
  // a chat turn's tool call fires.

  /** rebuilt PER CALL (not once at mount time) — a static off/now
   * snapshot would give every MCP-driven fact the same stale ChatLog
   * offset and subscription period; per-call freshness matches what
   * the /chat route already does per HTTP request. MCP calls do NOT
   * append to chatLog (demo-replay-projections stays the chat
   * route's) — MCP is the marketplace's OTHER front door, not a
   * second writer to the durable turn log */
  def mcpTable(using MatchStore): Map[String, okay.agent.ToolCall => String] =
    val names = chainedTable().keys
    names.map(name => name -> ((c: okay.agent.ToolCall) =>
      chainedTable(turnNo.incrementAndGet(), Period.now())(name)(c))).toMap

  def mcpRoute(using MatchStore): Request => Response ! Async =
    McpHttp.route(McpServer.Serving(
      info = Mcp.Info("okay-demo-market", "0.1.0"),
      tools = MatchTools.specs :+ Subscription.paySpec,
      call = mcpTable))

  /** an agent turn over the match tools: the LLM structures the
   * chat into the store and searches it — the okay-match story */
  def agentTurn(text: String, history: Seq[Anthropic.Message],
                modelH: okay.Handler[AgentModel], off: Option[Long] = None,
                identity: Option[String] = None,
                now: Period = Period.now())(using MatchStore): String =
    // the log's offset, when the turn came through the log: the model
    // is TOLD the provenance instead of inventing one; a verified
    // session (demo-sessions) is told too, as the identity to use for
    // facts_register regardless of what the message text claims
    val system = matchSystem + off.fold("")(n =>
      "\nProvenance for THIS turn: chat \"web-demo\", offset " + n + " — pass exactly these to facts_assert.") +
      identity.fold("")(e =>
        s"\nThe speaker is a SIGNED-IN session as $e — use this email for facts_register, " +
        "even if the message names a different one.") +
      "\nSubscription gate: a profile's first month is free; after that it needs the CURRENT period paid" +
      " to appear in search or matching. facts_register's answer may carry a \"notice\" field — relay it to" +
      " the user, in their own language, when present. subscription_pay(profile) marks the current period paid" +
      " when the user asks to pay/subscribe (\"оплатить\"/\"pay\")."
    given okay.Handler[AgentModel] = modelH
    given okay.Handler[Tool] = Handlers.tools(chainedTable(off.getOrElse(turnNo.incrementAndGet()), now))
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
      Agent.converse(text, MatchTools.specs :+ Subscription.paySpec).reflect
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
    """матч-режим: скажите \"умею <что>\" / \"offer: <что>\" или \"нужен <кто>\" / \"need: <что>\" (и email <адрес>); после списка кандидатов: спроси 1 2 / спроси всех; исполнителю: берусь <N> / отказываюсь <N>; сценарии: сценарий <имя> роль=email ...; шаг <N> <переход>; флоу <N>; подписка: оплатить"""
  private val englishHelp =
    """match mode: say "can: <what>" / "offer: <what>" or "want: <who>" / "need: <what>" (and email <address>); after a candidate list: ask 1 2 / ask all; as a candidate: accept <N> / decline <N>; scenarios: scenario <name> role=email ...; step <N> <transition>; flow <N>; subscription: pay"""

  def scriptedAgent(text: String, off: Long = turnNo.incrementAndGet(),
                    identity: Option[String] = None, now: Period = Period.now())
                   (using store: MatchStore): String =
    import okay.codec.Json.*
    val t = chainedTable(off, now)
    val en = isEnglish(text)
    def call(name: String, args: (String, Json)*): String =
      t(name)(okay.agent.ToolCall("d", name, JObj(args.toVector)))
    // a verified session (demo-sessions) is the identity of record;
    // the text-parsed "email x@y" is the fallback for turns with no
    // session at all (scripted/offline callers)
    val email = identity.getOrElse(resolveEmail(text, intakePolicy))
    def profile: String =
      Json.parse(call("facts_register", "email" -> JStr(email))) match
        case JObj(fs) => fs.collectFirst { case ("profile", JStr(p)) => p }.get
        case _ => ""
    val answer = text match
      case s if s.contains("умею") || s.contains("can:") || s.contains("offer:") =>
        val skill = s.replaceAll(".*(умею|can:|offer:)\\s*", "").replaceAll("email [^ ]+", "").trim
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("skill"),
          "side" -> JStr("offer"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(skill)))): Unit
        // the contact rides as a MATCHED fact: only an accepted deal
        // will show it to anyone — the demo of the second gate
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("contact"),
          "side" -> JStr("offer"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "vis" -> JStr("matched"),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(email)))): Unit
        if en then s"""stored offer: "$skill" (profile $email)"""
        else s"""записал предложение: \"$skill\" (профиль $email)"""
      case s if s.contains("нужен") || s.contains("нужно") || s.contains("want:") || s.contains("need:") =>
        val want = s.replaceAll(".*(нужен|нужно|want:|need:)\\s*", "").replaceAll("email [^ ]+", "").trim
        // the need is STORED first — the reverse chain fires from it
        // when the matching offer arrives later
        call("facts_assert", "profile" -> JStr(profile), "attr" -> JStr("need"),
          "side" -> JStr("need"), "chat" -> JStr("web-demo"),
          "offset" -> JNum(off.toDouble), "span" -> JStr(text),
          "value" -> JObj(Vector("t" -> JStr("text"), "s" -> JStr(want)))): Unit
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
      case s if s.contains("оплатить") || s.contains("pay") =>
        val me = store.register(email)
        call("subscription_pay", "profile" -> JStr(me.uuid)): Unit
        if en then "subscription paid for this period — back in search and matching"
        else "подписка оплачена на этот месяц — снова в поиске и матчинге"
      case _ => if en then englishHelp else russianHelp
    // the reminder rides EVERY reply from a gated user (computed
    // AFTER dispatch, so a "pay" turn's own reply is already clear)
    Subscription.subscriptionNotice(store.register(email).uuid, now) match
      case Some(n) => answer + " — " + n
      case None => answer

  /** which agent serves /match turns: real model when one is
   * configured (by the AMBIENT secrets, over the AMBIENT wire), the
   * deterministic table-driver otherwise */
  def matchTurn(text: String, history: Seq[Anthropic.Message], off: Long,
                identity: Option[String] = None)
               (using t: Transport, s: Secrets, m: MatchStore): String =
    Chat.secret("ANTHROPIC_API_KEY").map { key =>
      agentTurn(text, history, Provider.anthropic(t, key, "claude-sonnet-4-5"), Some(off), identity)
    }.orElse(Chat.secret("OKAY_CHAT_BASE").map { base =>
      agentTurn(text, history, Provider.openAi(
        t, "local", "default", s"$base/v1/chat/completions"), Some(off), identity)
    }).getOrElse(scriptedAgent(text, off, identity))

  // ---- the streaming content cut (demo-streaming-cut) -----------------
  //
  // okay-chat's `reply`/`chatRoute` guard the token BUDGET; this is
  // the demo's own EXTRA rule, riding the same `policy` seam
  // (specs/llm-agentic.md, llm-streaming-cut) — a stand-in for
  // "off-policy content," proving the mechanism cuts on what a
  // stream SAYS, not only how much of it there is. Demonstrable
  // OFFLINE: `Chat.scripted` ECHOES the user's own message, so
  // typing a banned word is itself the trigger — no separate
  // demo-only model needed.

  private val bannedWords = Set("секрет", "пароль")

  def contentPolicy(i: Int, token: String): Option[Cut.Violation] =
    bannedWords.find(w => token.toLowerCase.contains(w))
      .map(w => Cut.Violation("content-policy", i, token))

  // ---- the routes ----------------------------------------------------

  def routes(m: Chat.Model, budget: Int)(using Transport, Secrets, MatchStore)
  : PartialFunction[Request, Response ! Async] =
    // built ONCE per server, not per request — mcpRoute constructs a
    // fresh McpHttp session table each time it is evaluated, and a
    // session issued on one request must still be found on the next
    val mcpR: Request => Response ! Async = mcpRoute
    val core: PartialFunction[Request, Response ! Async] = {
    case r if r.method == okay.http.Method.Get && r.url == "/" =>
      val html = (if Chat.appJs.isDefined then reactPage else page)
        .replace("MODE", Chat.modeName)
      pure(Response(200, Seq("content-type" -> "text/html; charset=utf-8"),
        Http.one(html.getBytes(UTF_8))))
    case r if r.method == okay.http.Method.Get && r.url == "/market" =>
      val store = summon[MatchStore]
      import okay.matching.*
      // rows stay SERVER-RENDERED at load (works without JS, and the
      // gate test keeps reading plain HTML); the script below then
      // re-renders from /market.json on every feed ping
      def rowsOf(side: Side) = store.candidates(Query(side, k = 50))
        .filter(h => Subscription.subscribed(h.profile.uuid)).map { h =>
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
          |<button id="replay" style="background:#2a3342;color:#e6e9ef;border:0;padding:.5rem .9rem;border-radius:.6rem;cursor:pointer">перестроить из лога</button>
          |<span style="color:#7a869c;font-size:.85em"> — сбросить проекцию и заново вывести её из журнала чатов (нужен admin-токен, okay-admin)</span>
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
          |document.getElementById('replay').onclick = async () => {
          |  const t = prompt('admin token (okay-admin):'); if (!t) return;
          |  const res = await fetch('/admin/replay', {method: 'POST', headers: {authorization: 'Bearer ' + t}});
          |  alert(res.ok ? await res.text() : 'отказано: ' + res.status);
          |  render();
          |};
          |</script>
          |""".stripMargin.getBytes(UTF_8))))

    case r if r.url == "/mcp" => mcpR(r)

    case r if r.method == okay.http.Method.Get && r.url == "/market.json" =>
      val store = summon[MatchStore]
      import okay.matching.*
      // the page's data: each disclosed fact with its ATTRIBUTE name —
      // the facet key; `disclosed` is Public-only for an anonymous
      // viewer, so the gate holds on the JSON exactly as on the HTML
      def rows(side: Side) = JArr(store.candidates(Query(side, k = 50))
        .filter(h => Subscription.subscribed(h.profile.uuid)).map { h =>
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
          val json = Chat.obj(
            "deal" -> JNum(n.toDouble), "what" -> JStr(d.what), "state" -> JStr(d.state.toString),
            "events" -> JArr(events.map(e => Chat.obj(
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


    case r if r.method == okay.http.Method.Get && r.url == "/app.js" && Chat.appJs.isDefined =>
      pure(Response(200, Seq("content-type" -> "text/javascript"),
        Http.one(java.nio.file.Files.readAllBytes(Chat.appJs.get))))
    case r if r.method == okay.http.Method.Get && r.url == "/events/market" =>
      // the market-wide feed — matched BEFORE the /events/<email>
      // prefix route: "market" must not parse as an email
      val src: Source[Chunk[Byte]] =
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(Chat.sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(marketSub()))(kind =>
            Chat.sse("market", Json.print(JStr(kind)))))
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
        effect[Writer % Chunk[Byte] + Async, Unit](Writer(Chat.sse("hello", "")))
          .flatMap(_ => Writer.map(Writer.of(inbox(email)))(note =>
            Chat.sse("match", Json.print(JStr(note)))))
      pure(Response(200, Seq("content-type" -> "text/event-stream"), src))

    case r if Ops.routes(chatStore).isDefinedAt(r) => Ops.routes(chatStore)(r)

    case r if r.method == okay.http.Method.Post && r.url == "/login" =>
      val email = Chat.fieldOf(r.body, "email")
      if email.isEmpty then
        pure(Response(400, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("error" -> JStr("email required")))).getBytes(UTF_8))))
      else
        val code = Login.start(email)
        println(s"login code for $email: $code (10 min)")
        // no email transport exists in this stack yet (specs/security.md):
        // the code rides the response so the demo is usable end to end;
        // real delivery replaces this ONE field with silence
        pure(Response(200, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("sent" -> JBool(true), "devCode" -> JStr(code)))).getBytes(UTF_8))))

    case r if r.method == okay.http.Method.Post && r.url == "/login/confirm" =>
      val email = Chat.fieldOf(r.body, "email")
      val code = Chat.fieldOf(r.body, "code")
      if Login.confirm(email, code) then
        val token = Login.issue(email)
        pure(Response(200, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("ok" -> JBool(true), "email" -> JStr(email), "token" -> JStr(token)))).getBytes(UTF_8))))
      else
        pure(Response(401, Seq("content-type" -> "application/json"),
          Http.one(Json.print(JObj(Vector("ok" -> JBool(false), "error" -> JStr("wrong or expired code")))).getBytes(UTF_8))))

    }
    // /chat itself is okay-chat now (extracted 2026-09-02, specs/
    // chat.md): the marketplace's /match turns ride the turnOverride
    // seam instead of a hardcoded prefix check inside the route
    val marketplaceTurnOverride: Chat.TurnOverride = (r, messages) =>
      val last = messages.lastOption.map(_.content).getOrElse("")
      Option.when(last.startsWith("/match")) {
        // a verified session identifies the speaker over the
        // text-parsed email
        val sessionEmail = Secure.bearerToken(r).flatMap(Login.verify(_))
        val answer = matchTurnLogged(last.stripPrefix("/match").trim, messages.init, chatLog, sessionEmail)
        def stream(ts: List[String]): Unit ! (Writer % String + Async) = ts match
          case Nil => pure(())
          case t :: rest => effect[Writer % String + Async, Unit](
            Writer(t + " ")).flatMap(_ => stream(rest))
        Chat.reply(_ => stream(answer.split(' ').toList), budget)(messages)
      }
    // admin routes are okay-admin now (extracted 2026-09-02,
    // specs/admin.md): Secure.granted + Policy.scoped("admin") —
    // /admin/replay is no longer reachable without an admin token
    core.orElse(Chat.chatRoute(m, budget, marketplaceTurnOverride, contentPolicy))
      .orElse(Admin.routes(Admin.Issuer.verify)(
        () => replayProjections(chatLog), () => marketChanged("replay")))

  /** the whole demo as ONE value awaiting its environment
   * (demo-ctx-wiring): `main` wires production, a test wires stubs —
   * the same value both times, and a missing capability is a compile
   * error, not a container exception */
  def handler(budget: Int)
  : (Transport, Secrets, MatchStore) ?=> PartialFunction[Request, Response ! Async] =
    routes(Chat.model, budget)

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
        // no delivery channel yet (same limit Login.start states about
        // its one-time code) — the admin token rides the console
        println(s"admin token (okay-admin, /admin/replay): ${Admin.Issuer.issue()}")
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
  <div id="login" style="display:flex;gap:.4rem;align-items:center;font-size:.85em;color:#9fb0c8;margin:.3rem 0"></div>
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
  'какая столица Франции?',
  'расскажи про секрет'];
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

// real login (demo-sessions): confirm-and-sign, not trust-the-field —
// the signed token then rides every /chat call as a Bearer header
const loginBox = document.getElementById('login');
function authHeaders() {
  const t = localStorage.getItem('okay_token');
  return t ? {authorization: 'Bearer ' + t} : {};
}
function renderLogin() {
  const email = localStorage.getItem('okay_email');
  loginBox.innerHTML = '';
  if (email) {
    loginBox.append('вошли как ' + email + ' ');
    const out = document.createElement('button');
    out.type = 'button'; out.textContent = 'выйти';
    out.onclick = () => { localStorage.removeItem('okay_token'); localStorage.removeItem('okay_email'); renderLogin(); };
    loginBox.appendChild(out);
    subscribe(email);
  } else {
    const ei = document.createElement('input'); ei.placeholder = 'email'; ei.style.cssText = 'width:11rem;padding:.3rem .5rem';
    const lb = document.createElement('button'); lb.type = 'button'; lb.textContent = 'войти';
    lb.onclick = async () => {
      if (!ei.value.trim()) return;
      await fetch('/login', {method: 'POST', headers: {'content-type': 'application/json'},
        body: JSON.stringify({email: ei.value.trim()})});
      const ci = document.createElement('input'); ci.placeholder = 'код из консоли сервера'; ci.style.cssText = 'width:11rem;padding:.3rem .5rem';
      const cb = document.createElement('button'); cb.type = 'button'; cb.textContent = 'подтвердить';
      cb.onclick = async () => {
        const res = await fetch('/login/confirm', {method: 'POST', headers: {'content-type': 'application/json'},
          body: JSON.stringify({email: ei.value.trim(), code: ci.value.trim()})});
        const d = await res.json();
        if (d.ok) { localStorage.setItem('okay_token', d.token); localStorage.setItem('okay_email', d.email); renderLogin(); }
        else { cb.textContent = 'не то — ещё раз'; }
      };
      loginBox.innerHTML = ''; loginBox.append('код для ' + ei.value.trim() + ': '); loginBox.appendChild(ci); loginBox.appendChild(cb);
    };
    loginBox.appendChild(ei); loginBox.appendChild(lb);
  }
}
renderLogin();
function bubble(cls) { const d = document.createElement('div'); d.className = 'm ' + cls; log.appendChild(d); return d; }
f.onsubmit = async (ev) => {
  ev.preventDefault();
  const text = i.value.trim(); if (!text) return;
  i.value = '';
  bubble('user').textContent = text;
  history.push({role: 'user', content: text});
  const em = text.match(/[\w.+-]+@[\w.-]+/); if (em) subscribe(em[0]);
  const bot = bubble('bot');
  const res = await fetch('/chat', {method: 'POST', headers: Object.assign({'content-type': 'application/json'}, authHeaders()),
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
