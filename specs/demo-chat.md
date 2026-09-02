# demo-chat — the chat with an LLM, as a web page

## Overview
The user-requested showcase: a demo web application for chatting
with an LLM, built out of what the repository already ships and
nothing else. One JVM main serves a page and streams completions;
the browser needs no build step (a small inline script — the demo is
of the SERVER stack, not of a frontend toolchain).

The pieces, each doing its one job:
- **okay-jetty** serves the page and STREAMS the response body live
  (`Response.body` is a `Source[Chunk[Byte]]`; jetty writes chunk by
  chunk on a virtual thread when the content type is
  `text/event-stream`).
- **okay-llm** turns the provider's SSE into a token stream
  (`Llm.stream`), over the real `TransportJvm` when
  `ANTHROPIC_API_KEY` is set.
- **Cut** (llm-streaming-cut) guards the stream: a demo rule cuts
  generation mid-sentence and the page SHOWS the cut — the P9
  feature made visible.
- **The offline mode is not a mock of the demo — it IS the demo**:
  with no key, a scripted model streams a deterministic reply, so
  the application always runs, tests prove it end to end on a real
  socket, and the key only swaps the model handler (the seam
  doctrine, once more).

## Interface
- `ChatDemo.main` — serves on `OKAY_CHAT_PORT` (default 8090).
- `GET /` — the page: message list, input, send; a fetch-reader
  appends tokens as they arrive; a cut is rendered as a marked line.
- `POST /chat` — body: `{"messages":[{"role":..,"content":..}]}`
  (the client keeps the history; the server stays stateless).
  Answer: `text/event-stream` of `data: <json string>` token events,
  then one `event: done` or `event: cut` with the violation.
- The model seam: `ChatDemo.model` is `Seq[Message] => tokens`;
  `scripted` (offline) and `live` (key) both fit it.
- The guard: a token budget (`OKAY_CHAT_MAX`, default 512) enforced
  by `Cut.checked` — over budget, the stream is cut, named.

## Behavior
- [x] offline, on a real socket: POST /chat streams the scripted
      reply token by token (the test reads the stream incrementally,
      not as one blob), ending with `event: done`
- [x] the page serves and carries the client script
- [x] the cut shows: a scripted reply exceeding the budget streams
      its prefix and ends with `event: cut` naming the rule; no
      tokens follow the cut
- [x] a LIVE model streams through the same route: the local
      OpenAI-compatible endpoint (rozum, :8089) proves the wire path
      end to end (skipped when absent); Anthropic remains the
      key-gated third filling of the same seam
- [x] the React frontend: okay-demo/web (module okayChatWeb) — the chat's view/update are
      CROSS and JVM-tested (send flow, token folding, the cut line);
      the browser gets okay-ui's ReactJs over CDN React UMD globals
      and a fetch reader feeding $token/$done/$cut into the same
      fold; the server serves the React page and /app.js when a link
      exists, the vanilla page otherwise

## Matching (demo-chat-match — user ask)
With a model configured there IS no gate: every turn is an agent
turn, the tools are always on the table, and the MODEL decides when
the marketplace applies (offer/need -> work the tools; anything else
-> just answer). `/match` remains as the deterministic driver's
prefix for the no-model mode, and as a forcing hint.
`/match <text>` turns are MATCHMAKING turns over the shared
marketplace — one store per server, providers and seekers meet
across sessions, and it is DURABLE by default: sqlite at
OKAY_CHAT_DB (default okay-chat.db; ":memory:" asks for the memory
engine). The ask made the store interface nominal: `MatchStore`
(okay-match) is what the tools and the demo take — any engine fits.
- with a model configured (Anthropic key, or any OpenAI-compatible
  `OKAY_CHAT_BASE` — the local rozum model drives it), the turn is an
  AGENT conversation: the model works okay-match's own tool table
  (facts_register, registry_search/propose, facts_assert,
  find_candidates) under a system prompt; the answer streams through
  the same SSE framing;
- with no model, a deterministic driver speaks THE SAME tool table
  (умею…/нужен…, email …) — the offline mode stays the demo, and the
  tests prove the two-sided story end to end on a real socket:
  a provider chats in, a seeker finds them, the marketplace remembers
  across turns.

- [x] offline two-sided match through the real route (store, find,
      remember across turns)
- [x] LIVE: the local model drives the okay-match tools (store-or-ask
      asserted; skipped where no model listens)
- [x] UNGATED live: with no prefix at all the model itself reaches
      for the tools on an offer and leaves the marketplace untouched
      on small talk (both asserted against the local model)
- [x] LIVE seeker: "мне нужно починить велосипед, найди мне
      кого-нибудь" finds the stored provider — a two-turn intake
      (the email asked for and given), the match reported with the
      provider's skills
- [x] REVERSE CHAIN: events in either order — a need stored today
      with nobody fitting rings the seeker's live inbox (/events SSE)
      the moment the matching offer arrives tomorrow; structural (the
      tool table is wrapped), model-independent, two-window tested
      through real routes
- [x] sqlite parity: the store guarantees hold on sqlite (the
      booleans-as-integers dialect trap caught and fixed) and the
      marketplace survives a restart over the same file
- [x] Postgres backend (demo-pg-backend): `OKAY_CHAT_DB=postgres://
      user:pass@host:port/db[?sslmode=…&sslrootcert=…]` puts the
      marketplace on live Postgres through the wire driver — the
      same SqlMatch, one env var; `sslmode` rides the URL as
      operators expect (the TLS seam's ladder; verify-full with
      `sslrootcert`). The URL parse is pure and tested; the store is
      proven by okay-match's engine suite against the dockerized
      Postgres
- [x] log-first, demonstrable (demo-replay-projections): every
      `/match` turn lands in a persist `ChatLog` FIRST (OKAY_CHAT_LOG,
      a FileStore directory; ":memory:" for tests) and the log
      offset IS the provenance of what the turn asserts — no
      in-memory counter that restarts at zero. `MatchStore.reset()`
      drops the projection (memory: the maps; sql: the tables,
      recreated); `POST /admin/replay` resets the store and rebuilds
      it from the log through the SAME extraction the live chat
      uses, answering how many turns it replayed; a button on
      /market does it in one click. Tested with a memory log: after
      two turns, reset + replay yields the same market; replaying
      over the live store changes nothing. Under a model, replay
      re-runs the model per user turn — stated, that is what "the
      projection is derivable" costs

## The reverse chain (demo-chat-async)
Events arrive in EITHER order. A need with no match is STORED (the
scripted driver asserts it before searching; the prompt tells the
model to); every facts_assert is wrapped server-side — an arriving
OFFER runs the reverse search over stored NEEDS (and vice versa),
above a similarity floor (how well related texts score is the
embedder seam's business); a hit lands in the matched profile's
inbox, held open by the page as an SSE stream (/events/<email> —
the email rides the PATH: the route sees no query string). Both
pages subscribe on the first email they see and render 🔔 bubbles.

## The negotiation round (match-deals in the demo)
Candidates are listed NUMBERED; the client chooses ("спроси 1 2" /
"спроси всех"); each chosen provider's inbox rings with the deal
number; providers answer in their own chats ("берусь N" /
"отказываюсь N"). The round policy is store-driven and
restart-surviving (deliberately not a fiber holding a continuation):
an acceptance notifies the seeker WITH the unlocked contact (the
Matched gate's transaction), withdraws the round's other asks and
tells each stood-down provider; a refusal notifies the seeker, and
when everyone declined the seeker hears that the request stands.
Domains are anybody's: services, housing, jobs — the machinery never
knew it was about repairs (the jobs round is the test; housing is
the engine test's domain).

- [x] the full round through real routes: three offers, a numbered
      list, ask-all, one declines (seeker told), one accepts (seeker
      told WITH the contact), the third stands down (told)

## The deal timeline (demo-deal-timeline)

`Deal` (okay-match) carries only its CURRENT state — no history. The
demo layer makes the negotiation visible without touching the
engine: `chainedTable` gains a threaded `off: Long` (the same log
offset `scriptedAgent`/`agentTurn` already carry for facts_assert's
provenance — default `turnNo.incrementAndGet()` when a caller has
none), and the `match_inquire`/`match_respond` wraps each append a
`DealEvent(state, by, Provenance("web-demo", off, what))` to an
in-memory per-deal log — append-only, never rewritten, the same
story `supersede` tells for facts. `GET /deals/<n>` (and
`/deals/<n>.json`) renders the deal's current state plus its full
event vector, each event's provenance shown (offset — the ChatLog
turn that caused it — and the span of what was asked/answered): the
Asked → Accepted/Declined arc, or Asked → Withdrawn when the round's
other asks stand down.

- [x] a full round (ask two, one declines, one accepts) through the
      real route: /deals/<n>.json for each shows its own event
      vector (Asked; Asked, Declined; Asked, Accepted) with
      provenance on every event
- [x] the withdrawn stand-down carries its own event (Asked,
      Withdrawn) — the "someone else was chosen" story on the record
- [x] a deal that never existed answers 404, not an empty timeline

BUG found and fixed while landing demo-subscription-gate
(2026-09-02): `dealEvents` was keyed by bare `Long` deal id — two
independent `MemoryMatch()` stores (one per test, typically) both
number their own deals from 1, so a SECOND store's events could land
on the FIRST store's deal id and `dealTimeline` would resolve against
the wrong store (surfaced as an intermittent "no such deal" 404 in
whichever OTHER test happened to run first in the same JVM). Fixed by
keying on `(System.identityHashCode(store), dealId)` — not a
concurrency bug (bare JUnitCore runs sequentially), a cross-test
STATE LEAK: the map outlives every individual test.

## Flows in the demo (demo-flows)
The generic scenarios reach the chat: flow_advance is wrapped like
the rest of the tool table — a fired transition's notifications are
delivered to the role-holders' inboxes with the templates filled, so
ANY registered scenario's steps ring the right pages with no
per-scenario code; the prompt teaches the model scenario_get /
flow_start / flow_advance.

- [x] a transition fired through the tool rings the named role's
      inbox with the filled template, and the unlock is queryable
      via unlockedBy
- [x] OFFLINE flows: the no-model driver plays ANY registered
      scenario by phrases alone (сценарий <имя> роль=email …;
      шаг <N> <переход>; флоу <N>) — the escrow walk through real
      routes, roles enforced, the buyer's page ringing

## The DI style (demo-ctx)
The marketplace store is an AMBIENT capability: seven functions that
threaded `store: MatchStore = market` (the hidden-global-with-
override idiom) now take `using MatchStore`; `main` does
`provide(market)(...)`, each test `provide(itsOwn)(...)` — the
ctx-everywhere doctrine's DI, applied where an application lives.
The forgot-to-thread class of bug (a test once hit the global
sqlite) is gone structurally. `Cut.checked` gained the ambient-
prompt door (additive in okay-llm), so the guard site reads
`Cut.guard { Cut.checked(tokens)(rule) }`.

## The wiring value (demo-ctx-wiring)

The factory half of ctx-wiring (specs/context-functions.md, Filed)
closed 2026-09-01 "reopen only with a consumer that actually
rewires" — this is that consumer. The demo's whole handler is ONE
value awaiting its environment:

```scala
def handler(budget: Int)
: (Transport, Secrets, MatchStore) ?=> PartialFunction[Request, Response ! Async]
```

`Transport` (okay-llm) is the wire; `Secrets` (okay-conf) is where
the config lives, read as `env:NAME` references (`secret(name)`);
`MatchStore` was already ambient (demo-ctx). `main` — the process
edge, the one place `sys.env` belongs — installs the production
environment: `provide(Transports.http(), Secrets.env, market)`. A
test installs stubs: `Secrets.memory` decides the model DISPATCH
(live/local/scripted — previously an untestable `sys.env` read),
and a canned `Transport` feeds `Anthropic.stream` real SSE lines —
the LIVE parsing path runs offline for the first time. Offline
tests wire a DEAD transport that throws on touch, so "offline never
reaches the wire" is asserted, not assumed.

- [x] the same `handler` value wired twice: with a canned wire + a
      key in memory-Secrets the LIVE branch streams the canned
      tokens through the real `Anthropic.stream`; with no key the
      scripted branch answers — one value, two environments
- [x] every offline suite runs over the DEAD wire (a touch is an
      AssertionError) and stays green
- [x] behavior unchanged at the edge: `main` wires `Transports.
      http()` + `Secrets.env`, which resolve the same env vars the
      old `sys.env` reads did

## The live market page (demo-market-live)

`/market` was a static render at page load; now it is a projection
that MOVES. Three pieces, all riding machinery the demo already has:

- `GET /market.json` — the page's data: offers and needs as rows of
  disclosed facts, each fact with its ATTRIBUTE name (`{"attr":
  "skill","text":"..."}`) — the facet key. The same gate holds as on
  the HTML: `disclosed` is Public-only for an anonymous viewer.
- `GET /events/market` — a market-wide SSE feed (matched BEFORE the
  `/events/<email>` prefix route — "market" must not parse as an
  email). Every subscriber gets its own channel in a registry;
  every market mutation pings every open page. The publish points
  are the chainedTable wraps the demo already owns (facts_assert,
  match_inquire, match_respond, flow_advance) plus /admin/replay —
  the model path and the deterministic driver go through the same
  wraps, so the feed is model-independent. A closed page's channel
  stays in the registry until process end — stated, not hidden: the
  demo's subscriber count is human-scale.
- The page: rows stay SERVER-RENDERED at load (works without JS,
  and the gate test keeps reading plain HTML); a script then fetches
  /market.json, re-renders on every feed ping, and offers the
  attribute facets as toggle chips (client-side filter).

- [x] market.json: the seeded Public skill shows with its attr; the
      Matched phone stays off it (the gates hold on the JSON too)
- [x] a subscribed /events/market stream rings when a new offer
      lands through the real /match route
- [x] the page carries the live script (EventSource on
      /events/market) and the facet container; rows still server-
      rendered (the existing polish assertions stay green)

## Streaming content cut (demo-streaming-cut)

The demo as `llm-streaming-cut`'s first consumer (specs/llm-agentic.md
— the Elsewhere gate this closes). Until now `Chat.reply` guarded ONE
rule: the token budget (`i >= budget`). `okay-chat`'s `reply`/
`chatRoute` gain an optional `policy: (Int, String) =>
Option[Cut.Violation]`, checked inside the SAME `Cut.checked` call
alongside the budget — additive, defaults to never-violate so every
existing caller (including `okay-chat`'s own tests) is unchanged.

The demo supplies a content policy: a small banned-word set standing
in for "off-policy content" (no real moderation claim — the point is
the MECHANISM, a live generation aborting mid-stream on what it
SAYS, not just how much). Demonstrable OFFLINE, the doctrine once
more: `scripted`'s reply ECHOES the user's own message
(`"You said: $last — ..."`), so typing a banned word is itself the
trigger — no separate demo-only model wrapper needed. An example
chip on the page invites it directly.

- [x] a scripted reply containing a banned word is CUT before the
      full echo streams: `event: cut` names the content-policy rule,
      and no token after the banned one is emitted (through the real
      `/chat` route, not just the guard in isolation)
- [x] a scripted reply with NO banned word streams to `event: done`
      exactly as before this change (the passing path is unchanged)
- [x] over-budget still cuts on `token-budget` when a reply is both
      long AND clean — the two rules coexist in one guard, neither
      shadows the other
- [x] `Chat.chatRoute`'s default (`policy` omitted) behaves BYTE-
      IDENTICAL to the pre-widening signature — proven directly in
      okay-chat's own suite, not just asserted here

## The marketplace as an MCP server (demo-mcp-market)

`chainedTable` (the marketplace tool table, already the ONE surface
both the LLM agent path and the deterministic driver drive — specs/
demo-chat.md, "Matching") is a `Map[String, ToolCall => String]`, and
`MatchTools.specs :+ Subscription.paySpec` is its `Seq[ToolSpec]` —
exactly what `okay.mcp.Server.serve` takes. Serving it is the
integration the doc comment on `Tools.scala` names ("one call at the
integration site"): `McpHttp.route` mounts that server at `POST/GET
/mcp`, and any MCP client — Claude Code, Claude Desktop, a hand-rolled
one — becomes a market participant over the SAME substrate the chat
UI already drives. A tool call from MCP fires the SAME wraps a chat
turn's tool call fires (the reverse chain, the market feed ping, the
deal timeline) — model-independent was already the doctrine
(demo-chat-async); this is one more caller proving it, not a special
case.

```scala
def mcpTable(using MatchStore): Map[String, okay.agent.ToolCall => String]
def mcpRoute(using Transport, Secrets, MatchStore): Request => Response ! Async
```

`mcpTable` rebuilds `chainedTable(turnNo.incrementAndGet(),
Period.now())` PER TOOL CALL rather than once at server-mount time —
`Server.serve` takes one static table, but a static `off`/`now`
snapshot would give every MCP-driven fact the SAME stale ChatLog
offset and subscription period; per-call freshness matches what the
`/chat` route already does per HTTP request. Stated limit: unlike
`/chat`'s `/match` turns, MCP tool calls do NOT append to `chatLog`
(demo-replay-projections) — the log-first story stays the chat
route's; MCP is the marketplace's OTHER front door, not a second
writer to the durable turn log. A deployment wanting MCP calls
logged too is a straightforward follow-up, not implied here.

- [ ] an MCP `initialize` handshake against `/mcp` succeeds and lists
      the marketplace tools, `subscription_pay` among them
- [ ] an MCP `facts_assert` (an offer, Public) is immediately visible
      through `/market.json` — the SAME projection the chat UI reads,
      proving MCP and chat share one store, not two
- [ ] an MCP `facts_assert` on one side (an offer) rings a WAITING
      chat-side inbox on the other (a need stored earlier through
      `/chat`) — the reverse chain fires for an MCP-originated fact
      exactly as for a chat-originated one
- [ ] the chat UI is UNCHANGED: every existing demo test still passes
      — MCP is an additional front door, not a rewrite of the
      existing one

## Polish (demo-polish)
- The page states its MODE (scripted/local/live) and links /market.
- `/market` — the marketplace, visible: offers and needs as lists of
  their disclosed facts; ONLY Public shows (the gates hold on the
  page too, by test — a Matched phone stays off it).
- Example chips fill the input (the demo teaches itself).
- Failure is visible: the agent path answers `event: error` when the
  model dies (not a 500); the streaming path's dropped connection is
  detected client-side ("поток оборвался").
- "помощь"/"help" reaches the driver's phrasebook.

## The English phrasebook (demo-en-phrasebook)

The offline driver (`scriptedAgent`) spoke only Russian. Language is
now picked PER MESSAGE, not configured: `isEnglish(text)` is "no
Cyrillic character" — content alone decides which reply template
answers, with no session state. Every trigger pairs one-for-one:
умею/can:/offer:, нужен/нужно/want:/need:, спроси/ask (+
всех/all), сценарий/scenario, шаг/step, флоу/flow,
берусь/accept, отказываюсь/decline, помощь/help (already paired —
now decided by which WORD triggered, since an empty string carries
no Cyrillic either and would otherwise misroute). Both phrasebooks
speak the SAME tool table through the SAME wraps (chainedTable) —
language is presentation, not a second code path.

- [x] the full offer -> need -> ask -> accept round, phrased entirely
      in English, through the real route: matches the Russian round's
      shape (demo-chat-match/match-deals), replies in English
      throughout
- [x] "help" answers the English phrasebook; "помощь" still answers
      the Russian one — the empty-string trap named above is covered
- [x] a scenario/flow round (scenario ... ; step N ...; flow N)
      phrased in English walks the SAME escrow scenario the Russian
      OFFLINE FLOWS test walks, in English throughout

## Conditions at the intake (demo-conditions)
The silent guest@demo default was a policy decision nobody made. The
intake now SIGNALS `BadEmail` (the condition road: the signal point
stays live), with the "guest" restart on the menu: the demo's
default policy INVOKES it (the old behavior, chosen on the record),
a repairing policy can RESUME with a corrected address,
`OKAY_CHAT_STRICT=1` FAILS with an Unhandled naming the declined
menu. One intake, three outcomes, chosen at run — and an email
present never consults the policy at all.

## Sessions (demo-sessions)
- [x] confirm-and-sign login replaces trust-the-field: `POST /login`
      `{email}` mints a one-time 6-digit code (this stack has no
      email transport yet, specs/security.md — the code rides the
      response AND the server console, named as the demo's stated
      limit, not hidden); `POST /login/confirm` `{email,code}` spends
      it once and answers a signed ES256 session (`okay-security`'s
      `Jwt`, an in-process key pair — a restart signs everyone out,
      stated); the client presents it as `Authorization: Bearer` on
      every following call
- [x] a verified session is the identity of RECORD for a `/match`
      turn: it registers the ChatLog speaker AND is what the tool
      table asserts facts under, overriding a DIFFERENT "email x@y"
      the message text claims (proven: a session as `real@x` asserts
      facts under `real@x` even when the same message names
      `spoofed@x`; the deterministic driver enforces this today, a
      live model is TOLD the session and asked to honor it, unproven
      without a live key). The text-parsed email stays the fallback
      for a turn with no session — scripted/offline callers, and the
      existing tests, are unchanged
- [x] the vanilla page gets a real login widget: email → code → the
      token in `localStorage`, sent on every `/chat` call; logging in
      subscribes the SSE inbox by the same email, so the demo's
      "email chip" workflow is no longer required to see a match
- Cross-channel identity RIDES the token now (the ask's phrase): the
  same signed-in session resolves to one email everywhere it is
  presented, so identity no longer depends on what a message happens
  to say. Deeper cross-channel work — driving `requestLink`/
  `confirmLink` from two live sessions in the UI — is a separate,
  smaller box if named wanted; the identity primitive it would need
  (a verified session) is what this box delivers

## The subscription gate (demo-subscription-gate — user ask)

EXTRACTED 2026-09-02 into its own module (specs/subscription.md,
`okay-subscription`) — a pure move, no behavior change: the demo
calls `Subscription.subscribed`/`pay`/`backdateJoin`/
`subscriptionNotice`/`paySpec` now instead of same-file functions.
This section stays as the original design record; the module's own
spec is the source of truth for the API going forward.

A profile shows and matches FREE for its first calendar month; after
that, only a period actually PAID keeps it visible. Unpaid: gated —
absent from search AND from matching (the reverse chain does not
notify others of it, and it does not notify on others' new posts
either) — but NEVER deleted, and every turn from a gated user carries
a reminder. Paying takes effect IMMEDIATELY (same turn) and the
reminder stops for that period. Demo layer only — `okay-match`'s
`MatchStore` is untouched, the same doctrine as
[[demo-deal-timeline]] and [[demo-market-live]]: state that the
engine has no opinion about lives beside it, keyed by profile uuid.

- `Period(y, m)` — a calendar-month key (`"2026-09"`); `Period.now()`
  reads the wall clock, but every gate function TAKES a `now:
  Period` (default `Period.now()`) so tests advance months without
  waiting one — the same threading style `off` already uses for
  provenance.
- `joined: uuid -> Period` — the FIRST period a profile was ever
  checked (anchored lazily, on first gate check — a profile the demo
  never touched defaults to "just joined," never surprise-gated).
  `paid: uuid -> Set[Period.key]` — periods actually paid.
  `subscribed(uuid, now) = joined(uuid, now) == now || paid.contains(now.key)`.
  Note `subscribed` is a QUERY, not a mutator — the anchor is set
  once, lazily, on a profile's first-ever check, so passing a
  different `now` later does nothing; `backdateJoin(uuid, period)` is
  the explicit test seam that forces it (found the hard way: a test
  that tried to "advance a month" by re-calling `subscribed` with an
  old period was a silent no-op against an already-anchored profile).
- **Enforcement, three sites** (all filter by `subscribed`, never
  delete): the `find_candidates` tool wrap (covers both the
  deterministic driver's search AND the LIVE model's tool calls —
  one filter, two paths); the reverse chain's `waiting` search
  (demo-chat-async) — AND the wrap skips firing the chain at all
  when the ASSERTING profile itself is gated, so a gated post never
  surfaces to anyone; `/market` and `/market.json` rendering
  (demo-market-live).
- **The reminder, two mechanisms for two paths**: the deterministic
  driver (`scriptedAgent`) appends a suffix to its own reply,
  computed AFTER the turn's dispatch (so a "pay" turn's own reply
  never carries a now-stale reminder — `subscribed` is already true
  by the time the suffix is computed); the LIVE path's
  `facts_register` tool wrap attaches a `"notice"` field to its JSON
  answer when gated, and one new sentence in `matchSystem` tells the
  model to relay it, in the user's language — the SAME channel the
  model already reads its provenance instruction from.
- **Paying**: a new `subscription_pay` tool (profile) marks the
  CURRENT period paid — demo-only, no real payment integration (the
  same "the offline mode is not a mock, it IS the demo" spirit: a
  stub that behaves like the real gate would). The driver phrases
  are `оплатить`/`pay`, paired per [[demo-en-phrasebook]]'s doctrine.

- [x] a fresh profile is visible and matchable in its join month with
      no reminder; advancing `now` one period with nothing paid gates
      it from find_candidates, from the reverse chain (as poster AND
      as the waiting side), and from /market and /market.json — and
      every turn from that user now carries a reminder
- [x] `pay` (or `оплатить`) un-gates the SAME turn: the reply carries
      no reminder, and an immediately following search/market check
      shows the profile again
- [x] a gated profile is never REMOVED from the store — its facts and
      profile row are readable throughout; only visibility/matching
      are withheld
- [x] the LIVE path: `facts_register`'s JSON carries a `notice` field
      exactly when gated, absent otherwise (a stub Transport/handler
      test, not a live-model assertion)

## Out of scope
- Persistence of conversations, multi-user rooms (okay-match's
  DURABLE store is one constructor swap for facts/deals/flows; the
  chat HISTORY itself stays client-side, by the SSE over WebSocket
  decision below).
- Passwordless is not passwordFREE forever: real email delivery,
  rate limiting the code endpoint, and a refresh/rotate story are
  this box's stated gaps — a demo's session, not a production one.
- (Lifted 2026-09-01 by user ask: the React frontend landed as
  okay-chat-web.) The ui-wire browser leg stays its own demo.
- Real payment processing — `subscription_pay` is a demo stub, same
  as everything else in the offline driver; a real integration would
  be its own gated backlog entry, not implied by this ask.

## Decisions
- **SSE over WebSocket** — the reply is a one-direction stream per
  request; SSE is the smaller honest tool, and jetty's streaming
  path already speaks it.
- **History client-side** — a stateless server demos the stack
  without inventing session storage the spec would then owe.
- **The stub streams like the wire does** — token by token with the
  same framing, so the offline test proves the same path the key
  exercises.
