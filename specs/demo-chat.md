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

- [ ] a full round (ask two, one declines, one accepts) through the
      real route: /deals/<n>.json for each shows its own event
      vector (Asked; Asked, Declined; Asked, Accepted) with
      provenance on every event
- [ ] the withdrawn stand-down carries its own event (Asked,
      Withdrawn) — the "someone else was chosen" story on the record
- [ ] a deal that never existed answers 404, not an empty timeline

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

## Conditions at the intake (demo-conditions)
The silent guest@demo default was a policy decision nobody made. The
intake now SIGNALS `BadEmail` (the condition road: the signal point
stays live), with the "guest" restart on the menu: the demo's
default policy INVOKES it (the old behavior, chosen on the record),
a repairing policy can RESUME with a corrected address,
`OKAY_CHAT_STRICT=1` FAILS with an Unhandled naming the declined
menu. One intake, three outcomes, chosen at run — and an email
present never consults the policy at all.

## Out of scope
- Auth, persistence of conversations, multi-user rooms (persist owns
  durable history; okay-match's DURABLE store is one constructor swap
  — the demo keeps memory).
- (Lifted 2026-09-01 by user ask: the React frontend landed as
  okay-chat-web.) The ui-wire browser leg stays its own demo.

## Decisions
- **SSE over WebSocket** — the reply is a one-direction stream per
  request; SSE is the smaller honest tool, and jetty's streaming
  path already speaks it.
- **History client-side** — a stateless server demos the stack
  without inventing session storage the spec would then owe.
- **The stub streams like the wire does** — token by token with the
  same framing, so the offline test proves the same path the key
  exercises.
