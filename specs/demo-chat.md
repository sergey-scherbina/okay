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
