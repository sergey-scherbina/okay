# okay-match — structuring the unstructured, then finding it

## Overview
Two-sided matching over LLM-structured chat data. One person tells an
LLM what they can do; another tells it what they need; the system
stores both and finds the counterpart. The technical problem this spec
addresses is exactly the middle: **a framework in which an LLM can
save nearly anything in structured form and search it fast — without
the schema being known in advance and without the vocabulary
collapsing into chaos.**

The founding decision is log-first: the primary data is the chats
themselves, appended to a durable log (okay-persist topics). Every
structured artifact — facts, profiles, indexes — is a **rebuildable
projection** of that log. A better extraction prompt, a refactored
attribute vocabulary, a merged synonym: replay the log, rebuild the
projection. Structuring mistakes are recoverable by construction.

The second founding decision meets the real failure mode of
LLM-written schemas, which is not "no schema" but **vocabulary
drift** — the same attribute invented five times under five names
until search dies of synonymy. The answer is a registry of attributes
with a hard tool contract: search before create.

## The model

- **Profile** — the entity. Registration binds an email to a profile
  **UUID known only to its owner**. A profile both offers and seeks
  (matching is symmetric; a need is stored by the same machinery as a
  skill). Email change BY the owner is an ordinary superseding fact
  with history. Re-registration from a new address because "the old
  one stopped working" is an account-recovery/hijack problem — an
  OPEN security question, deferred to stage 2 with okay-security; the
  model records it, stage 0-1 do not solve it.

- **Chat** — entries in a persist topic; offsets are provenance. The
  log is append-only and is the only source of truth.

- **Attribute** (the registry) — slug, value type (a `Schema` from
  okay-codec: typed values, validation and serialization for free),
  description, synonyms, status (`provisional | established |
  merged-into(other)`), volatility (stable | volatile). New
  attributes are born provisional and promoted by use; synonyms are
  merged by a registry migration plus projection rebuild — the log
  never changes.

- **Fact** — `(profile, attribute, typed value, provenance,
  confidence, timestamp, supersededBy, visibility)`. Provenance is a
  chat reference plus text span: the system can always answer "why do
  you believe I can do X" with a quotation. Facts are append-only;
  corrections supersede with a reason (the ask-then-update
  conversation happens in chat; the framework records its outcome).

- **Visibility — two gates.** Owner intent (`public | matched |
  private`) AND platform policy. The effective disclosure is the
  minimum of the two: even what an owner would show, the platform may
  withhold (contact data flows only through platform-controlled
  disclosure — that gate is the business). Stage 0 stores both gates
  and honors them in search results; the policy engine behind the
  platform gate is stage 2.

- **Value types, the small core** — text, number, range, geo point /
  area, time window / schedule, entity reference. Grown through the
  registry, never designed up front; the whole construction exists so
  that guessing ahead is unnecessary.

## Interface
House style: an effect facade first, implementations as handlers.
Three concerns, three small signatures (names to settle in code):

- `Registry` — `search(text): Vector[AttrDef]` (semantic, over
  descriptions and synonyms), `propose(def): AttrId` (born
  provisional; a near-duplicate proposal RETURNS the existing
  attribute instead of creating), `get(slug)`.
- `Facts` — `assert(profile, attr, value, prov, conf, vis)`,
  `supersede(factId, value, reason)`, `profile(id): Profile` (current
  state AND history — reading profiles is required both for the
  in-chat merge dialogue and for search).
- `Match` — `candidates(query): ranked profiles` where the query is a
  structured need (facts + free text), executed hybrid: hard filters
  over typed facts, semantic candidates over embeddings, rank fusion;
  LLM rerank of the top-k is stage 2.

The LLM does the structuring through **tools that mirror these
operations 1:1** (MCP surface). The extraction contract is
replayable: an assert carries provenance, and asserting the same
(profile, attribute, provenance) twice is one fact — so re-running
extraction over the same log produces the same store, and the
interactive in-chat extraction (which reads the profile, notices
conflicts, asks, supersedes) commutes with offline replay.

## Behavior (stage 0)
- [x] search-before-create holds: proposing an attribute
      near-duplicate to an existing one returns the existing id, and
      the registry stays synonym-free under a replayed extraction
- [x] facts carry provenance to a chat span; supersede keeps the old
      fact reachable in history; `profile` shows current + history
- [x] both visibility gates are stored; search results never disclose
      below the effective gate (min of owner intent, platform policy)
- [x] replay idempotence: re-extracting the same chat log yields no
      duplicate facts and the same profile state
- [x] hybrid search on the memory handler: a hard constraint (typed
      fact filter) excludes; semantic similarity ranks the rest; a
      seeker's stored need finds the provider stored earlier
- [x] the MCP tools mirror the effect operations and a scripted
      two-side scenario (provider chat, then seeker chat) matches
      end to end

## Staging
- **stage 0** — the model, the three effects, the in-memory handler,
  embeddings via okay-rag's index, MCP tools, replay discipline.
- **stage 1** — durable handlers. Named first: sqlite and
  Postgres+pgvector through the Sql seam, and the chat log on persist
  topics; but the list is OPEN by principle — the three effects are
  the whole contract, so a backend is anything that can serve them:
  text files for a test, memory (the stage-0 reference), a technology
  that does not exist yet. A new backend is a new handler in one
  place; the core does not change. Registry migrations (synonym
  merge → projection rebuild) land here too. LANDED 2026-09-01:
  SqlMatch (the three handlers over any `Sql` driver — H2 in tests,
  proven surviving a restart), ChatLog on a persist topic with
  `replay` (a fresh store rebuilt from the log equals the live one;
  replaying over the live one changes nothing), and `mergeAttr`
  (the loser's facts move, the winner answers).
- **stage 2** — cross-channel/email-recovery identity (the hijack
  question, with okay-security), LLM rerank, the platform-policy
  engine for disclosure, freshness/volatility in ranking. LANDED
  2026-09-01 but for cross-channel identity: `Rerank` is an effect
  with a deterministic lexical handler (`top` composes it over
  `Find`); `PlatformPolicy` gates per attribute — Allow / AfterMatch
  / Withhold — and an AfterMatch fact that matched is NAMED in
  `Ranked.withheld` (the seeker learns THAT, not WHAT: the business
  hook); volatile attributes decay the rank (exp2 half-life);
  email recovery is a hashed-secret rebind behind a hash/verify SEAM
  (okay-security's Password plugs in at the site — no dependency),
  and without the secret a new address gets a fresh profile, never a
  hijack. Cross-channel identity landed as match-identity-x — see its section below.

## Cross-channel identity (stage 2 completion: match-identity-x)
The same person arrives from another messenger with no shared email.
The danger is symmetric to recovery: the LINK ITSELF must not become
the hijack — claiming someone's phone number in a new chat must earn
a stranger nothing.

- **Identifying attributes.** The registry marks attributes as
  `identifying` (a phone is, a skill is not). Only identifying
  facts generate link candidates, matched by exact value.
- **Candidates leak nothing.** `linkCandidates(p)` answers "a profile
  sharing your <attribute> exists" plus a masked email hint
  (`m***@e***.com`) — never the value, never the profile's facts.
- **The challenge proves control of the OLD channel.** `requestLink`
  mints a single-use, expiring token addressed to the OLD profile;
  the integration site delivers it through the old channel (the old
  chat, the old email — its job, not this module's). The person in
  the NEW chat producing the token proves they hold both ends;
  `confirmLink(token, prov)` then records the link, with chat
  provenance like every other event. The stage-2 recovery secret is
  the fallback route for a dead old channel.
- **A link is an equivalence, not a merge.** Both profiles stay;
  `identityOf` answers the class, `profileOf` and search read facts
  across it (one person, one candidate in results, whichever profile
  the facts live on). Nothing is rewritten — log-first holds.

Behavior:
- [x] only identifying attributes generate candidates, and the
      candidate answer carries the attribute name and a masked hint,
      not the value and not the other profile's facts
- [x] the token flow links: minted for the old profile, single-use,
      expiring; the wrong token and the expired token refuse; the
      recovery secret links as the fallback
- [x] the identity class reads as one: facts asserted on either
      profile serve one search candidate and one profile view
- [x] no hijack by assertion: asserting someone else's phone in a
      new chat yields a candidate hint and NOTHING else — no facts,
      no link, no disclosure

## Deals (match-deals): the confirmed match, at last
`Vis.Matched` promised "disclosed only after a confirmed match" and
the model had no such thing — the negotiation supplies it. Candidates
may be several; the client CHOOSES whom to ask; the asked may be
busy, may decline — several asked means someone agrees.

- **Deal**: (seeker, provider, what, state) — `Asked -> Accepted |
  Declined`; `Withdrawn` reserved. Append-only states, timestamps.
- `inquire(seeker, provider, what)` — the ask; `respond(deal, by,
  accept)` — only the ASKED provider may answer, anyone else is
  refused; `dealsFor(profile)` — both directions.
- **The unlock**: `contacts(viewer, other)` answers the other's
  `Vis.Matched` facts (and the platform's `AfterMatch`-gated ones)
  ONLY when an Accepted deal binds the two — the second gate's
  transaction, executed.
- Tools mirror it (match_inquire / match_respond / match_deals /
  match_contacts) so the model drives the flow.

Behavior:
- [x] several candidates answer a need, and inquiries go only to the
      CHOSEN ones
- [x] respond is the asked provider's alone; a stranger's answer is
      refused
- [x] Accepted unlocks contacts both ways (Matched facts,
      AfterMatch-gated facts); Declined and Asked unlock nothing
- [x] the engines agree: memory and sql (sqlite included) hold the
      same deal guarantees; deals survive a restart

## Scenarios as data (match-scenarios)
The deal was ONE state machine hardcoded in enums and engine methods;
the user's review question ("can we add new scenarios? how?") gets
the registry answer a second time: an interaction scenario is DATA,
symmetric to the attribute registry — because which scenarios will be
needed is as unknowable in advance as which attributes.

- **ScenarioDef** — name, roles, states (initial, terminals),
  transitions; a **Transition** carries the ROLE it belongs to (the
  generalization of "respond is the asked provider's alone"), the
  visibility unlocks it grants (viewer-role -> attribute; the
  generalization of contacts()), and the notifications it sends
  (role -> message template with {state},{by},{scenario} holes).
- **validate(def)** — total: unknown roles/states named by the
  transition, unreachable terminals, a terminal with exits — answers
  as data, never throws.
- **Flow** — the instance: scenario, parties (role -> profile),
  state, append-only history. `start` checks arity and validity;
  `advance(flow, transition, by)` is the ONE engine method: the
  transition must exit the current state and `by` must hold its
  role; effects fire on success.
- **unlockedBy(viewer, other)** — visibility earned through flows:
  facts whose attribute an executed transition unlocked for the
  viewer's role. `contacts` remains and is subsumed.
- Tools: scenario_define / scenario_get / flow_start / flow_advance /
  flow_state — the model drives ANY registered scenario the way it
  drives deals today.
- **The typed builder (stage 1)**: definitions are built in ONE
  program, so the doctrine's other half applies — a phantom-indexed
  builder where naming an undeclared state or role does not COMPILE;
  the data form stays primary (wire-loadable), the builder is the
  safe pen.
- **`MatchStore.scenarios: Vector[ScenarioDef]`** (demo-scenario-editor)
  — every registered definition, `deal` always among them; the
  listing an editor needs and `scenario`/`defineScenario` didn't
  answer alone. Both engines keep definitions in an in-memory map
  (`reset()` does not clear it — configuration, not projection);
  **stated limit**: `SqlMatch` does not persist scenario definitions
  to a table the way it persists flows (`match_flows`) — a restarted
  process starts with `deal` alone plus whatever the demo's own main
  re-registers. Pre-existing, not touched here.

Behavior:
- [x] a scenario defined as data runs end to end: roles enforced per
      transition, terminals close the flow, history kept
- [x] validate names each malformation as data (unknown role/state,
      unreachable terminal, terminal with exits)
- [x] transition unlocks grant visibility (unlockedBy) and
      notifications fire with the template holes filled
- [x] a SECOND scenario (multi-step, three roles) runs on the same
      engine with zero engine changes — the universality proof
- [x] engines agree (memory + sqlite): flows survive a restart
- [x] engines agree on Postgres too (demo-pg-backend): the SAME
      SqlMatch over the pg wire driver — the DDL portable (`DOUBLE
      PRECISION`), the `?` placeholders renumbered by
      `Placeholders.numbered`; the sqlite engine suite runs verbatim
      against live Postgres (guarantees, deals, flows, each surviving
      a reconnect); the store spec's "Postgres is the same line" is
      now a test, not a sentence
- [x] the typed builder: an undeclared state in a transition is a
      compile error; the built value equals the hand-written data

## The gate policy, live (demo-gate-ui)
`PlatformPolicy` (stage 2) was already DATA — an immutable
`Map[String, Gate]` — but it was bound at CONSTRUCTION time, a
constructor parameter never reassigned; flipping a gate meant editing
code and restarting. The BACKLOG ask ("let a viewer flip it and
watch /market react") needed one seam: the policy an operator sees
live, not the one baked in at boot.

- **`MatchStore.gate(attr): Gate`** — the current gate, `Allow`
  unless overridden; **`setGate(attr, g): Unit`** — flips it live, no
  restart; **`gateOverrides: Map[String, Gate]`** — everything
  currently overridden, for an admin listing.
- Both engines keep a `livePolicy` VAR seeded from the constructor's
  `policy` (the STARTING value); every disclosure check
  (`candidates`, `disclosed`) reads the var, so a flip takes effect
  on the very next query — no cache to invalidate, because there
  never was one.
- Configuration, not projection: `reset()` does not touch it, the
  same reasoning as scenario definitions (Scenarios as data, above).

Behavior:
- [x] setGate takes effect on the NEXT query, no restart — candidates
      filtered/withheld differently before and after the same call
- [x] gateOverrides answers exactly what was set, nothing implied by
      the default
- [x] reset() leaves the gate policy untouched (configuration, not
      projection — the scenario-definitions precedent)

## Conditions at the tools (match-conditions)
The v1 coercions of malformed tool values (a "num" tag with no
number quietly became 0.0) are now a NAMED policy. `valueOr` signals
`MalformedValue(tag, payload)` with the `legacy` restart on the
menu; `Tools.table(store)` keeps yesterday's behavior by invoking it
(nothing changes for existing users), `Tools.table(store, policy)`
lets a deployment repair (Resume with a corrected Value — the signal
point is live) or refuse (Fail becomes a {"refused": ...} answer the
MODEL can read and retry — no fact stored). A well-formed value
never consults the policy.

- [x] one malformed assert, three outcomes by policy: legacy
      coercion, repair-as-text, refusal with nothing stored — and a
      well-formed value never consults the policy

## The vector cache (match-vec-cache)

`MemoryMatch` has cached profile summaries since stage 0: `summaries`,
keyed `(profile, side)`, dropped on assert, supersede, an identity
link and reset. `SqlMatch` — the engine a deployment actually runs —
had nothing, and embedded every candidate's summary on every
`candidates()` and every live attribute's text on every
`registrySearch()`.

That was invisible while `embed` defaulted to `Vectors.hashing`, which
is arithmetic over character trigrams. With a real encoder it is one
model inference per candidate per query. Measured in a downstream
deployment on a real multilingual encoder, 50 profiles, 1 vCPU: 4.4s
for the first search, 1.5s for each one after, about 80ms per profile.

The fix is a table, not a hook:

    match_vecs(k VARCHAR PRIMARY KEY, fp VARCHAR, dim INT, vec BLOB)

`k` names the entity — `p:<uuid>:<side>` for a profile summary,
`a:<slug>` for an attribute — so the row count is bounded by entities
and an update overwrites rather than accumulates. `fp` is a SHA-256 of
the text the vector was computed from, and it is what makes this
correct without any invalidation logic: a changed fact is a changed
summary is a changed fingerprint, and a stale row is simply not used.

This matters more than it sounds. The alternative — invalidating on
every write path — is the design `MemoryMatch` uses, and it needs four
separate call sites to remember (assert, supersede, identity link,
reset); a fifth write path added later silently serves stale vectors.
The fingerprint cannot be forgotten, because it is derived from the
same text the vector is.

`embedTag` is a new constructor parameter, empty by default. Vectors
from one encoder are noise to another, and the dimension often matches
even when the model does not, so a deployment that can switch encoders
passes its model name and the fingerprint changes with it. Empty means
"unversioned", which is what every existing construction gets.

- [x] a second `candidates()` over the same facts embeds nothing
- [x] a changed fact changes the fingerprint and the vector is recomputed
- [x] a superseded fact does the same
- [x] the cache survives a new `SqlMatch` over the same database — the
      restart case the memory engine cannot have
- [x] `registrySearch` embeds each live attribute once, not once per call
- [x] a different `embedTag` recomputes rather than serving the old vector
- [x] `reset()` drops the cache with everything else
- [x] ranking is UNCHANGED: the same query answers the same order and
      the same scores it did before the cache existed

### One statement for many vectors (match-vec-batch)

The cache above removed the model inferences and left the round trips:
`candidates()` looked a vector up per candidate, which is one SELECT
per row. Measured downstream: ~1.3ms a row, 65ms for fifty, and it
grows with the marketplace exactly as the thing it replaced did.

`candidates()` now reads every cached vector for the passing set in
ONE statement, then embeds and stores only the misses. The batch is
chunked, because a database that accepts an `IN` list does not accept
an unbounded one.

- [x] a warm `candidates()` over N profiles issues ONE read, not N
- [x] a partly warm set embeds only the misses
- [x] ranking is unchanged, again: same order, same scores

## Out of scope
- The account-recovery security flow (recorded above; stage 2).
- Payment/monetization mechanics — the platform gate EXISTS in the
  model from day 0; what stands behind it is stage 2.
- Any UI; chats arrive through existing transports.

## Decisions
- **Log-first, projections rebuildable** — structuring errors must be
  recoverable; the LLM will be wrong and will improve.
- **A registry with search-before-create, not a free jsonb** — the
  failure mode is drift, and the registry is the one mechanism that
  bounds it while staying open.
- **Facts append-only with supersede + reason** — the user's merge
  policy (freshest wins, but ask first; sometimes the new one is the
  error and is discarded) needs history, not overwrite.
- **Two-gate visibility from day 0** — retrofitting privacy is
  expensive; one enum and one policy hook now are cheap.
- **Effect facade first, handlers second** — memory + rag now, any
  store later, the same typed programs over all of them; the backend
  list is open BY CONSTRUCTION (a handler in one place, the core
  untouched) — the house style, stated once here and assumed.
