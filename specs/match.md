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
  engine for disclosure, freshness/volatility in ranking.

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
