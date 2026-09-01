# okay-match — two-sided matching over LLM-structured chats

One person tells an LLM what they offer; another tells it what they
need; the system stores both, finds the counterpart — in either
order of arrival — and walks the two through a negotiation whose
shape is itself data. The module is the framework the LLM structures
inside: effects to write against, tools that mirror them, stores
that are projections of a chat log. The design is argued in
[specs/match.md](../../specs/match.md); this page is the map and the
how-to.

## The two founding decisions

**Log-first.** The primary data is the chats themselves (persist
topics; `ChatLog`). Facts, profiles, indexes, flows — every
structured artifact is a REBUILDABLE projection: replay the log with
a better extraction and the store follows. Extraction is idempotent
by `(profile, attribute, provenance)`, so replaying over a live
store is a no-op and over a fresh one a rebuild — proven by test.

**A registry against vocabulary drift.** The failure mode of
LLM-written schemas is not "no schema" but the same attribute
invented five times under five names. Attributes are DATA in a
registry with a hard tool contract — search before create — and
synonym merges are projection rebuilds (`mergeAttr`), the log
untouched.

## The model, layer by layer

| layer | what it is |
|---|---|
| **Profile** | email + owner-secret UUID; both offers and seeks (matching is symmetric) |
| **Attribute** | registry data: slug, `Kind` (Text/Num/Range/Geo/Time/Ref), description, synonyms, `Provisional→Established`, `volatile` (decays rank), `identifying` (may generate link candidates) |
| **Fact** | `(profile, attr, side, typed value, provenance, confidence, ts, vis)` — append-only; corrections `supersede` with a reason; provenance is a chat span: the system can always quote WHY it believes something |
| **Visibility** | two gates: the owner's `Vis` (Public/Matched/Private) AND the platform's `Gate` (Allow/AfterMatch/Withhold). Effective disclosure is the minimum; Private never even matches |
| **Deal** | the built-in negotiation: `Asked → Accepted/Declined/Withdrawn`; `respond` is the asked party's alone; `Accepted` IS the "confirmed match" the Matched tier waits for — `contacts()` unlocks |
| **Scenario** | interaction flows as DATA — see below |
| **Identity** | cross-channel linking without a hijack path — see below |

## Search

`Find.candidates(Query(side, filters, text, k))` is hybrid: hard
predicates over typed facts EXCLUDE (`Pred.Is/AtLeast/AtMost/Within/
HasText` — number, range, geo radius, substring), embeddings RANK
the rest (per-side profile summaries; the embedder is a seam —
hashing offline, a real model in production), `volatile` attributes
decay the score on an exp2 half-life, and `Ranked.withheld` NAMES
the AfterMatch facts that matched — the seeker learns THAT a contact
exists, never what it is. Disclosure of the value is the platform's
transaction, executed by deals/flows.

## The reverse chain: events in either order

A need with no match today is stored; every `facts_assert` can be
wrapped (the demo does) so an OFFER arriving tomorrow runs the
reverse search over stored NEEDS (and vice versa) above a similarity
floor — and the waiting party's page rings. Nothing about it is the
model's job: the chain is structural.

## Scenarios as data

Which interaction flows a marketplace needs is as unknowable in
advance as which attributes — so the registry answer applies twice:

```scala
ScenarioDef(name, roles, initial, states, terminal, transitions)
Transition(name, from, to,
  by,        // the ROLE that may fire it
  unlocks,   // (viewer role, attribute) — visibility earned
  notifies)  // (role, template with {scenario}/{state}/{by}/{what})
```

`validate` answers malformations as data (unknown role/state,
terminal with exits, unreachable terminal) and refuses registration.
`Flow` is the instance — parties (role→profile), state, append-only
history — and `advanceFlow` is the ONE engine method: the transition
must exit the current state, the caller must hold its role, unlocks
are recorded (`unlockedBy`), the fired transition returned so the
application delivers its notifications. The deal machine is itself
four lines of data now (`ScenarioDef.deal`); a three-role escrow
sale runs on the same engine with zero engine changes (the test).

**The typed pen**: `ScenarioBuilder` is phantom-indexed by the
declared states and roles (a 20-line match-type membership, no
macros) — a route naming an undeclared state does not compile. The
data form stays primary and wire-loadable; the builder is for
definitions written in code.

## Cross-channel identity, without building the hijack

Only `identifying` attributes generate link candidates, and a
candidate answer is an attribute name plus a masked email — never a
value, never facts. The link is proven by a single-use expiring
token minted for the OLD profile and delivered through the OLD
channel (your transport's job); the recovery secret (a hash/verify
SEAM — okay-security's `Password` plugs in) is the dead-channel
fallback. A confirmed link is an equivalence, not a merge: search
folds the class into one candidate, nothing is rewritten.

## Stores: open by construction

`MatchStore` is the nominal trait; every engine is a constructor:

- `MemoryMatch` — the cross reference (tests, single-user).
- `SqlMatch(JdbcSql(conn))` — ANY `Sql` driver: H2 in tests, sqlite
  (`jdbc:sqlite:file.db` — booleans come back as integers; the
  decoder knows) and Postgres are the same line. Facts, deals,
  flows, unlocks, links: durable, restart-proven. Scenario
  DEFINITIONS are configuration, registered at boot.

Entropy, hashing, embedding, platform policy — all constructor
seams with working defaults; production fills them
(`SecureEntropy.strong` is already the JVM default).

## The tools

The whole surface mirrors 1:1 into agent `ToolSpec`s plus a Json
dispatch table (`Tools.specs` / `Tools.table(store)`) — exactly the
pair `mcp.Server.serve` takes:

`registry_search` `registry_propose` · `facts_register`
`facts_assert` `facts_supersede` `facts_profile` ·
`find_candidates` · `match_inquire` `match_respond` `match_deals`
`match_contacts` · `flow_start` `flow_advance` `flow_state`
`scenario_get` · `ident_candidates` `ident_request` `ident_confirm`

An LLM given this table and an intake prompt runs the whole
marketplace; the [chat demo](okay-demo.md) is exactly that, live.

## Tests

45+ across the module: every spec checkbox, both engines, sqlite
parity and restarts, the scripted two-sided scenario, the three-role
escrow walk, compileErrors pins on the typed builder.
