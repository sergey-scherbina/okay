# okay-subscription: gate a resource behind a paid period

## Overview

Extracted 2026-09-02 from `okay-demo/ChatDemo.scala`'s
demo-subscription-gate (specs/demo-chat.md, "The subscription gate"),
which was itself a direct user ask: a resource shows freely for its
first calendar month, then needs the CURRENT period paid to stay
visible — unpaid, it is GATED (excluded from search/matching) but
NEVER deleted, and gets a reminder. That logic was already fully
decoupled from `MatchStore`/`ChatLog` and took a bare `String` id —
genericizing it into its own module was a pure move, not a redesign:
every function, its default-argument shape, and its behavior are
unchanged from the demo. Only the home (`okay.subscription.
Subscription` instead of `okay.demo.ChatDemo`) and the callers
(`Subscription.subscribed(...)` instead of a same-file function call)
moved.

The doctrine this module is a straight port of, from the demo's own
spec: state the engine (or whatever domain module a subject belongs
to) has NO OPINION about lives beside it, keyed by an opaque id. This
module never sees a `Profile`, a `MatchStore`, or any domain type —
"subject" is just a `String`, so it gates a marketplace profile today
and could gate anything with an id tomorrow.

## The model

- `Period(y, m)` — a calendar-month key (`"2026-09"` via `.key`).
  `Period.now()` reads the wall clock; every gate function instead
  TAKES a `now: Period` (default `Period.now()`), so a caller (a
  test, or a scheduled job wanting to check "as of last month")
  never needs to wait on the clock.
- `joined: uuid -> Period` (private) — the FIRST period a subject was
  ever gate-checked, anchored LAZILY on first `subscribed` call. A
  subject the module never touched defaults to "just joined," never
  surprise-gated.
- `paid: uuid -> Set[Period.key]` (private) — periods actually paid.
- `subscribed(uuid, now) = joined(uuid, now) == now || paid.contains(now.key)`
  — free for the join period, paid-this-period or gated after.

`subscribed` is a QUERY, not a mutator: the anchor is set once,
lazily; calling it again with a different `now` does nothing (found
the hard way while landing the original demo feature — a test that
tried to "advance a month" by re-calling `subscribed` with an old
period was a silent no-op against an already-anchored subject).
`backdateJoin(uuid, period)` is the explicit, honest seam for a test
(or an operator tool) that wants a subject ALREADY past its free
month: it force-sets the anchor, not a query.

## Interface

```scala
object Subscription:
  final case class Period(y: Int, m: Int):
    def key: String
  object Period:
    def of(epochMillis: Long): Period
    def now(): Period

  def subscribed(uuid: String, now: Period = Period.now()): Boolean
  def pay(uuid: String, now: Period = Period.now()): Unit
  def backdateJoin(uuid: String, period: Period): Unit
  def subscriptionNotice(uuid: String, now: Period = Period.now()): Option[String]
  val paySpec: okay.agent.ToolSpec
```

- `pay` takes effect IMMEDIATELY — the very next `subscribed` check
  (same turn, even) sees it; there is no eventual-consistency delay.
- `subscriptionNotice` answers `None` when subscribed, `Some(text)`
  naming the gate otherwise — a caller decides HOW to surface it (a
  chat reply suffix, a banner, a tool-result field); the module does
  not render UI.
- `paySpec` is the static `ToolSpec` (name, description, JSON schema)
  for exposing "pay" to an LLM tool table — pure data, so every
  consumer gets the identical contract instead of re-deriving the
  schema. Where in a tool table it's wired, and what side effects
  ride along the actual payment action (e.g. a market-feed ping),
  is inherently per-app — this module ships the contract, not the
  wiring.

## Storage

In-memory only today (`ConcurrentHashMap`, JVM). No trait+impl split:
one implementation, no effect boundary anywhere in the code, so an
algebra would be an abstraction with no second user — the project's
own doctrine (PState/Delim policy: additive where apt, primary only
where necessary) applies the same way here. When a durable backend is
actually wanted, `okay-persist`'s `Store`/`Topic`/`Typed[A]` (used
today by `okay.matching.ChatLog` for exactly this "durable per-key
state" shape) is the natural next layer — added when a second
consumer needs it to survive a restart, not speculatively.

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `chainedTable`'s `facts_assert`/
  `facts_register`/`find_candidates`/`subscription_pay` wraps,
  `/market` + `/market.json` rendering, `reverseChain`, and
  `scriptedAgent`'s per-turn reminder all call `Subscription.*`
  directly, as an ordinary imported dependency (no DI parameter —
  there is no private state left in the demo to inject around; this
  mirrors how `okay.demo.Login` is already consumed as a plain
  object). Full behavior, unchanged: specs/demo-chat.md, "The
  subscription gate."

- [x] every existing demo-subscription-gate test in
      `TestChatDemo.scala` passes unchanged after the rename
      (`ChatDemo.subscribed` -> `Subscription.subscribed`, etc.) —
      the end-to-end HTTP-route proof this module's own unit tests
      do not replace
- [x] a fresh subject is subscribed in its join period with no
      reminder; `backdateJoin` into the past gates it (`subscribed`
      false, `subscriptionNotice` present)
- [x] `pay` un-gates the SAME check that follows it, no delay
- [x] `subscribed`/`subscriptionNotice` on a subject the module never
      saw before defaults to subscribed (lazy anchor, no
      surprise-gating)

## Filed (BACKLOG slugs, not built this pass)

Two more extractions out of `ChatDemo.scala`, designed but not yet
built (each earns its own claim, spec-gate, and landing):

- **okay-admin** — `adminRoutes(verify: String => okay.security.
  Verified, policy: okay.security.Policy = okay.security.Policy.
  scoped("admin"), replay: () => Long, onReplayed: () => Unit):
  PartialFunction[Request, Response ! Async]`, built on
  `Secure.granted` (the identical 401/403 ladder every other
  protected route in this stack already uses). Fixes a real gap
  found while planning this extraction: the demo's `POST
  /admin/replay` is completely UNAUTHENTICATED today.
  `replay`/`onReplayed` are injected closures — the marketplace-
  specific `replayProjections(chatLog)`/`marketChanged("replay")`
  stay in the demo. Flagged highest-risk of the three (first
  authenticated route this repository ships) — land it deliberately,
  with the same scrutiny okay-security's own auth code gets, not as
  a quick follow-on.
- **okay-chat** — the demo's `Model` type + `scripted`/`live`/
  `local`/`model`/`modeName` + `sse`/`reply`/`obj` (Cut-guarded SSE
  framing) + `fieldOf`/`messagesOf`/`appJs`, and `chatRoute(m,
  budget, turnOverride: Seq[Anthropic.Message] => Option[Source[
  Chunk[Byte]]] = _ => None)`. The override returns an
  ALREADY-SSE-FRAMED `Source`, not a bare `String`, so a consumer's
  special-cased turns (the demo's `/match` prefix) keep their own
  token-streaming shape instead of being forced through a
  string-only hook. The page/reactPage HTML stays OUT of the module
  — the demo's copy is market-flavored (a market link, example
  chips, `/events/<email>` inbox JS); templating that via a config
  case class was considered and rejected as string-templating
  wearing a case-class costume, no real type safety gained. The
  module can ship its own minimal generic page later if a
  non-marketplace consumer asks for one.

## Decisions

- **Bare object of functions, not a `trait Subscription` algebra** —
  matching okay-match's `Model.scala`/`Memory.scala`/`Sql` split
  would earn its keep the moment a SECOND backend exists; today
  there is one, and adding the trait now is the abstraction-before-
  a-second-user pattern this codebase explicitly avoids elsewhere.
- **`uuid: String`, not a domain type** — the module never learns
  what it's gating; genericity was already true of the demo code
  (profile uuids are just strings there too), so this cost nothing
  to keep.
- **`chainedTable` (which stays in okay-demo) takes no gate
  parameter** — `Subscription.subscribed`/`pay` are called as
  ordinary imports, not threaded through as function values; there
  is no private state left to inject around post-extraction, and the
  module's own `backdateJoin` is the test seam that would otherwise
  motivate a DI parameter.
