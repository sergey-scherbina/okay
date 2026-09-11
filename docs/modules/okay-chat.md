# okay-chat

> A streaming LLM chat component (specs/chat.md): the model seam,
> Cut-guarded SSE framing, and the `/chat` route — extracted 2026-09-02
> from `okay-demo`'s `ChatDemo.scala` (a pure move; the demo's page and
> market-flavored logic stayed where they are).

Depends on: `okay-llm` (the model, `Cut`), `okay-http`, `okay-conf`
(`Secrets`), `okay-codec`. JVM-only.

## Guide

**The model seam.** `Chat.Model = Seq[Anthropic.Message] => Unit !
(Writer % String + Async)` — history in, token stream out. Three
fillings: `scripted` (deterministic, offline — always works, no key),
`live(apiKey)` (Anthropic), `local(base)` (any OpenAI-compatible
endpoint). `Chat.model(using Transport, Secrets)` picks by what
`Secrets` holds (`ANTHROPIC_API_KEY` then `OKAY_CHAT_BASE`, else
`scripted`); `Chat.modeName` names the pick for a startup banner or a
page.

**The route.** `Chat.chatRoute(m, budget, turnOverride)` answers
`POST /chat`. `Cut.checked` stands in the stream with the token
budget; over it, the source stops and the frame says why
(`event: cut`, `{"rule":..,"at":..}`); otherwise every token is a
`data:` frame, then `event: done`.

**Intercepting a turn.** `turnOverride: (Request, Seq[Anthropic
.Message]) => Option[Source[Chunk[Byte]]]` gets first refusal at
every `/chat` call — the FULL request (so a consumer can read its own
headers, a bearer token say) plus the parsed messages. Answering
`Some(source)` takes over the reply entirely, already SSE-framed with
`Chat.sse`/`Chat.obj`; `None` falls through to the plain guarded model
stream. This is how `okay-demo` routes a `/match`-prefixed message to
its own marketplace turn while every other message rides the plain
model.

**Reused elsewhere.** `Chat.sse(kind, data)` is PUBLIC because a
consumer's OTHER streams (a live feed, a per-user inbox) want the
identical framing convention, not just `/chat`; `Chat.fieldOf`/
`messagesOf` parse a request body without depending on the route;
`Chat.appJs` locates a linked Scala.js/React bundle if one was linked
(`OKAY_CHAT_APP`, or the default build path), for a consumer that
serves two page faces the way `okay-demo` does.

| | |
|---|---|
| `Chat.Model`, `.scripted`/`.live`/`.local`/`.model`/`.modeName` | the seam and its fillings |
| `Chat.chatRoute(m, budget, turnOverride)` | `POST /chat` |
| `Chat.reply(m, budget)(messages)` | the guarded SSE source alone, for a consumer building its own route |
| `Chat.sse`/`Chat.obj` | the SSE frame / a JSON object literal, reused by a consumer's other streams |
| `Chat.fieldOf`/`.messagesOf` | body parsing |
| `Chat.appJs` | the linked frontend bundle, if one exists |

## The lead ledger (`okay.chat.leads`)

**Two lines in the chat.** `Leads.watch(ledger, salt)(sessionId, text)`
records a turn and returns the `Lead` it wrote;
`Leads.outcome(ledger, lead, Matched, contact)` appends what came of
it. Nothing else is needed, and no model is called: the category comes
from cues in Polish, Russian and English, the budget and the date from
`okay-intent`'s own parsers, and what none of them read stays
`Unknown`.

**What is deliberately absent.** The message. `session` is a salted
hash, so two requests from one conversation can be told apart from two
people's and nobody downstream can walk it back; `contact` exists only
when somebody asked to be contacted, and `Demand.deliverable` is the
only door that returns it.

**The ledger is a CSV.** It survives a restart, a spreadsheet opens it,
and `Bulk.csv` aggregates it on one JVM or on a cluster unchanged
(docs/modules/okay-spark.md). Append-only: an outcome changes later,
and `Demand.latest` takes the last row per request.

### Using it

```scala
val ledger = Ledger.at("leads.csv")
val salt = conf.secret("OKAY_LEADS_SALT")

// in the chat, per turn:
val lead = Leads.watch(ledger, salt)(sessionId, userMessage)
// later, when something came of it:
Leads.outcome(ledger, lead, Lead.Outcome.Matched, contact = Some("tg:@ann"))

// what to say to somebody who might pay for it:
println(Demand.report(ledger.read()).show)
```

```
sbt "okayChat/runMain okay.chat.leads.Report leads.csv"
```

### The ledger API

| member | signature | meaning |
|---|---|---|
| `Capture.lead` | `(message, sessionId, salt, at) => Lead` | cues + parsers, no model |
| `Capture.refine` | `(lead, category?, city?, budget?, urgency?) => Lead` | fill holes only |
| `Leads.watch` | `(ledger, salt)(sessionId, message) => Lead` | the whole integration |
| `Leads.outcome` | `(ledger, lead, outcome, contact?) => Lead` | appended, history kept |
| `Ledger.append/read/readEither` | | a row, all rows, all rows and every failure |
| `Demand.report` | `Iterable[Lead] => Report` | every figure, one pass |
| `Report.rolling` | `days => Vector[(date, Double)]` | the Group's window |
| `Demand.medianBudgets` | `Iterable[Lead] => Map[Category, (Long, Double)]` | t-digest, with the count |
| `Demand.deliverable` | `Iterable[Lead] => Vector[Lead]` | consent, and still live |

### Ledger gotchas

- The salt is a deployment secret. Rotating it is the "forget
  everybody" button — old rows stop being joinable to new ones, which
  is a feature and a thing to do on purpose rather than by accident.
- `Demand.latest` keys on `(session, at)`, so a correction must carry
  the ORIGINAL `at` (which `Leads.outcome` does by copying the lead).
- The cue tables are the product's own vocabulary and will need
  extending per market; a cue is a word that means somebody wants that
  kind of thing, not a word that merely occurs in such requests.

**Recording is a decorator of the route's own seam.** `Chat.chatRoute`
gives a `TurnOverride` first refusal at every turn, so the ledger hooks
there and nowhere else:

```scala
Chat.chatRoute(model, budget, Recording.turns(ledger, salt)())
```

It records the last USER message (the ask), never intercepts — whatever
the wrapped override answered is what the person gets — and swallows a
write failure, because a full disk must not take the chat down and a
lost row is worth less than a refused answer.

