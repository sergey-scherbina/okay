# okay-chat: a streaming LLM chat component

## Overview

Third and last of the three extractions the user asked for out of
`okay-demo/ChatDemo.scala` (specs/subscription.md and specs/admin.md
landed first). The demo's chat mechanics — the model seam, the
Cut-guarded SSE streaming, the request/body parsing — were already
decoupled from `MatchStore`/`ChatLog`: nothing in `Model`, `scripted`,
`live`, `local`, `reply`, `sse`, `fieldOf`, `messagesOf`, or `appJs`
mentions the marketplace. The one real tangle was the `/chat` route
handler itself, which hardcoded the `/match`-prefix special case
inline — this module cuts that seam with a hook (`turnOverride`)
instead of forcing a rewrite on either side.

## The model

- **`Model`** — `Seq[Anthropic.Message] => Unit ! (Writer % String +
  Async)`: history in, a token stream out. `scripted`/`live`/`local`
  are the three fillings (offline deterministic, Anthropic, any
  OpenAI-compatible endpoint); `model`/`modeName` pick and name one
  from the ambient `Secrets` (`ANTHROPIC_API_KEY`, else
  `OKAY_CHAT_BASE`, else scripted) — unchanged from the demo, `secret`
  reads `env:NAME` references so dispatch is testable without the
  process environment.
- **`reply`** — `Cut`-guards a `Model`'s stream against a token
  budget and frames it as SSE (`data:` per token, `event: done` or
  `event: cut` naming the violation). `sse`/`obj` are its private
  framing helpers.
- **`chatRoute`** — the `POST /chat` handler as a `PartialFunction`,
  parameterized by an optional `turnOverride` hook (below).
- **`fieldOf`/`messagesOf`** — pure JSON body parsing (one named
  string field; the OpenAI-shaped `messages` array).
- **`appJs`** — locates a linked Scala.js bundle (`OKAY_CHAT_APP`, or
  the conventional build-output glob), for a consumer that wants a
  richer client than server-rendered HTML.

## The `turnOverride` seam

```scala
type TurnOverride = Seq[Anthropic.Message] => Option[Source[Chunk[Byte]]]

def chatRoute(m: Model, budget: Int, turnOverride: TurnOverride = _ => None)
             (using Transport, Secrets)
: PartialFunction[Request, Response ! Async]
```

The override returns an ALREADY-SSE-FRAMED `Source`, not a bare
`String` — the demo's `/match` branch has its own token-splitting
shape (the marketplace agent's whole answer, split and restreamed
word by word through the same `Writer` loop `reply` uses), and
forcing every override into "just a string" would either lose that
shape or make this module reimplement it generically for a single
caller. When `turnOverride(messages)` answers `None`, `chatRoute`
falls through to `reply(m, budget)(messages)` — the plain path,
unchanged. The demo supplies `turnOverride = messages => Option.
when(messages.lastOption.exists(_.content.startsWith("/match")))
(marketplaceStream(messages))`, keeping `MatchStore`, `ChatLog`,
`Secure.bearerToken`, and `Login.verify` entirely on its side — this
module never learns those types exist.

## What stays in the demo

- **page/reactPage HTML** — the demo's copy is market-flavored (a
  `/market` link, Russian example chips, `/events/<email>` inbox
  subscribe-on-first-email-seen JS): not a few interpolated strings,
  a chunk of bespoke behavior. Templating it via a config case class
  was considered while planning this extraction and rejected —
  string-templating wearing a case-class costume, no real type
  safety gained. `okay-chat` ships no page at all; a consumer renders
  its own, calling `/chat` and reading the SSE frames `reply` writes
  (`data:`/`event: done`/`event: cut`, documented above).
- **`main`** — wires `Transports.http()` + `Secrets.env` + the
  marketplace's `market`/`chatLog` and starts Jetty; this module owns
  none of that, only the `Model`/`chatRoute` values `main` composes.
- **`/login`, `/login/confirm`, `/market`, `/market.json`,
  `/deals/*`, `/events/*`, `/admin/replay`** — every other route
  stays exactly where it already was (demo, or `okay-admin` since
  the previous landing); `chatRoute` only ever claims `POST /chat`.

## Interface

```scala
package okay.chat

object Chat:
  type Model = Seq[okay.llm.Anthropic.Message] => Unit ! (Writer % String + Async)
  type TurnOverride = Seq[okay.llm.Anthropic.Message] => Option[Source[Chunk[Byte]]]

  def secret(name: String)(using Secrets): Option[String]
  def scripted: Model
  def live(key: String)(using Transport): Model
  def local(base: String)(using Transport): Model
  def modeName(using Secrets): String
  def model(using Transport, Secrets): Model

  def reply(m: Model, budget: Int)(messages: Seq[Anthropic.Message]): Source[Chunk[Byte]]

  def fieldOf(body: Body, name: String): String
  def messagesOf(body: Body): Seq[Anthropic.Message]
  def appJs: Option[java.nio.file.Path]

  def chatRoute(m: Model, budget: Int, turnOverride: TurnOverride = _ => None)
               (using Transport, Secrets)
  : PartialFunction[Request, Response ! Async]
```

## Consumers

- `okay-demo`'s `ChatDemo.scala`: `routes` composes `Chat.chatRoute(
  model, budget, turnOverride)` via `orElse` alongside the
  marketplace routes and `Admin.routes` — three modules, three
  `PartialFunction`s, one server. `model`/`modeName`/`appJs` are
  called from the demo's own `page`/`main` exactly as before, just
  qualified. `fieldOf` is reused by `/login`/`/login/confirm` too
  (it was never marketplace-specific).

- [ ] the scripted path streams token by token and ends with
      `event: done` — through `Chat.chatRoute`, byte-identical to
      the pre-extraction behavior (the existing offline test, moved
      to prove the module rather than the demo's own copy)
- [ ] over budget the stream is CUT, named, and no tokens follow —
      same test, same assertion, now against the module
- [ ] `turnOverride` answering `Some` short-circuits `reply` entirely
      (the override's frames are what the client sees, unmodified);
      answering `None` falls through to the plain model path
- [ ] through the real demo route: `/match`-prefixed turns still
      reach the marketplace (unchanged from before this landing),
      proving the seam moved without moving the behavior

## Decisions

- **No trait+impl, a bare object** — same reasoning as
  okay-subscription: one implementation, direct-style functions
  already testable by construction (a fake `Model`, a canned
  `Transport`), no second consumer motivating an algebra yet.
- **The override returns a framed `Source`, not a `String`** — the
  cheaper "just override the text" hook was considered and rejected
  in planning: it would either drop the demo's own token-streaming
  shape or force this module to invent one generic enough for a
  single caller, the abstraction-before-a-second-user trap again.
- **No page/HTML ships with this module** — a rendering opinion is
  not "chat mechanics"; the SSE frame contract (documented above) is
  the actual interface a client needs, and it is small enough to
  restate rather than share.
