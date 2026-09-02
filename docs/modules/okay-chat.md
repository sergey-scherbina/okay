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
