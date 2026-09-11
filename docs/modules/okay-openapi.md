# okay-openapi

> The OpenAPI document as a rendering of the router that serves it —
> paths, methods, parameters (path and query, each with the kind its
> `Param` declared), request bodies and declared responses, every
> schema being the one the codec was derived from.

Depends on: `okay` (JVM), `okay-http`, `okay-codec`.

## Guide

**Nothing is declared twice.** `OpenApi.document(api, router)` reads
`Router.entries` — the same vector that dispatches — so a path in the
document is a path that answers, and a request body's schema is
`JsonSchema.of` over the very `Schema` the decoder uses. A route
cannot be served and undocumented, or documented and unserved.

```scala
val api = Api("Board", "1.0", servers = Vector("https://board.example"))
val json: Json = OpenApi.document(api, router)
val text: String = OpenApi.print(api, router)
```

**OpenAPI 3.1, for a mechanical reason.** Its schema dialect IS JSON
Schema, so okay-codec's renderer is the whole schema story; under 3.0
every schema would need a translation layer with a drift of its own.

**The document is `Json`, not a typed model of the specification.** A
model would be a third vocabulary to keep in step with a standard this
repository does not own, and nothing here reads the document as types
— it is written out and served.

**Operation ids are derived** from method and path (`GET /board/{id}`
→ `getBoardById`), so they are stable: a document whose ids move when
nothing moved is a document nobody diffs.

**Parameters carry their kind.** `Router.Entry` holds
`Route.Described` — the path template AND its parameters — so
`Route[Int]("id")` renders as `{"type": "integer"}` and a `Queried`
route's query parameters are rendered at all, each with the
`required` its declaration gave it and an `array` schema where `all`
made it repeatable. Before `openapi-parameters` the entry carried the
template as a STRING, and this renderer re-parsed `{name}` out of it:
every path parameter came out a string and no query parameter came
out at all.

**A handler that answers a VALUE declares its response by its own
type:**

```scala
router.out[Int *: EmptyTuple, Task](Method.Get, byId)(t => pure(Task(t.head)))
router.jsonOut[EmptyTuple, NewTask, Task](Method.Post, board)((_, t) => pure(store(t)))
```

The router encodes with the same `Schema` the document renders, so the
two cannot drift, and it adds the failures it produces itself —
`jsonOut`'s 400 for a body that does not parse. A handler that builds
its own `Response` declares nothing, and the document says "undeclared"
rather than inventing a 200.

**And an answer that is not JSON declares too** (openapi-media).
Nothing in that argument was ever about JSON: it needs only that the
ROUTER, not the handler, decides what goes on the wire. So a page, a
stream and a bundle declare the same way:

```scala
router.html(Method.Get, Route.root)(_ => pure(page))
router.events(Method.Get, Route / "events" / "board")(_ => pure(feed))
router.bytes(Method.Get, Route / "app.js", "text/javascript")(_ => pure(bundle))
router.media(Method.Get, Route / "report", "application/pdf", 200, "last night's run")(...)
```

The handler answers the CONTENT and the router writes the
content-type, from the same value the entry declares, so an
operation's `content` key in the document is the media type a client
will actually receive. Each takes a `description`, which is the one
sentence about an answer that does not have to wait for stage 3.

The first consumer is the reason the shape exists: okay-demo answers
HTML, two event streams and a JavaScript bundle, and its committed
document said `undeclared` six times out of six while every one of
those answers was perfectly well defined.

## What it cannot say, and why

**Headers.** A route declares a path, its parameters, its query and
its body; a handler that reads a header reads it off the `Request`
with nothing declared anywhere. This renderer therefore says nothing
about headers rather than guessing, and the fix — if one is wanted —
is a declaration in okay-http, not a heuristic here.

**Prose.** A route has no place to carry a sentence about itself, so
summaries and tags are absent and operation ids are derived. That is
stage 3 of specs/openapi.md.

## Serving it

```scala
val all = app.routes orElse OpenApi.routes(api, app).routes
```

`/openapi.json` is the document; `/openapi` is a page for a person,
RENDERED ON THE SERVER. No JavaScript and no CDN: this repository
renders deployments for machines with no network, and a page that only
works where the network does is not documentation there. The cost is
that there is no "try it" button.

The document describes the router you pass, so by default it describes
the application and not the two routes that serve it. Pass the joined
router if you want them in.

okay-demo does this, and commits the result: `okay-demo/openapi.json`
is the rendering, `sbt "okayDemo/runMain okay.demo.DemoOpenApi"`
regenerates it, and a drift test refuses a difference — so an API
change shows up in review as a changed file beside the code that
changed it.

## Gotchas

- The order of paths follows the order the router declared them, on
  purpose: a diff of two documents should read as a diff of the code.
- `Api` carries what the router cannot know — title, version, servers
  — and nothing else. It is not a place for prose about operations;
  that is stage 3 of specs/openapi.md.
