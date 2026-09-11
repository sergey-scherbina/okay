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

## What it cannot say, and why

**Headers.** A route declares a path, its parameters, its query and
its body; a handler that reads a header reads it off the `Request`
with nothing declared anywhere. This renderer therefore says nothing
about headers rather than guessing, and the fix — if one is wanted —
is a declaration in okay-http, not a heuristic here.

**Prose.** A route has no place to carry a sentence about itself, so
summaries and tags are absent and operation ids are derived. That is
stage 3 of specs/openapi.md.

## Gotchas

- The order of paths follows the order the router declared them, on
  purpose: a diff of two documents should read as a diff of the code.
- `Api` carries what the router cannot know — title, version, servers
  — and nothing else. It is not a place for prose about operations;
  that is stage 3 of specs/openapi.md.
