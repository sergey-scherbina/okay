# okay-openapi

> The OpenAPI document as a rendering of the router that serves it —
> paths, methods, path parameters and request bodies, with the body's
> schema being the one the decoder was derived from.

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

## What it cannot say yet, and why

`Router.Entry` carries the method, the path template and the request
body's schema. That is all it carries, so:

- **path parameters are declared as strings.** `Route[Int]("id")`
  knows it is an int; the kind lives on `Route.params` and never
  reaches the entry.
- **query parameters are absent.** A `Queried` route knows them; the
  entry does not.
Responses are no longer among them. A handler that answers a VALUE
declares its response by its own type:

```scala
router.out[Int *: EmptyTuple, Task](Method.Get, byId)(t => pure(Task(t.head)))
router.jsonOut[EmptyTuple, NewTask, Task](Method.Post, board)((_, t) => pure(store(t)))
```

The router encodes with the same `Schema` the document renders, so the
two cannot drift, and it adds the failures it produces itself —
`jsonOut`'s 400 for a body that does not parse. A handler that builds
its own `Response` declares nothing, and the document says "undeclared"
rather than inventing a 200.

The two remaining gaps are declarations in okay-http and are filed
there (BACKLOG "openapi").

## Gotchas

- The order of paths follows the order the router declared them, on
  purpose: a diff of two documents should read as a diff of the code.
- `Api` carries what the router cannot know — title, version, servers
  — and nothing else. It is not a place for prose about operations;
  that is stage 3 of specs/openapi.md.
