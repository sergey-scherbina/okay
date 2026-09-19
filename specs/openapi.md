# OpenAPI: the document as a rendering, and who reads it

## Overview

The operator asked for OpenAPI/Swagger support (2026-09-11). The
board already governs how to start it, and the rule is not
negotiable: BACKLOG's `optics-outside-describe` says **do NOT start
this as "generate OpenAPI"** — name the consumer first, "or it lands
with no caller — the trap this arc has now recorded three times."

So this spec opens with the consumer, states what the input can
support TODAY, names the three gaps that stand between that and a
document worth publishing, and only then lists stages.

The material is already here and is not in question: `Router.entries`
carries a method, a path and — since optics-outside stage 7 — the
request body's JSON Schema; `Route` carries `describe`, `params` and
its query declarations; okay-codec's `JsonSchema.of[A]` renders a
`Schema[A]` as JSON Schema, and has for tool declarations since
`Toolbox`. An OpenAPI document is a fold over those. Writing the fold
is the small part.

## Who reads it

Three candidate readers, honestly ranked. The first is the one this
spec builds for; the others are recorded so a later lane does not
re-argue them.

**An integrator outside this repository** — someone writing a client
against a service built here. They are the real reader and they are
not in the tree, so the in-repo proxy for them is a COMMITTED
document with a drift test, the shape this repository already trusts
for deployments: `okay-demo/deploy` is a rendering of a value and a
test refuses a drift between them. A committed `openapi.json` makes an
API change visible in review as a diff, which is the thing prose
cannot do.

**A person with a browser.** The service serves the document and a
page that renders it, so "what does this API answer" is a URL rather
than a question for a maintainer. This is a genuine consumer, and it
is the one that makes the document load-bearing rather than
decorative — an unserved document rots.

**A conformance check** — every entry the router dispatches appears
in the document and nothing else does. This one is tempting and is
NOT a consumer: it compares the renderer with its own input. It is
worth having as a test of the renderer, not as a reason for it.

What is deliberately not a reader here: code generation, in either
direction. A client for a service built here is already typed by its
`Route` values, and generating Scala from a document would be a
second source of truth for the thing this repository keeps in one.

## What the input supports today, exactly

Measured against `Router.Entry` as it stands (method, path, matches,
run, body):

- **Paths and methods** — yes. `Entry.path` is the described form,
  and `Route.params` names the variable segments with their kinds.
- **Path parameters** — names AND kinds, since
  `openapi-parameters`. The entry carries `Route.Described`, not a
  template string, so `Route[Int]("id")` renders as an integer. This
  paragraph has now been wrong twice in opposite directions: the spec
  first claimed the kinds were available, stage 0 measured that they
  were not, and the fix was to stop flattening the description at the
  router's door rather than to teach the renderer to guess.
- **Request bodies** — yes. `Entry.body: Option[Json]` is already a
  JSON Schema, produced by the same derivation the decoder uses, so a
  declared body cannot drift from the parser.
- **Query parameters** — YES, since `openapi-parameters`, with the
  `required` each declaration gave and an `array` schema where `all`
  made one repeatable. They are NOT part of the path template: the
  path is what dispatches and a query is a separate list, which is
  the shape OpenAPI wants and the split `describe`/`describeFull`
  already made.
- **Responses** — YES since openapi-responses, for a handler that
  answers a VALUE. The declaration is the handler's type: the router
  encodes with the same `Schema` the entry carries. A handler that
  builds its own `Response` declares nothing, and the document says
  so; that is a choice the author makes per route rather than a hole
  in the model.
- **Headers** — NO, and this is now the only structural gap. A route
  declares a path, parameters, a query and a body; a header is read
  off the `Request` with nothing declared, so the renderer says
  nothing rather than guessing.
- **Summaries, tags, operation ids** — NO, and this gap is the
  smallest: a route has no place to carry a sentence about itself.

The order those gaps matter in is responses, queries, prose — and the
first two are declarations in okay-http, which belongs to the
optics-outside arc rather than to this one.

## The model

A document is a value, rendered:

```scala
final case class Api(title: String, version: String, servers: Vector[String] = Vector.empty)

object OpenApi:
  /** the document for a router, as okay-codec Json */
  def document(api: Api, router: Router): Json
  /** the two routes that serve it: the document and a page that renders it */
  def routes(api: Api, router: Router, at: String = "/openapi.json"): Router
```

**OpenAPI 3.1, not 3.0**, and the reason is mechanical rather than
fashionable: 3.1's schema dialect IS JSON Schema, so
`JsonSchema.of[A]` drops straight in. Under 3.0 every schema would
need a translation layer with its own drift.

The document is `okay.codec.Json`, not a case-class model of OpenAPI.
A model would be a third vocabulary to keep in step with a
specification this repository does not own, and nothing here consumes
it as types — it is written out and served.

## Behavior

Stage 0 — the document, from what exists — SHIPPED (openapi-render,
okay-openapi):
- [x] `document(api, router)` renders paths, methods, path parameters
      and request bodies for every router entry, as OpenAPI 3.1
- [x] a path with variables renders as `/board/{id}`, with each
      parameter declared `in: path`, `required: true` — and `type:
      string`, because the kind does not reach the entry (above)
- [x] an entry with a declared body renders `requestBody` with the
      schema `JsonSchema.of` produced — byte for byte the one the
      decoder uses
- [x] the renderer's own law: every entry appears exactly once, and
      the document names no path the router does not dispatch
- [x] the document parses as JSON and round-trips through okay-codec
- [x] an undeclared operation SAYS SO in its `responses` rather than
      claiming a `200` nobody promised
- [x] operation ids are derived from method and path, stable and
      distinct

Stage 1 — responses — SHIPPED (openapi-responses):
- [x] a route declares what it answers by the handler's TYPE, not by
      an annotation beside it: `Router.out`/`outAt`/`jsonOut`/
      `jsonOutAt` take a handler answering a VALUE, and the router
      encodes it with the same `Schema` the entry carries — so a
      document cannot promise what the service does not send
- [x] the router declares the failures IT produces: `jsonOut`'s 400
      for a body that does not parse, with the error schema, without
      the author writing anything
- [x] a declared status other than 200 is the status sent and the
      status rendered
- [x] `document` renders them, sorted by status, each with its schema
- [x] a handler that builds its own `Response` (`on`, `at`) still
      declares nothing, and the document says exactly that

Stage 2 — the readers — SHIPPED (openapi-serve):
- [x] `OpenApi.routes(api, router)` serves the document at
      `/openapi.json` and a page at `/openapi`
- [x] the page is RENDERED ON THE SERVER: no JavaScript and no CDN,
      decided in the open — this repository renders deployments for
      machines with no network, and a page that cannot explain the API
      there is not documentation there. The cost is no "try it"
      button; the page carries the paths, parameters, bodies and
      declared answers as HTML
- [x] okay-demo serves both, from the same router that answers its
      requests
- [x] `okay-demo/openapi.json` is COMMITTED and a drift test refuses a
      difference — `sbt "okayDemo/runMain okay.demo.DemoOpenApi"`
      regenerates it, the way DemoDeploy does for the deployment
- [x] and the stronger check the drift test alone would not make:
      every path the document names is a path the service answers

Stage 2b — the media (openapi-media), which stage 2 exposed — SHIPPED:
- [x] `Router.Answer` carries a MEDIA TYPE, bare (`text/html`, not
      `text/html; charset=utf-8`): the key a document files an answer
      under is a kind of content, and a charset is a detail of one
      response
- [x] `Router.html`/`bytes`/`events` — and `media`, which the three
      are written in terms of and which is public, because the list of
      media types is not ours to close — declare by construction the
      same way `out` does: the handler answers the CONTENT and the
      ROUTER writes the content-type, from the same value the entry
      declares
- [x] the charset is added only where the router did the encoding
      (`html` takes a `String`); handed bytes, it says nothing it
      cannot know
- [x] each takes a `description`, so an answer has one true sentence
      without waiting for stage 3
- [x] the renderer keys `content` by the entry's media, and an answer
      with no schema still gets a content block: `{}` says "this media
      type, shape unstated", while no content at all says "no body"
- [x] the page shows the media type beside the status
- [x] okay-demo declares all six of its operations, and its committed
      document contains the word `undeclared` zero times — a test
      holds that, and a second one checks on the REAL service that
      what the document files an operation under is the content-type
      the service sends

What stage 2b deliberately does NOT do: an answer whose STATUS is
chosen per request. Every declaring combinator fixes the status where
the route is declared, and okay-ops's `/healthz` answers 200 or 503 by
what it finds. Two declared answers with a handler that picks, or an
answer type carrying its status, are both real designs; neither has a
consumer yet, and okay-ops is the one that will name it.

Stage 3 — the prose, last and smallest — SHIPPED (openapi-prose):
- [x] an entry may carry a SUMMARY, and it is the one part of a
      declaration that cannot be derived: a path comes from the route,
      a parameter's kind from its `Param`, an answer from the
      handler's type. `Router.summarised(text)` attaches it to the
      entry just declared — one builder method rather than a parameter
      on each of twenty combinators
- [x] summarising an empty router THROWS where the table is built: a
      builder method that silently did nothing would put the sentence
      on no operation at all
- [x] the document renders `summary` when there is one and omits the
      field when there is not — an empty string would promise prose
      nobody wrote. The derived operation id stays either way
- [x] the page shows it under the method and path
- [x] `out`/`outAt`/`jsonOut`/`jsonOutAt` take the `description` the
      media combinators already had, so an answer can stop reading
      "the declared answer"
- [x] okay-demo summarises all six operations, and a test refuses a
      published operation with no summary (or one under ten characters)
- [x] found on the way: `out` and its three siblings were reachable
      only through `Router.empty`, since the companion mirrored only
      `on`/`at`/`json`/`of`. Every declaring form can begin a table now

## Decisions

- **Consumer first, and named.** The integrator, proxied in-repo by a
  committed document plus a drift test, and the browser. The
  conformance check is a test of the renderer, not its reason.
- **A separate module, `okay-openapi`** (okay-http + okay-codec). The
  document vocabulary does not belong in okay-http, which is about
  serving; and okay-http must not gain a dependency for something
  only some of its users want.
- **3.1 for the schema dialect**, so okay-codec's renderer is the
  whole schema story.
- **`Json`, not a typed model of the specification.**
- **No code generation, in either direction.**
- **The gaps are named before the stages** — responses above all. A
  lane that renders paths and calls itself OpenAPI support would be
  the same "no caller" trap in a new coat.

## Out of scope

- generating clients or servers from a document; validating requests
  against one; OpenAPI 3.0; a typed model of the specification;
  anything that would put the document's vocabulary into okay-http
