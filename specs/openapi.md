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
- **Path parameters** — yes, from `params`, each with a `kind`
  ("string", "int", …) that maps onto a JSON Schema type.
- **Request bodies** — yes. `Entry.body: Option[Json]` is already a
  JSON Schema, produced by the same derivation the decoder uses, so a
  declared body cannot drift from the parser.
- **Query parameters** — NO. A `Queried` route knows them, the entry
  does not carry them. This is the first gap.
- **Responses** — NO. Nothing in the tree declares a response's status
  or its schema; a handler answers a `Response` built by hand. This is
  the second gap, and the larger one: a document whose every operation
  says only "200, unspecified" is not worth publishing.
- **Summaries, tags, operation ids** — NO, and the third gap is the
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

Stage 0 — the document, from what exists (this lane, after the input
is agreed with the optics-outside owner):
- [ ] `document(api, router)` renders paths, methods, path parameters
      and request bodies for every router entry, as OpenAPI 3.1
- [ ] a path with variables renders as `/board/{id}`, with each
      parameter declared `in: path`, `required: true`, and the type
      its `kind` says
- [ ] an entry with a declared body renders `requestBody` with the
      schema `JsonSchema.of` produced — byte for byte the one the
      decoder uses
- [ ] the renderer's own law: every entry appears exactly once, and
      the document names no path the router does not dispatch
- [ ] the document parses as JSON and round-trips through okay-codec

Stage 1 — responses, which is what makes it worth serving:
- [ ] a route may declare what it answers (status and `Schema`), in
      okay-http, beside the body declaration it already has — the
      shape to agree with the arc's owner, not to invent here
- [ ] `document` renders those; an undeclared operation says so
      rather than claiming 200

Stage 2 — the readers:
- [ ] `routes` serves the document at `/openapi.json` and a page at
      `/openapi` that renders it, self-contained enough to work with
      no network (a CDN reference would make the page a liability in
      an air-gapped deployment; decide it there, in the open)
- [ ] okay-demo serves both, and its document is COMMITTED with a
      drift test, the way `okay-demo/deploy` is

Stage 3 — the prose, last and smallest:
- [ ] a route may carry a summary; the document renders it. Until
      then an operation's id is derived from method and path

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
