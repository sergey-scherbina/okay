# Headers in a route declaration

Operator's ask, 2026-09-11: *"нужен какой-то обобщённый механизм
декларации и работы с заголовками в нашем апи и особенно в роутинге"*.

## Overview

A route declares a path, its parameters, a query and a body. A header
is declared by nobody: a handler reaches into the request and takes
one by name, and every interpreter downstream is blind to it. The
generated OpenAPI document therefore shows `POST /admin/replay` as an
open door, and a caller who believes it gets a 401 the document never
mentioned.

This is the same defect `openapi-parameters` fixed one level up — a
description that stops short of the truth — and it is fixed the same
way: by declaring the thing once and handing the declaration to more
than one interpreter.

**There is already a general unit, and it is not new.** `Route.Named[T]`
is a name and a `Param[T]`, and its own comment says why there is one
type rather than two: *"the OPERATOR says where it goes (`/` into the
path, `:?` into the query)"*. A request header is the THIRD place a
`Named[T]` can go and a response header the FOURTH. Nothing about the
unit changes.

The fifth declaration is not a parameter and is the reason this spec
has three stages rather than one.

## The constraint, stated before the design

**A header cannot join the route's `A`.** The law the arc rests on is

    unapply(url(a)) == Some(a)

— the path a client builds is the path the server matches, from one
declaration. If `A` carried an `Authorization` value, `url(a)` would
have nowhere to put it and `unapply(url(a))` could not recover it. The
law would hold "mostly", which this repository refuses on principle:
`Param.string` already refuses an empty capture rather than let
`unapply(url(a))` hold for most inputs.

So the honest statement is about WHAT THE OPTIC FOCUSES:

- `Routed[A]` is a prism **on the url** — `Prism[String, String, A, A]`,
  and `url`/`unapply`/`prism` belong to it. Untouched by this spec.
- A declaration that mentions headers is a prism **on the request**.
  It is a different optic, and it gets a different type rather than a
  quietly widened old one.

That is why `Routed[A]` does not grow a header field: a route that
"knows" about a header it cannot place in a url is a type whose law is
a half-truth.

## Stage A — a request header is a `Named[T]` in a third place

### Interface

    val events = Route / "events" :@ "last-event-id".opt[String]

    Router.on(Method.Get, events)((_, lastId) => resume(lastId))

`:@` because `@` reads as *at* — metadata carried AT the request rather
than in the url — and because its first character is `:`, so it shares
`:?`'s precedence and stays left-associative (the precedence reasoning
is in `Route.:?`'s own comment and is not repeated here).

It produces `Route.Bound[A, H]`: the url optic `Routed[A]` it wraps,
plus the header declarations and a decoder `Request => Option[H]`. `A`
is the url's tuple, unchanged; `H` is the headers'.

### Behavior

- A REQUIRED header that is absent is a MISS, the same as a required
  query parameter that is absent — the route simply does not match,
  and the caller's 404 stays the caller's. It is not a 400: the router
  does not know that another route would not have matched.
- `opt` is absent-tolerant, `all` collects repeats (a header may
  legally appear more than once).
- Names match case-insensitively, because HTTP header names are
  case-insensitive on the wire and a declaration that only matched
  `Last-Event-ID` and not `last-event-id` would be a trap.
- The law, mirroring the url's: `read(requestWith(h)) == Some(h)` — a
  round trip through actual header lines.

### Design

`Bound` wraps rather than extends `Routed`, so nothing that holds a
`Routed[A]` can be handed something whose law is different. The
router gains overloads that take a `Bound[A, H]` and hand the handler
`(ar.Out, hr.Out)` — the same `Route.Arity` witness that collapses a
one-element url tuple collapses a one-element header tuple, so a
single declared header arrives as its value.

`Described` gains `headers: Vector[Route.H]`, and `Route.H` is `Route.Q`'s
shape (name, kind, required, repeated, schema) because a header
parameter and a query parameter differ only in where they are read.

## Stage B — security is declared, ENFORCED, and described — LANDED 2026-09-11

### Why it is not "a header called Authorization"

Two reasons, and the second is the important one.

OpenAPI models authentication as `securitySchemes` + `security` on an
operation, not as a header parameter. Rendering `Authorization` as a
parameter produces a document that validates and lies: a generated
client would put a literal string in a box instead of doing a bearer
flow.

And a declaration that is only DESCRIBED is decoration. `Secure.granted`
wrapped the finished `PartialFunction` — the router's output, not its
declaration — so the requirement was applied after the table was built
and could not reach `Entry` at all.

### The module boundary, which shapes the interface

`okay-security` depends on `okay-http`, not the other way round. So
`Policy` and `Verified` cannot appear in a route declaration, and
okay-http must not grow an identity model of its own. What it owns is
the HTTP half:

    Route.Security(scheme = "bearer", scopes = Set("admin"), realm = "okay")
    Router.Verify = String => Either[String, Set[String]]

A bearer token in; the scopes it grants, or a refusal. `Secure.verifier`
adapts a deployment's `String => Verified` to it, and the refusal's WHY
crosses the seam only to be dropped — a caller gets a uniform 401
either way, and the reason is the deployment's to log.

**What does not cross is a `Policy`.** A declared route says "these
scopes"; a rule that reads the ACTION or the RESOURCE stays with
`Secure.granted`, which is why both roads exist and neither is
deprecated.

### Interface

    val replay = (Route / "admin" / "replay").secured("admin")

    Router.on(Method.Post, replay)((_, _) => doReplay())
          .enforcing(Secure.verifier(verify))

`secured` produces a `Headed`, for stage A's own reason: a credential
is read from the REQUEST, and `Routed[A]` is the url's prism.

### Behavior

- **Protection does not change WHICH requests a route answers, only
  who gets through.** `matches` is untouched, so a secured route still
  MATCHES a request with no credential and answers 401. This is the
  OPPOSITE of a declared required header, which is a miss — and the
  inversion is deliberate: a route that missed would answer 404 to
  everyone without a token, which leaks less and lies more, and breaks
  the invariant `Secure.bearer` states in its own comment.
- 401 for no credential or one that does not verify, 403 for verified
  and not permitted, each with `WWW-Authenticate`. The WHY of a 401
  stays server-side.
- **The handler never runs for a refused request.** Definedness is
  `matches`, never `run` — the distinction that cost a one-time login
  code its 401 in stage 7 of specs/optics-outside.md.
- The entry's `answers` gains 401 and 403, so the document says them
  without the author writing anything, exactly as `json[B]`'s 400 does.
- **FAIL CLOSED.** A secured entry with no verifier installed does not
  serve: it answers 401 `no_verifier`. Declaring a requirement and
  forgetting `enforcing` would otherwise open a hole the document
  swears is shut, which is worse than having no declaration at all. A
  route that suddenly 401s everywhere is a loud mistake.
- **The law, twice.** In the table: `enforcing` refuses exactly the
  entries whose `security` is non-empty. In the document: the
  operations carrying a `security` key are exactly those entries. Both
  are asserted as set equalities, in okay-http and okay-openapi
  respectively.

### Out of scope, stated

- **Delivering the `Principal` to the handler.** okay-admin's handler
  is `{ _ => ... }` and does not want one; `Secure.granted` keeps the
  ambient-principal road for handlers that do. Widening the declared
  form to carry a principal is a separate decision with its own
  callers.
- **`out`/`jsonOut` on a secured route.** Those take a `Routed`, and
  `secured` produces a `Headed`, so a route cannot yet declare both a
  requirement and a response VALUE. Nothing in the tree wants both;
  when something does, the overloads are mechanical.

### A smell it removes

`Secure.bearer` asks definedness by building a fake principal —
`route(Principal("", "", Claims())).isDefinedAt(r)`. It is harmless
because definedness does not depend on the principal, but it is the
stage-7 family: asking "does this match?" through machinery only the
ANSWER needs. With the requirement on the entry, definedness is
`entries.exists(_.matches(r))` and no dummy exists.

## Stage C — response headers

### Interface

    Router.out(Method.Get, r, status = 200)(h)
          .answering(200, "etag".as[String])

`Answer` gains `headers: Vector[Route.H]`. The 401 that stage B
produces carries `WWW-Authenticate` by construction, which is the
first consumer and the reason this stage is third rather than never:
it falls out of B nearly for free.

## Decisions

- **`Named[T]` is not extended.** Four placements, one unit. A
  placement that needed a different unit would be a sign the
  abstraction was wrong.
- **A required header that is missing is a MISS, not a 400.** A router
  that answered 400 would be claiming no other route could have
  matched, which it cannot know.
- **Security is enforced by the router, not only rendered.** A
  declaration nobody executes is a comment with a type.
- **`Routed[A]` is not widened.** The url optic keeps its law; the
  request optic is a new type. This is the whole content of "what the
  optic focuses", and getting it wrong would cost the arc its only
  load-bearing property.
