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

## Stage B — security is declared, ENFORCED, and described

### Why it is not "a header called Authorization"

Two reasons, and the second is the important one.

OpenAPI models authentication as `securitySchemes` + `security` on an
operation, not as a header parameter. Rendering `Authorization` as a
parameter produces a document that validates and lies: a generated
client would put a literal string in a box instead of doing a bearer
flow.

And a declaration that is only DESCRIBED is decoration. Today
`Secure.granted` wraps the finished `PartialFunction` — the router's
output, not its declaration — so the requirement is applied after the
table is built and cannot reach `Entry` at all. If the router does not
enforce what the route declares, the document and the wrapper drift,
and nothing catches it.

### Interface

    val replay = Route / "admin" / "replay" secured Policy.scoped("admin")

    Router.on(Method.Post, replay)(_ => doReplay())   // 401/403 are the router's

### Behavior

- The router answers 401 with `WWW-Authenticate` when the credential
  is absent or unverifiable, 403 when it verifies but the policy
  refuses — the existing ladder, moved from the wrapper into the
  table.
- The entry's `answers` gains those two, so the document says them
  without the author writing anything — exactly as `json[B]`'s 400
  already appears.
- The law: **what the document calls protected is what the router
  refuses.** A test drives every secured entry with no credential and
  asserts 401, and with a wrong scope and asserts 403.

### Out of scope

Replacing `Secure.granted`. It stays for a `PartialFunction` that is
not a `Router`, and okay-admin's conversion is the worked example of
the other road, not a deprecation.

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
