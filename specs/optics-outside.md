# Optics on the outside — as an API, not as an implementation

## Overview

`specs/optics.md` is closed. It put optics INSIDE okay: the Json
walks, the Ui tree, `State % S` zooming, and finally `Fuse`, which
takes the run-time tax to zero at compile time. Every consumer there
is ours.

The operator's direction (2026-09-10) is the other direction —
optics, profunctors, arrows and categories as **the vocabulary a user
of the library writes**. That is a different question and it needs a
different test, because most of what can be dressed as an optic
should not be.

**The criterion.** A plain function `S => A` already covers "read
this out". An optic earns its place in a public API exactly when the
same declaration must be given to **more than one interpreter, and at
least one of them DESCRIBES the declaration instead of running it**.
That is the thing a function cannot do: you cannot ask `f: S => A`
what it looks at. A profunctor optic can be asked, because the
profunctor IS the interpreter and `Forget[R]` never runs anything.

So the shape of every candidate below is the same:

| interpreter | profunctor | what it gives the user |
|---|---|---|
| run | `Function1` | `modify`, `set` |
| read | `Forget[R]` | `get`, `preview`, `foldMap` |
| **describe** | `Forget[R]` on a static witness, or a reified route | **SQL, a URL, a schema, an audit, a graph** |

**Why the profunctor encoding and not van Laarhoven.** For an
internal optic it does not matter. For a public one it decides
everything: a new interpreter is a new INSTANCE, so we can add a
generator of OpenAPI, or of an audit, years later and not one line of
any user's declaration changes. It is tagless final, for optics.

**Categories and arrows, honestly.** `Category` (`>>>`, `id`) is a
good public vocabulary almost everywhere and costs nothing — a route,
a pipe and a codec all compose that way. `Arrow` (`first`, `***`,
`&&&`) is a WORSE public vocabulary than a monad for ordinary code,
and pays for itself in exactly one case: when the graph must be known
STATICALLY, before it runs, so it can be drawn, fused or shipped to a
cluster. Where staticness is not needed, an arrow is a tax on syntax
for nothing. This spec will only reach for `Arrow` under that
condition, and says so at the one candidate that meets it.

**And the cost, which optics.md already measured.** Run-time optic
machinery reaches the hand-written byte only through `Fuse`, at
compile time. In a public API the tax is paid by the USER, on the
user's data path. So the candidates split:

- the optic builds a PLAN once (a route table, a SQL string, a
  schema, an audit) — free, it ran at start-up;
- the optic runs PER ELEMENT (redaction of a stream, live diffs) —
  not to be shipped without `Fuse` in front of it.

## The candidates

Ranked by the criterion, all six carried into `BACKLOG.md` under
`optics-outside`.

1. **`optics-outside-routes` — a route is a prism.** One declaration
   answers three questions: does this path match (server), what is
   the path for these parameters (client, reverse routing), and what
   does it look like (OpenAPI, an MCP tool). The prism law is the
   feature: `parse(url(a)) == Some(a)`. **This spec's stage 1.**
2. **`optics-outside-policy` — a projection policy is a traversal.**
   Which fields of a record may be seen, embedded, logged. The audit
   interpreter — "name the fields this policy touches", with no
   document in hand — is the one a function cannot answer, and this
   repository has already paid for not having it: price and contact
   in a summary's text sank a priced offer from 0.63 to 0.13
   (`weighed-not-read-out-of-the-embedding`).
3. **`optics-outside-live` — subscribe to a lens.** The lens compiles
   to a wire path, the server pushes only the focused part, a client
   write comes back as `set`. The most valuable and the most work:
   the optic has to be reifiable and to survive serialisation.
4. **`optics-outside-query` — a query is an optic.** `Forget[Sql]`
   compiles to SQL, `Function1` runs the same predicate over a
   `Vector` in a test, `set` compiles to UPDATE. okay-sql is strings
   plus `Schema` today, so the seat is empty — and typed query DSLs
   are a swamp, which is why this is fourth and not first.
5. **`optics-outside-topology` — a dataflow is an arrow.** The one
   candidate that meets the staticness condition above: the graph has
   to exist as a value before it runs. Wants `ArrowChoice`, or
   branches fall out of the static picture.
6. **`optics-outside-conf` — a setting is a lens that knows its
   path.** The error names `server.tls.port` and the reference table
   generates itself.

## Stage 1 — a route is a prism

### Interface

```scala
package okay.http

/** A path, and the parameters it carries. `A` is a tuple of the
 *  captured parameters, `EmptyTuple` for a fully literal path. */
final class Route[A <: Tuple]:
  def /[B <: Tuple](that: Route[B])(using c: Route.Concat[A, B]): Route[c.Out]
  def /(lit: String): Route[A]

  /** interpreter 1 — MATCH. Also the extractor, so a server writes
   *  `case Post(Users(id)) =>` and gets `id: Int`. */
  def unapply(path: String): Option[A]

  /** interpreter 2 — BUILD. Reverse routing: the client and the
   *  redirect use the same declaration the server matches with. */
  def url(a: A): String

  /** interpreter 3 — DESCRIBE. `/users/{id}/posts/{slug}`, and the
   *  segments as data for OpenAPI or an MCP tool. */
  def describe: String
  def segments: Vector[Route.Seg]

  /** the bridge to `specs/optics.md`: a route IS a prism, so it
   *  composes with everything the core optic already has. */
  def prism: Prism[String, String, A, A]

object Route:
  val root: Route[EmptyTuple]
  def lit(s: String): Route[EmptyTuple]
  def apply[T](name: String)(using p: Param[T]): Route[T *: EmptyTuple]

  enum Seg:
    case Lit(value: String)
    case Var(name: String, kind: String)

  /** a captured segment's codec — the only thing a new parameter
   *  type has to supply */
  trait Param[T]:
    def kind: String
    def parse(s: String): Option[T]
    def print(t: T): String
```

Written out, a declaration reads:

```scala
val userPost = Route.lit("users") / Route[Int]("id") / "posts" / Route[String]("slug")
// userPost.url((7, "hello world")) == "/users/7/posts/hello%20world"
// userPost.unapply("/users/7/posts/hello%20world") == Some((7, "hello world"))
// userPost.describe == "/users/{id}/posts/{slug}"
```

and a server dispatches with the existing convention, unchanged —
`PartialFunction[Request, Response ! Async]`:

```scala
def routes: PartialFunction[Request, Response ! Async] =
  case Get(userPost(id, slug)) => ...
```

`Router` assembles those into one partial function AND a description:

```scala
final case class Router(entries: Vector[Router.Entry]):
  def routes: PartialFunction[Request, Response ! Async]
  def describe: Vector[(Method, String)]   // for OpenAPI / MCP
```

### Behavior

- [x] a fully literal route matches its own path and nothing else
- [x] a captured segment parses by its `Param` and refuses what the
      `Param` refuses (`/users/abc` does not match `Route[Int]`)
- [x] arity is part of the match: a longer or shorter path misses
- [x] **the prism law**: `unapply(url(a)) == Some(a)`, for every
      parameter type, including strings that need escaping
- [x] a segment carrying `/`, a space or a non-ASCII character
      round-trips through percent-encoding, and an encoded `/` does
      NOT become a segment boundary
- [x] `describe` names one `{...}` per captured segment, in order,
      with the names given at the declaration
- [x] `prism` satisfies the same law through `Optic`'s own
      `preview`/`review`, and composes with a core optic
- [x] a `Router` dispatches by method AND path, and misses (no entry)
      leave the partial function undefined — the caller's 404 stays
      the caller's
- [x] `Router.describe` lists every entry, and is derived from the
      same values that dispatch — a route cannot be documented and
      unrouted, or routed and undocumented

### Design

**Why not `Optic` itself.** Path composition ACCUMULATES parameters:
`Route[EmptyTuple] / Route[Int *: EmptyTuple]` is `Route[Int *:
EmptyTuple]`. `Optic.andThen` composes focus-through-focus and
intersects constraints; it has no place to put a growing tuple. The
structure a path needs is a MONOIDAL one — the invertible-syntax
`Syntax` of Rendel & Ostermann, not a lens composition. So `Route` is
its own type, and `prism` is the honest bridge back: for a FIXED `A`
a route is exactly a prism `String <-> A`, and that is where the law
is stated and tested.

**`Concat` is a typeclass, not `Tuple.Concat`.** The match type
`Tuple.Concat[A, B]` composes fine in the forward direction and
cannot be taken apart again: printing needs `A` and `B` back out of
`c.Out`, and the compiler will not prove
`Concat[Concat[A1,A2],R] = Concat[A1,Concat[A2,R]]`. An `asInstanceOf`
would close that hole, and the operator's rule forbids it
(`AGENTS.md`, "NO CAST WITHOUT A REAL NECESSITY"). The typed route is
an inductive typeclass carrying BOTH directions:

```scala
trait Concat[A <: Tuple, B <: Tuple]:
  type Out <: Tuple
  def join(a: A, b: B): Out
  def split(o: Out): (A, B)
```

with two instances — `EmptyTuple` and `H *: T` — which is how tapir's
`ParamConcat` does it, and it costs nothing at run time beyond the
allocation of the tuple the user asked for.

**Percent-encoding is ours, not the JDK's.** `java.net.URLEncoder` is
JVM-only and okay-http is cross-platform; it also encodes for a query
string (`+` for a space), which is wrong inside a path. `Route`
encodes a SEGMENT: unreserved characters through, everything else as
UTF-8 `%XX`. Decoding happens per segment, AFTER the split on `/`, so
an encoded `%2F` inside a parameter can never be read as a boundary —
the defect every hand-written router in this repository is currently
open to.

**Method stays out of `Route`.** A route is a path, and the prism law
is a statement about paths. The verb belongs to the dispatch table,
where `Router` puts it, and to the `Get`/`Post` extractors, which are
one line each and compose with any route.

### Out of scope for stage 1

Query parameters, headers and bodies (stage 2 — same structure, a
different monoid); a wildcard tail segment; OpenAPI rendering itself
(stage 3 — `describe` is the input it needs, and rendering is a
different module's job); content negotiation.

## Decisions

- **2026-09-10** — the arc's criterion (an interpreter that
  DESCRIBES) is written before any candidate, because it is what
  disqualifies the pretty ones. It is the reason routes come first
  and a typed query DSL comes fourth.

## Results

### Stage 1 — LANDED 2026-09-10 (optics-outside-routes)

`okay-http/src/main/scala/okay/http/Route.scala`, 20 tests in
`TestRoute`, green on JVM and JS. `docs/modules/okay-http.md` has the
user-facing half.

**It shipped with a caller, deliberately.** The optics arc closed with
an honest finding recorded twice in CHANGELOG — the aggregating
families had no production consumer — and a PUBLIC api with none would
repeat it worse. `Acceptance.routes` was three
`case r if r.url.startsWith("/person")` guards; it is now a `Router`,
served unchanged by all four backends (the JDK's, Jetty, Netty, NIO),
and the conversion FOUND two defects that had been there the whole
time: `startsWith("/person")` also answered `/personal`, and every
route answered every verb, so a POST to `/person` was served the JSON
body. `TestRoute` asserts both refusals.

**Two things the design had to settle, and did.**

`Concat` had to be a typeclass. `Tuple.Concat` composes forwards and
will not come apart: `url` needs `A` and `B` back out of `c.Out`, and
the compiler cannot prove `Concat[Concat[A1,A2],R] =
Concat[A1,Concat[A2,R]]`. An `asInstanceOf` would have closed it in
one line and is exactly what AGENTS.md forbids; the inductive witness
carries `join` and `split` together and costs nothing at run time.

A handler needs the REQUEST, not only the path. The first cut had
`on(method, route)(h: A => Response ! Async)` and could not express
`/echo`, whose whole job is to read the body — a router whose handlers
cannot see the request is a demo, not an API. `at` is the primitive
and `on` is defined in terms of it.

**One property is not total, and is refused rather than papered
over.** An empty path segment is not a value: `"/x//y"` and `"/x/y"`
would build the same URL from different parameters. `Param.string`
refuses the empty capture, so `unapply(url(a)) == Some(a)` holds on the
domain instead of holding "mostly".

**What the prism buys.** `route.prism` hands a route to the core
optics as `Prism[String, String, A, A]`, and `set` on a matching path
rebuilds the path from new parameters. `put` on a MISS is the whole
unchanged — a prism's set, not its review — which the test asserts as
the shape rather than working around.
