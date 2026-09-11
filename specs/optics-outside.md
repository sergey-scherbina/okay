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
  def /[B <: Tuple](that: Route[B])(using c: Route.Split[A, B]): Route[c.Out]
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
val userPost = Route / "users" / Route[Int]("id") / "posts" / Route[String]("slug")
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

**`Split` is a typeclass, not `scala.Tuple.Concat`.** The standard
library gives the forward direction and only that — the match type,
and `++` for the values. Nothing takes a tuple APART again, and `url`
needs `A` and `B` back out of `c.Out`. `split` is therefore the whole
addition; `join` could have been `++`.

The reason not to build on `Tuple.Concat` at all is sharper than
"the compiler will not prove associativity", which is only the
symptom. It is that the match type does not NORMALISE. Set
`Out = Tuple.Concat[A, B]` and the induction resolves once; the
accumulated type is then itself a `Tuple.Concat[A1, A2]`, which is
stuck while its arguments are abstract, has no head to peel, and so
the NEXT `/` in the chain cannot resolve at all. `Out = H *: c.Out`
is a cons chain at every step, and a cons chain can always be peeled.

An `asInstanceOf` would close the hole in one line, and the operator's
rule forbids it (`AGENTS.md`, "NO CAST WITHOUT A REAL NECESSITY") —
rightly here, because the cast would have to take its split index from
`segments.length`, a different field, making correctness depend on an
invariant (`segments.length` equals the arity of `A`) that nothing
enforces and that stage 3's wildcard tail would break. The typed route
is an inductive typeclass carrying BOTH directions:

```scala
trait Split[A <: Tuple, B <: Tuple]:
  type Out <: Tuple
  def join(a: A, b: B): Out
  def split(o: Out): (A, B)
```

with two instances — `EmptyTuple` and `H *: T` — which is how tapir's
`ParamConcat` does it, and it costs nothing at run time beyond the
allocation of the tuple the user asked for. The chain of instances is
exactly as long as `A`, which is how "where to cut" is answered
without counting anything at run time. The refinement in each given's
declared TYPE (`{ type Out = ... }`) is load-bearing: without it
`c.Out` stays abstract at the call site and a user sees `Route[c.Out]`
instead of `Route[(Int, String)]`. It was called `Concat`
until an hour after it landed, when the operator read the name and
took it for the standard library's; `Split` says what it actually
adds, since `join` could have been `++`.

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

## Stage 2 — a tool is one declaration, not three

### Why this one, and not the six

The six candidates were ranked before any of them had been looked for
in the tree. Looking for stage 1's successor found something better
than the ranking: the repository's worst case of the exact defect this
arc is about is a TOOL declaration, and it is not on the list.

`BoardTools` (okay-demo) declares four tools. Each is written three
times:

```scala
ToolSpec("board_add", "Add a task ...", schema("text" -> "string", "owner" -> "string"))
...
"board_add" -> (c => for t <- str(c, "text"); o <- str(c, "owner") yield ...)
```

- the JSON Schema is hand-written, by a local `schema(...)` helper;
- the dispatch table re-reads the same field names as string
  LITERALS (`str(c, "text")`), so a rename breaks the tool silently;
- the tool's name is written twice, in two structures keyed by it, so
  a tool can be declared and undispatched or dispatched and
  undeclared — the property `Router.describe` was built to make
  impossible.

And the derivation already exists and is unused: `ToolSpec.apply[A]`
and `ToolSpec.args[A]` both take ONE `Schema[A]`, and
`okay.codec.JsonSchema` is documented as "the FOURTH algebra over
`Schema[A]` ... so a tool's signature cannot drift from its parser".
`Schema` is the optic algebra `specs/optics.md` named in its own
Overview. So this stage is the arc's thesis with the interpreters
already written: DECLARE (`JsonSchema.of`), DECODE (`Codecs.json`),
DISPATCH (the handler) — three interpretations of one value.

### Interface

```scala
package okay.agent

final class Tool[A]:
  val name: String
  val description: String
  val spec: ToolSpec                  // DECLARE — derived, not written
  def handle(c: ToolCall): String     // DECODE with the same Schema, then run

object Tool:
  /** the usual kind: the argument type IS the declaration */
  def apply[A](name: String, description: String)(run: A => String)(using Schema[A]): Tool[A]

  /** a tool whose argument is arbitrary JSON — the schema is given,
   *  because there is no case class to derive it from */
  def raw(name: String, description: String, schema: Json)(run: Json => String): Tool[Json]

final class Toolbox:
  def add[A](t: Tool[A]): Toolbox
  def specs: Seq[ToolSpec]                      // what the model is told
  def table: Map[String, ToolCall => String]    // the existing seam, unchanged
  def call(c: ToolCall): Option[String]
  def duplicates: Vector[String]
```

`table` keeps the type `Mcp.Server`, `Handlers.tools` and `Stepper`
already take, exactly as stage 1 kept
`PartialFunction[Request, Response ! Async]`. Nothing downstream
changes.

### Behavior

- [x] a tool's spec is derived from its argument type: the properties
      are the fields, and `required` names every field that is not an
      `Option` and has no default
- [x] the SAME `Schema` decodes the call, so a renamed field cannot
      leave the declaration and the handler disagreeing
- [x] a call whose arguments do not decode answers with DATA naming
      the tool, not an exception — a model given an exception learns
      nothing
- [x] `specs` and `table` are drawn from one vector, so their name
      sets are equal by construction
- [x] a duplicate name is the one inconsistency left (a `Map` keeps
      the last, a `Seq` keeps both) and `duplicates` names it
- [x] a raw tool keeps the schema it was given and passes the
      arguments through untouched
- [x] `BoardTools` and `StateMcp` are declared this way, and their
      existing suites pass unchanged

### Design

**`Tool.raw` is not a wart.** `StateMcp`'s three tools take a JSON
Merge Patch — arbitrary JSON by definition, with
`additionalProperties: true`. There is no case class to derive from,
and inventing one would be a lie about the protocol. What `raw` still
buys is the PAIRING: the name is written once and the spec and the
handler cannot come apart. Being honest about which half of the win
applies is the point of having the constructor at all.

**The declared schema changes, and that is the finding.** The
hand-written schemas never declared `required` — the model was never
told that `board_add` needs both `text` and `owner`. The derived one
does. `board_assign`'s `id` also goes from `"number"` to `"integer"`,
which is what a `Long` is. Both are more accurate and both are prompt
text (`jsonschema-render-is-prompt-text`), so the demo suites run
before the gate.

### Out of scope

Effectful handlers (`ToolCall => String ! Rest`): `Handlers.gated` and
`relayTools` already take the pure table, and widening the seam is a
separate decision with its own callers. Tool RESULTS as typed values
rather than `String` — the same, and the MCP wire says string.

## Stage 3 — the query string

### Why before a renderer

Stage 1's DESCRIBE interpreter has no consumer that renders anything —
which is the "no production caller" trap this arc names twice already.
A renderer (OpenAPI, or an MCP tool declaration) is the obvious next
lane, and it would be immediately out of date if a route could not
describe the rest of a request. So the query comes first.

It is also not filler. A path is ORDERED and a query is not, so this
is where `unapply(url(a)) == Some(a)` first has to survive a shape
where many urls mean one value.

### Interface

```scala
val search = Route / "search" :? Query[String]("q") +& Query.opt[Int]("page")
// Route[(String, Option[Int])]

final class Query[B <: Tuple]:
  val declared: Vector[Route.Q]
  def +&[C <: Tuple](that: Query[C])(using Route.Split[B, C]): Query[?]

object Query:
  def apply[T](name: String)(using Route.Param[T]): Query[T *: EmptyTuple]        // required
  def opt[T](name: String)(using Route.Param[T]): Query[Option[T] *: EmptyTuple]  // optional
  def all[T](name: String)(using Route.Param[T]): Query[Vector[T] *: EmptyTuple]  // repeated

final case class Route.Q(name: String, kind: String, required: Boolean, repeated: Boolean)
```

`describe` stays the PATH — the shape OpenAPI wants, with query
parameters as a separate list — and `describeFull` renders both for a
human.

### Behavior

- [x] a query parameter is read by NAME, so wire order is irrelevant
- [x] an absent optional parameter is `None`, and `None` writes nothing
- [x] a missing required parameter is a miss
- [x] present and unparseable is a MISS, not `None`
- [x] unknown query parameters are ignored
- [x] a repeated parameter keeps every value in wire order; empty
      writes nothing
- [x] the prism law survives the query, including values containing
      `&`, `=`, `+`, spaces and non-ASCII
- [x] a raw `+` in a value is a space, and a literal `+` round-trips
- [x] a fragment belongs to neither the path nor the query
- [x] a `Router` dispatches a route that reads the query, with the
      required parameter part of the match

### Design

**The operators are `:?` and `+&`, and that is precedence rather than
taste.** Scala takes an infix operator's precedence from its FIRST
character, and `?` is in the "all other special characters" group,
which binds TIGHTER than `/`. So `Route / "search" ? q` parses as
`Route / ("search" ? q)` and does not compile; the first cut only
worked because `Route.lit("search")` was a complete expression to the
left of it. `:` is below `/`, and `+` sits between them, so
`Route / "search" :? a +& b` groups exactly as it reads and a whole
declaration needs no parentheses. Associativity comes from the LAST
character, so `:?` remains left-associative. These are http4s's
operators; this is the reason they are the ones they are.

`Route / "users"` is the companion's own `/`, so a declaration never
has to start with `Route.lit(...)` or `Route.root`.

**`Query` is its own type, not more `Route` combinators.** The path is
ordered and the query is not, and mixing them would put the split
between the two halves of the tuple somewhere that depends on
declaration interleaving. As one `Query` handed over by `?`, the split
is exactly one `Split` and the path's parameters keep their positions
whatever the query does.

**The prism becomes visibly a prism.** `url` writes the query in
DECLARATION order, so it picks the canonical url among the many the
route accepts. `unapply(url(a)) == Some(a)` holds; `url(unapply(u))`
normalises `u` and is not the identity. That was already true of a
path (`/users/007`), but the query makes it unmistakable.

**Present and unparseable is a miss.** `?page=abc` on an `Int` meant
something and got it wrong. Treating it as an omission hides the
caller's mistake behind a page of results.

**Where the empty-value rule lives — a bug the query exposed.**
`Param.string` refused the empty string, so that an empty PATH segment
could not build the same url from different parameters. That is a rule
about the POSITION, not about the type, and attaching it to `Param`
broke the query on the day the query arrived: `?tag=` is a perfectly
good empty value and `Query.all[String]` must round-trip one. The
refusal moved into the path capture, where it belongs. It surfaced
only because a second consumer of the same `Param` appeared — which is
the general lesson worth keeping.

## Stage 4 — the DESCRIBE interpreter gets a consumer

### The consumer, and the correction that shaped it

Stage 1 built `describe` and nothing has read it since. The obvious
next lane is a renderer, and "generate OpenAPI" is exactly the lane
with no caller this arc has now recorded three times. So the consumer
came first, from the tree rather than from the imagination.

A probe path lives in three independent string literals:

```scala
// okay-ops/Ops.scala          — serves it
case r if r.method == Get && r.url == "/healthz" => ...
// okay-deploy/Deploy.scala    — names it, and renders it into SIX targets
final case class Health(livenessPath: String = "/healthz", readinessPath: String = "/readyz", ...)
// okay-script/ScriptDeploy.scala — a third pair, by hand
health = Health(livenessPath = "/healthz", readinessPath = "/healthz")
```

The first draft of this section said the three had already diverged,
because `Site.opsRoutes` serves no `/readyz` while `ScriptDeploy`
names readiness at `/healthz`. **That was wrong, and reading the file
rather than grepping it is what corrected it**: the choice is
deliberate and explained in place — a Site is ready when it is live,
because its pages compile before the port binds.

The real defect is narrower and worse: nothing would NOTICE a
divergence. Rename an ops path in `Site` and the manifest keeps the
old literal; Kubernetes finds out first, by restarting the pod. And
the sharpest form of the argument is that okay-script already holds
this principle for SETTINGS and never extended it to paths — its own
comment says they "are DERIVED from the value the program itself
reads ... a name this deployment could invent does not exist".

### Interface

```scala
object Ops:
  val healthz: Route[EmptyTuple] = Route / "healthz"
  val readyz:  Route[EmptyTuple] = Route / "readyz"
  val stats:   Route[EmptyTuple] = Route / "stats"
  val metrics: Route[EmptyTuple] = Route / "metrics"

  def router(store: Store, ...): Router     // the four, declared once
  def routes(store: Store, ...): PartialFunction[Request, Response ! Async] = router(...).routes
  val paths: Set[String]                     // what a deployment names its probes from
```

`Site` gains the same shape for its own three, and `ScriptDeploy`'s
`Health` names paths taken from those values rather than from
literals.

### Behavior

- [x] `Ops.routes` answers exactly what it answered before, and the
      existing suites pass unchanged
- [x] `Ops.router(...).describe` and `Ops.paths` agree — the audit and
      the dispatch cannot drift, which is the law this stage exists for
- [x] `Site.opsRouter` and `Site.opsPaths` agree the same way
- [x] every probe path `ScriptDeploy`'s `Health` names is a path
      `Site` actually serves
- [x] a query string no longer defeats an ops route (`/healthz?x=1`
      matched in `Site`, which used `pathOf`, and MISSED in `Ops`,
      which compared the whole url — the two disagreed, and now do not)

### Design

**okay-deploy does not learn about okay-http, and that is the right
answer rather than a workaround.** `okay-deploy` depends on
`okayCodec` and `okayConf` only; it renders manifests and has no
business knowing what a `Route` is. So the coupling is enforced in the
module that already has BOTH sides — okay-script, which serves the
paths and writes the manifest. A dependency added to make a check
convenient would cost more than the check is worth.

**`paths` is derived from the route values, and a test proves it
matches the router.** The alternative — deriving `paths` from
`router(...)` — needs a `Store` that a deployment has no reason to
build, which is the same phantom requirement stage 2 caught in
`BoardTools.specs`. The honest shape is: values are the source, the
router is built from them, and a test asserts the two agree.

## Stage 5 — the declaration form, decided by the compiler

Four rounds of "could it be shorter?" from the operator. Every
proposal was put to the compiler rather than argued about, and three of
the answers were NO — which is the useful half, because each no is a
rule about the language rather than an opinion about style. The probes
are in the session scratchpad; what they settled is here.

### What the language refuses, and why

**`:?[String]("q")` and `:?[String] "q"` do not parse.**

```
val a = base :?[String]("q")
                 ^ expression expected but '[' found
```

Scala takes an infix operator's precedence from its FIRST character
and does not accept a type argument after one: the parser wants an
operand there. This is why the earlier `Route.lit("search") ? …`
worked at all — the receiver was a complete expression to the left.

**The type argument may ride the OPERAND** — `"q"[String]` is
`"q".apply[String]`, a plain `SimpleExpr1 TypeArgs`, which parses where
`:?[String]` does not. **But `apply` in particular is unreachable**, and
that took a second measurement to learn: `String` ALREADY has an
`apply` through `StringOps`, and an extension method is only consulted
when the member does not exist. `"id"[Int]` compiled in the isolated
probe, where nothing competed, and failed the moment it met the real
imports — with a message about `scala.collection.StringOps`, which
names the winner rather than the problem.

So the hole is `"id".as[Int]`. The loss is one dot; the gain is that
all three forms are ordinary named methods (`as`, `opt`, `all`) instead
of one magic `apply` beside two named ones — which is the more
teachable API anyway. The general lesson is worth keeping: **a probe
that isolates the feature also isolates away its competitors**, and an
extension on a type as populated as `String` has many.

**`Router./` as the name of the empty table is declined.** Not taste:
`/` already means "append a path segment" in this API, and one symbol
with two unrelated meanings is the mistake the `Concat`/`Split` rename
was made to avoid, in the other direction.

### The form

```scala
import okay.http.syntax.*    // opt-in: the extensions live behind it

val userPost = Route / "users" / "id".as[Int] / "posts" / "slug".as[String]
val search   = Route / "search" :? "q".as[String] :? "page".opt[Int]
val tagged   = Route / "posts" / "id".as[Int] :? "tag".all[String]

val router = Router
  .on(Method.Get, healthz)(_ => text(200, "live"))
  .at(Method.Post, userPost)((p, r) => …)
```

The rule is one sentence: **a bare string is a literal segment,
`"name".as[T]` is a hole.** The type argument is exactly what turns a
literal into a capture, and the operator says where the parameter goes
— `/` into the path, `:?` into the query. One operand type, `Named[T]`,
because a named typed parameter IS the same thing in both places.

`Route[Int]("id")` and `Query[String]("q")` stay. They name the
concepts aloud, which is what a reader meeting the API wants; the
terse form is for the reader who already knows. The extensions are
behind an import because an `apply[T]` on `String` reaches every string
in scope, and a stray `"abc"[Int]` should not produce an error about
`Param` in a file that never asked for any of this. The object is
`syntax` rather than `query`, because a `query` object beside the
`Query` class differs from it only in case and the two class files
overwrite one another on a case-insensitive filesystem — the compiler
warned, and this repository allows no warnings.

`:?` chains, so `+&` leaves the common case and keeps its real job:

```scala
val paging = Query.opt[Int]("page") +& Query.opt[Int]("size")
val posts  = Route / "posts" :? paging
val users  = Route / "users" :? paging
```

### A path may not follow a query, structurally

`:?` answers a `Queried[A]`, which has no `/` at all. `Routed[A]`
carries what both stages can do — `unapply`, `url`, `describe`,
`params`, `queries`, `prism`, `of` — and `Router` takes a `Routed`.

**The refusal is structural on purpose, and the alternative was tried
and rejected by evidence.** To improve the MESSAGE, a poisoned
`/` carrying `@compileTimeOnly` was added to `Named`, since precedence
sends the mistake there (`/` binds tighter than `:?`, so a segment
written after a query parameter attaches to the parameter, not to the
route). The annotation fired in five isolated variants — dotted,
infix, overloaded method, overloaded caller, extension receiver,
parenthesised and not — and silently did NOT fire in the real
expression, even after a clean rebuild. Worse, its presence made two
invalid declarations compile, because the poisoned method returned a
usable type where absence had produced an error.

So the message stays imperfect and the refusal stays guaranteed:
without a `/` on `Named`, the mistake is "value / is not a member of
Named[String]" — the right answer with a confusing locus, which is the
correct trade. An error mechanism that protects "usually" is worse
than none, because it removes the real barrier in exchange for prose.

### Behavior

- [x] `"id".as[Int]` in a path position captures, and `Route[Int]("id")`
      still does the same thing
- [x] `"q".as[String]`, `"page".opt[Int]`, `"tag".all[String]` in a query
      position, matching `Query[String]`, `Query.opt`, `Query.all`
- [x] the extensions are NOT in scope without their import
- [x] `:?` chains, and `+&` still composes a query value
- [x] a path segment after a query does not compile, in both
      groupings (bare infix, and parenthesised)
- [x] `Router.on/at/of/ofAt` and `Toolbox.on/raw` start a table from
      the companion; `empty` remains the zero of a fold
- [x] every existing declaration in the repository still compiles

## Stage 6 — a route's parameter names are the case class's field names

`Route.Of[C]` maps the route's tuple into a case class with
`m.fromProduct`, which is POSITIONAL. The TYPES are checked, so a real
mismatch is caught by the compiler — but the names are not compared at
all, and

```scala
val r = Route / "users" / "id".as[Int] / "posts" / "slug".as[String]
r.of[Wrong]      // case class Wrong(a: Int, b: String) — accepted, silently
```

fits. Positional mapping is type-safe and yet says nothing about
whether the declaration means what it appears to mean, and `describe`
publishes those names to OpenAPI and to MCP tool schemas, where being
wrong is not cosmetic.

### The check, and the honest shape of it

`of[C]` becomes `inline`, reads `MirroredElemLabels` with
`constValueTuple`, and compares them with the route's own parameter
names — the path's, in order, then the query's:

```
route parameters (id, slug) do not match the field names (a, b)
```

**It is a run-time check at construction, not a compile-time one, and
that is a property of the problem.** The labels are type-level; the
route's names are values in `Vector[Seg.Var]`, put there by
`Route[T](name)` and `"name".as[T]`. There is nothing for the compiler
to compare. Making it compile-time means lifting the names INTO the
type — `Route[A, N <: Tuple]` with a tuple of singleton strings, and an
`inline def as` that keeps `name.type` — which puts a second type
parameter on every signature and in every user annotation. That is the
cost already declined once in stage 5, where the path/query stages
became two classes rather than one phantom parameter, and it buys less
here: a route is declared as a `val`, so the check fires at class
initialisation. Start-up, before the first request, or never.

**Why refuse rather than report.** `Toolbox.duplicates` returns data
instead of throwing, and that is right there: a duplicate tool name is
something a caller can inspect and decide about. A name mismatch is
different — the mapping it produces is WRONG, there is no sensible way
to continue with it, and the value being constructed is a declaration
rather than a request. Refusing at construction is the same discipline
as a `require` on a constructor argument.

### Behavior

- [x] a route whose parameter names are the class's field names, in
      order, is accepted
- [x] a route whose names differ is refused, and the message names
      both sides
- [x] the query's parameters count too, and in tuple order (path
      first, then query)
- [x] a route with no parameters and a class with no fields is fine
- [x] every existing `of[C]` in the repository still works

## Stage 7 — the request body is part of the declaration

### The consumer, and the defect it was already carrying

Found before the feature, which is the order stage 4 settled. okay-demo
picks its login fields out of the body by string literal:

```scala
case r if r.method == Post && r.url == "/login/confirm" =>
  val email = Chat.fieldOf(r.body, "email")
  val code  = Chat.fieldOf(r.body, "code")
  if Login.confirm(email, code) then … else 401 "wrong or expired code"
```

`Chat.fieldOf` answers `""` for a missing field AND for a body that is
not JSON at all, so the two cannot be told apart. `/login` checks the
empty string and answers 400. `/login/confirm` does not: a malformed
request reaches `Login.confirm("", "")` and the caller is told
**401, wrong or expired code** — a diagnosis about their credentials
for what was a broken request. Both routes are covered only by
`portTest`, which is `Live` and out of the default gate.

### Interface

```scala
Router.json[B](Method.Post, route)(h: (A, B) => Response ! Async)(using Schema[B]): Router
```

The body is declared where the method and the handler are, not on the
`Route`: a `Route` describes a URL, and a URL has no body. The schema
decodes before the handler runs, so a handler never sees an undecoded
body; a body that does not decode is answered 400 with data —
`{"error": …}`, the same rule `Toolbox` states for a tool call, and for
the same reason.

The entry records the body's JSON Schema, so `describe` carries it —
what a renderer will need, and what stage 2 already gives tools.

### Behavior

- [x] a declared body arrives at the handler decoded
- [x] a body that is not JSON is answered 400, and never reaches the
      handler
- [x] a body missing a required field is answered 400
- [x] the answer is DATA naming what failed, not an exception
- [x] a route with no declared body is unchanged (`on`, `at`)
- [x] `/login` and `/login/confirm` are declared this way, and a
      malformed body is a 400 rather than a 401

## Stage 8 — DESCRIBE gets a consumer that can break

Stage 1 built the describing interpreter and only stage 4 has read it,
once, to name a probe path. Everything else asserted it against itself.
An interpreter nothing depends on is a claim, not a property.

**The consumer is the documentation, and the backlog entry's rule
picked it.** That entry — written in stage 3 — says do NOT start this
as "generate OpenAPI", name the consumer that reads the output first,
because a lane with no caller is the trap this arc has recorded three
times. OpenAPI has no reader in this repository. Module documentation
does, it lists endpoints in prose, and prose drifts: that is exactly
what stage 4 found by hand between `Ops` and `Site`.

`Router.markdown` renders the table from the entries that dispatch;
`docs/modules/okay-ops.md` commits the block between
`<!-- generated: ... -->` markers; `TestOpsSurface` asserts the
committed block is still what the live router renders. The repository
already keeps deployment renderings honest the same way.

**The drift test was verified by causing drift.** A passing drift test
proves nothing — it is equally green when the documentation is right
and when the test compares something with itself. `/readyz` was
changed to `/readyz-typo` in the committed block and the suite went
red, on the assertion and for the stated reason; the file was restored
in the same command. That is "no failing test, no fix" applied to a
test whose only claim IS its ability to catch.

The test does not depend on okay-deploy for `repoRoot`: three lines
walking up to `build.sbt` cost less than a monitoring module depending
on a deployment one, which is the same judgement stage 4 made about
okay-deploy not learning what a `Route` is.

### Behavior

- [x] `markdown` renders the verb, the path and whether a body is
      declared, from the entries that dispatch
- [x] the committed doc block equals what the router renders
- [x] a perturbed doc block fails the suite

## Decisions

- **2026-09-10 — nested pairs instead of a flat tuple: considered,
  rejected.** `Route[A] / Route[B] : Route[(A, B)]` makes composition
  trivial — `join` is `(a, b)`, `split` is `case (a, b)`, no typeclass
  and no match type at all. It was rejected because the flattening has
  to happen SOMEWHERE, and pairs move it from the combinator to every
  consumer. Concretely, `lit("users") / int / "posts" / str` would be
  `Route[((Unit, Int), String)]`, and (1) the extractor the type exists
  for becomes `case Get(userPost(((_, id), slug)))`, deepening with
  every segment; (2) `of[C]` becomes impossible, because
  `Mirror.ProductOf`'s `MirroredElemTypes` is FLAT, so a flattener
  would be needed anyway — an induction over a binary tree rather than
  over a list, which is strictly more cases; (3) literal segments stop
  being free (today `Split[EmptyTuple, B] = B` erases them; with pairs
  each contributes a `Unit` to be pruned, by — again — a typeclass).
  The typeclass's real job is to turn a left-associated SYNTAX
  (`a / b / c` groups as `((a/b)/c)`) into a right-nested TYPE, and
  that is work the pair encoding simply does not do.


- **2026-09-10** — the arc's criterion (an interpreter that
  DESCRIBES) is written before any candidate, because it is what
  disqualifies the pretty ones. It is the reason routes come first
  and a typed query DSL comes fourth.

## Results

### What stays open, and what each is waiting for — 2026-09-11 (optics-outside-remaining)

Five candidates are closed: routes, tools, the query string, DESCRIBE
with a consumer that can break, and conf. The four below were
re-checked for a consumer against the tree AS IT IS TODAY, which
includes `okay-leads` — a module that landed this morning and whose
whole purpose is "the fields a provider would pay for, none of the
data that would make passing them on illegal", the closest thing this
repository has ever had to a projection policy.

None of them opened. What follows is the measurement and the TRIGGER
that would open each, written down so the next agent neither
re-derives it nor, worse, builds one of these without a caller — the
trap this arc has now recorded four times.

**policy — no seat, and `okay-leads` is the sharpest test of that.**
A `Lead` never travels redacted. `Demand.Report` is aggregates —
counts, digests, pairs — and cannot carry a contact by construction
rather than by policy; `Demand.deliverable` is a row FILTER (has a
contact, is not finished), not a field projection. The other candidate
seat, `TestNoCredentialLogs`, is still a regex over committed source
with no value-level projection to audit, and the adapters it guards
have no log lines at all. **Trigger:** the first place that hands a
record to an outside eye with SOME fields removed AND has to answer
"which fields does this policy touch" with no document in hand. The
audit is the interpreter that earns the traversal; redaction alone is
a function.

**query — blocked behind policy, by the spec's own ranking.** okay-sql
is still strings plus `Schema` for rows. Unchanged, deliberately: a
typed query DSL is a swamp, and the spec asks for routes and policy to
measure the shape first. Routes did; policy has not.

**live — measured at the only live consumer in the tree, and it is
already minimal.** `ChatDemo` publishes a KIND
(`feed.publish("board")`) and the client re-fetches `/board.json`. A
lens-addressed push would replace a cheap re-fetch of a handful of
tasks with an optic that must be reifiable AND survive serialisation.
**Trigger:** a document large enough that re-fetching it on every
change is the measured cost — with the measurement, not the intuition.

**topology — the staticness condition is met by nothing here, and WHY
is the useful part.** `Stage[I, O, A] = A ! (Take % I + Writer % O)`
is a program, not a graph. It is a value before it runs, but
everything past the first effect lives inside a continuation, so it
cannot be walked, drawn, fused or shipped without running it. That is
not an oversight: a monadic pipeline's shape legitimately depends on
its values, and an `Arrow` is exactly the trade — a static shape, at
the price of needing `ArrowChoice` before a branch can exist at all.
okay-flink is one file of interop with no plan value of our own to
compare against. **Trigger:** a second consumer that needs the graph
BEFORE it runs — a renderer, a fuser, or a cluster shipper.

**tools-effectful — the premise re-verified rather than recalled.**
`Persist.append(partition, key, value, ack): Long` returns a value
directly; okay-persist is synchronous by design, so nothing in the
tree gives a tool a reason to suspend. All three callers —
`Mcp.Server`, `Handlers.tools`, `Stepper` — take `A => String`.
**Trigger:** the first tool that must do I/O its caller cannot do for
it.

The rule the arc has been enforcing on itself, one more time, because
it is the whole content of this section: **a declaration earns an
optic when it is handed to more than one interpreter and at least one
of them DESCRIBES rather than runs.** Every candidate that closed had
that consumer in the tree already. Every candidate still open is
waiting for one, and the waiting is the correct state.

### The conf candidate — CLOSED 2026-09-11 (optics-outside-conf)

The sixth candidate asked for "a setting is a lens that knows its
path: the error names `server.tls.port`". Half of it is refused by a
decision already in the tree and half of it was already built, which
is worth writing down rather than discovering twice.

**The path half is refused.** `Serve.Config` says *flat and scalar on
purpose*: an environment carries text, numbers, yes/no and a secret
REFERENCE, and the pairs a program actually wants — a certificate WITH
its key, an ACME account WITH its domains — are assembled where half
of one can be a named refusal instead of a silent fallback. There is
no nested config in this repository, so a path lens would be
machinery for a shape nothing has. `Conf.fromEnv` refuses a nested
field BY NAME, which is the same decision stated at run time.

**The description half was built and under-consumed.** `envName`
derives `OKAY_TLS_RELOAD` from `tlsReload`, one derivation for the
program that reads and the deployment that renders, and
`Serve.Config.names` publishes the list. `TestScriptConfig` held that
list against the DEPLOYMENT and against nothing a person opens — so a
setting could be declared, rendered into a unit file, read at boot and
still be undiscoverable. `OKAY_ACME_EAB` was exactly that: the
external account binding a commercial CA hands you out of band, in
specs/acme.md and in no guide. The red test named it; the fix is two
rows and a paragraph in `docs/okay-script-guide.md`.

**The law is one-directional, and the measurement is why.** The
obvious form — every `OKAY_*` the guide names is a setting — is false
twice over. `OKAY_CONF` names the config FILE, so it cannot be a field
of what that file parses into; `OKAY_STAGING` is okay-staging's codec
switch, which every program in the tree shares. A program's
environment is strictly larger than its config, and only the inclusion
that says "nothing declared is undiscoverable" holds.

So the candidate closes with one test and no new abstraction. That is
the arc's own criterion applied honestly: the declaration was already
going to three interpreters, and what it needed was not a fourth
algebra but a consumer that could break.

### Stage 2, finished — LANDED 2026-09-11 (optics-outside-tools-adopt)

Stage 2 gave `Toolbox` and converted `BoardTools`; the rest of the
tree kept the hand-written pair, and BACKLOG recorded the remainder as
test tables — "the cost of the drift is lower". That was wrong about
one of them. okay-demo's `RepoAgent` is an APPLICATION, and it held
two `ToolSpec` vals beside a `Map` keyed by the same two names;
`RepoMcp` handed the two structures to one `Server.Serving` as two
arguments, so nothing but care kept the name sets equal. Five test
tables copied the same shape, which matters more than a test usually
does: they are what a reader of okay-mcp copies.

**The conversion changed behaviour, and that is the finding.** The
hand-written decode answered `"bad args: $e"` — prose — while every
tool declared through `Toolbox` answers `{"error": "<tool>: <why>"}`.
The same agent therefore reported failure in two shapes, decided by
which module happened to declare the tool, and a model calling both
had to guess which one it was reading. The red test was written
against that (`TestRepoTools`, in the default gate; `TestRepoAgent` is
`Live`-tagged and indexes this whole repository, which is why the
tools' CONTRACT had no fast test at all).

Two smaller things. `Json.parse` is lossless on a non-JSON string, so
prose comes back as a `JStr` and the test's `errorOf` returns `None` —
the distinction under test needs no string matching. And the tutorial
in `docs/modules/okay-agent.md` still SHOWED the hand-written table;
documentation is the copy source of record, so a sweep that leaves it
is not finished.

### Stage 7 — LANDED 2026-09-11 (optics-outside-routes-body)

`Router.json[B]` declares the body; `/login` and `/login/confirm` are
declared with it, and a malformed body is a 400 rather than a 401.

**The lane found a defect of mine, four stages old.** `Router.routes`
was `Function.unlift(find)`, so `isDefinedAt` called the HANDLER to
discover whether the route matched. That was harmless while every
handler in the tree merely BUILT a program — `Acceptance`, `Ops` and
`Site` all do — and wrong the moment one did work outside it.
`Login.confirm` spends a one-time code, so an `isDefinedAt` followed by
an `apply` spent it twice and answered **401 to a correct code**.

`Entry` carries `matches` apart from `run` now; `isDefinedAt` asks only
the method and the path, and `applyOrElse` keeps `orElse` composition
from paying for a second search. Two tests pin the CAUSE rather than
the symptom — `isDefinedAt` leaves the handler's counter at zero, and a
route that did not match does not run — because the symptom test would
have fixed `/login` and left the next effectful handler open.

Three things about how it was found are worth keeping. It was invisible
for four stages because no handler had an effect outside its program,
so the defect had no witness. The property that made it visible is the
one that makes `Login.confirm` correct: spending the code ONCE. And it
surfaced in the Live suite, which the default gate does not run — the
second time this session that explicitly running the Live tests of
converted code paid for itself.

### Stage 6 — LANDED 2026-09-11 (optics-outside-route-labels)

`of[C]` is `inline`, reads `MirroredElemLabels` and refuses a route
whose parameter names are not the class's field names, in order —
path first, then query.

**Verified by removing the check.** With the `require` disabled, the
two new tests fail with "expected exception ... but body evaluated
successfully": a route declaring `(id, slug)` mapped into
`case class Wrong(a: Int, b: String)` without a murmur, which is
exactly the silence the stage was written to end. The types agree in
that example on purpose — otherwise the compiler would have refused it
and the test would have proved nothing about the names.

### Stage 4 — LANDED 2026-09-10 (optics-outside-ops-routes)

`Ops.healthz/readyz/stats/metrics` and `Site.Ops.healthz/stats/metrics`
are values; the routers are built from them; `ScriptDeploy` names both
probes from `Site.Ops`. Two tests hold the ends together — `paths` is
exactly what the router dispatches, and every probe path the manifest
names is one the Site serves — and neither could be stated before.

**A disagreement nobody had noticed.** `Ops` compared the WHOLE url
(`r.url == "/healthz"`) and `Site` compared only the path
(`pathOf(r.url)`), so `/healthz?probe=1` was served by okay-script and
missed by okay-ops. Two modules with the same-looking check behaved
differently. Both behave like `Site` now, and a test says so.

**A flake that had to be understood before it could be dismissed.**
The lane's first run of `okayOpsJVM/test` went red on
`TestSignals`, a suite this lane does not touch. Structural argument —
it never mentions `Ops` — was not accepted as sufficient, because "my
diff could not have done that" is exactly the reasoning that has
failed here before. A control run on UNMODIFIED master failed it twice
in three tries at load ~20, the same rate as the worktree, which
settled the attribution by measurement.

The mechanism turned out to be a defect worth fixing rather than
tagging: the test waited with
`while hold == null && spins < 10_000_000 do spins += 1` and then
asserted unconditionally. That loop exits on EITHER condition, so an
exhausted spin budget was indistinguishable from a successful wait —
under load the spawned thread never got a core and the assertion spoke
about a request that had not started. The product's ordering was
correct throughout. A `CountDownLatch` made the timeout a timeout and
gave the cross-thread `var` the happens-before edge it never had: five
clean runs after, against two failures in three before.

### Stage 3 — LANDED 2026-09-10 (optics-outside-routes-query)

`Query` in `okay-http/src/main/scala/okay/http/Route.scala`, 13 new
tests in `TestRoute` (32 in the suite, 44 in the module), green on JVM
and JS. `docs/modules/okay-http.md` has the user-facing half.

**The rename that came with it.** `Route.Concat` is `Route.Split`. The
operator read the old name and took it for `scala.Tuple.Concat`, which
is a fair reading — and the typeclass's whole addition over the
standard library IS `split`, since `join` could have been `++`. The
doc comment also states the real reason not to build on the match
type, which is not "the compiler will not prove associativity" (a
symptom) but that the match type does not NORMALISE: with
`Out = Tuple.Concat[A, B]` the accumulated type becomes a stuck
`Tuple.Concat[A1, A2]` with no head to peel, and the NEXT `/` cannot
resolve at all.

**A bug the query exposed, in a rule attached to the wrong layer.**
`Param.string` refused the empty string, so an empty PATH segment
could not build the same url from different parameters. That is a rule
about the POSITION, not about the type — `?tag=` is a perfectly good
empty value, and `Query.all[String]` has to round-trip one. The
refusal moved into the path capture. It surfaced the moment a SECOND
consumer of the same `Param` existed, which is the general shape worth
remembering: a constraint on one use site, written into a shared type,
is invisible until the second use site arrives.

**No `Router` change was needed**, which is the small confirmation
that stage 1's seam was cut in the right place: `unapply` takes the
whole url, so a route that reads a query dispatches through the
`PartialFunction` unchanged, with its required parameters part of the
match.

### Stage 2 — LANDED 2026-09-10 (optics-outside-tools)

`okay-agent/src/main/scala/okay/agent/Toolbox.scala`, 9 tests in
`TestToolbox`, plus two coupling tests in okay-demo's `TestBoard`.
`docs/modules/okay-agent.md` has the user-facing half. Both converted
callers' suites pass unchanged: okay-agent 136, okay-demo 56.

**The name was taken, and the collision improved the API.**
`okay.agent.Tool` already exists — it is the EFFECT signature
(`enum Tool[+A] derives Effect`), the thing `Agent.call` performs. So
the element could not be `Tool`, and rather than inventing a synonym
the builder moved onto the box: `Toolbox.empty.on[A](name, desc)(run)`.
That reads exactly like stage 1's `Router.empty.on(...)` — one
vocabulary across the arc — and it makes the element type
MONOMORPHIC: `A` is consumed by `on`, where the `Schema[A]` is still
in scope, so a box of tools with different argument types is an
ordinary `Vector[Entry]` needing neither an existential wrapper nor a
cast. Stage 1's `Router.Entry` had already found the same road.

**A phantom requirement, caught before it was built.** The first cut
of the conversion wanted `BoardTools.specs` to stay a board-free
`val`, which meant either a fake `Board` that exists only to be
ignored, or generalising `Toolbox[-R]` so handlers are
`R => A => String`. Both were written down before the call sites were
read; BOTH callers already had a board in scope. `specs(board)` it is,
and the generalisation stays unbuilt until something actually needs to
render a declaration away from its environment.

**What the conversion changed in the prompt — the finding.** The
hand-written schemas never declared `required`, so no model was ever
told that `board_add` needs both `text` and `owner`. The derived
declaration says so, and `TestBoard` now pins it. `board_assign`'s
`id` went from `"number"` to `"integer"`, which is what a `Long` is.
A schema change is a model-facing change
(`jsonschema-render-is-prompt-text`), and this one makes the
declaration more accurate rather than less.

**`raw` earns its place and only half the win.** `StateMcp`'s three
tools take a JSON Merge Patch — arbitrary by definition, with
`additionalProperties: true`. There is no case class, so there is no
derivation; what the toolbox still gives is the PAIRING, the name
written once instead of twice. Saying which half applies is the point
of the constructor existing.

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

`Split` had to be a typeclass. `Tuple.Concat` composes forwards and
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
