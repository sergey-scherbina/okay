# Declaring an API

A path, a query, a body, a tool — each written once, and then asked
several different questions. This is the user-facing half of
`specs/optics-outside.md`; the spec keeps the decisions and the
refuted alternatives, this keeps what you write.

## The idea, and the test that makes it worth doing

A plain function `S => A` already covers "read this out". A
declaration earns something more than a function exactly when it has
to be handed to **more than one interpreter, and at least one of them
DESCRIBES it instead of running it**. You cannot ask `f: S => A` what
it looks at; you can ask a declaration.

So every type below answers at least three questions:

| | a route | a tool |
|---|---|---|
| MATCH / DECODE | `unapply` — does this url fit, and with what parameters | `Schema[A]` decodes the call's arguments |
| BUILD | `url(a)` — the path for these parameters | — |
| DESCRIBE | `describe`, `params`, `queries` — with no request in hand | `spec` — the JSON Schema the model is told |

The third row is the reason the first two are worth reifying. A
`case r if r.url == "/healthz"` can serve a request and can tell you
nothing else; a value can be printed into an OpenAPI operation, read
by a deployment naming a probe, or compared against the handler that
answers it.

## A path

```scala
import okay.http.*

val healthz  = Route / "healthz"
val userPost = Route / "users" / Route[Int]("id") / "posts" / Route[String]("slug")
```

`Route` is the companion, so a declaration never opens with a
constructor. A bare string is a literal segment; `Route[T](name)` is a
captured one, and `T` needs a `Route.Param[T]` — `String`, `Int`,
`Long` and `Boolean` come with the module.

```scala
userPost.unapply("/users/7/posts/hello%20world")  // Some((7, "hello world"))
userPost.url((7, "hello world"))                  // "/users/7/posts/hello%20world"
userPost.describe                                 // "/users/{id}/posts/{slug}"
```

`unapply` makes it an extractor, so a server reads its parameters in
the pattern:

```scala
def routes: PartialFunction[Request, Response ! Async] =
  case Get(userPost(id, slug)) => ...        // id: Int, slug: String
```

**The law is the feature.** `unapply(url(a)) == Some(a)` says the path
a client builds is the path the server matches, from one declaration.
A hand-written `r.url == "/users/" + id` cannot state that property,
let alone check it. `TestRoute` checks it over integers, longs,
booleans and strings that need escaping — `"a/b"`, `"100%"`, Cyrillic,
an emoji.

Two details follow from it. Percent-decoding happens **per segment,
after the split**, so a parameter holding `"a/b"` builds
`/users/7/posts/a%2Fb` and reads back as `"a/b"` — while
`/users/7/posts/a/b` is a different path with one segment too many.
And an **empty** segment is refused, because `"/x//y"` and `"/x/y"`
would otherwise build the same url from different parameters.

## A query

```scala
val search = Route / "search" :? Query[String]("q") :? Query.opt[Int]("page")
val tagged = Route / "posts" / Route[Int]("id") :? Query.all[String]("tag")

search.url(("cats", Some(2)))            // "/search?q=cats&page=2"
search.url(("cats", None))               // "/search?q=cats"
search.unapply("/search?page=2&q=cats")  // Some(("cats", Some(2)))
search.describeFull                      // "/search?q={q}&page={page}"
```

`Query[T](name)` is required, `Query.opt[T]` optional (absent is
`None`, and `None` writes nothing), `Query.all[T]` repeated (every
occurrence in wire order, empty writes nothing). `:?` chains; `+&`
composes a query worth sharing:

```scala
val paging = Query.opt[Int]("page") +& Query.opt[Int]("size")
val posts  = Route / "posts" :? paging
val users  = Route / "users" :? paging
```

**This is where a route stops resembling an iso and is plainly a
prism.** `url` writes the query in declaration order, so it picks the
canonical url among the many the route accepts:
`unapply(url(a)) == Some(a)` holds, and `url(unapply(u)) == u` does
not, because `url` normalises. `route.prism` hands the whole thing to
the core optics as `Prism[String, String, A, A]`.

Three rules worth knowing, each chosen rather than inherited:

- **present and unparseable is a MISS, not `None`.** `?page=abc` on an
  `Int` is a request that meant something and got it wrong; answering
  it as though the parameter had been omitted hides the caller's
  mistake behind a page of results.
- **unknown parameters are ignored**, or every `utm_source` would
  break the route.
- **a raw `+` in a value is a space** — the form-encoding convention
  every browser writes. `url` never emits one, so the round trip is
  unaffected either way.

**A path may not follow a query**, and that is structural: `:?`
answers a `Queried[A]`, which has no `/` at all. `Routed[A]` is what
both stages share — `unapply`, `url`, `describe`, `params`, `queries`,
`prism`, `of` — and what `Router` takes.

## A table

```scala
val router = Router
  .on(Method.Get, healthz)(_ => text(200, "live"))
  .at(Method.Post, userPost)((p, r) => usePayload(p, r))

router.routes    // PartialFunction[Request, Response ! Async]
router.describe  // Vector((Get, "/healthz"), (Post, "/users/{id}/posts/{slug}"))
```

`on` hands the handler the path's parameters; `at` hands it the
request as well — its body, its headers, its peer. `at` is the
primitive and `on` is written in terms of it, because most handlers
need the request.

`routes` is deliberately **not** a new protocol: it is the same
`PartialFunction` every server in this stack already takes, so
adopting `Router` changes nothing downstream. `Router.empty` is still
there and still means something — the zero of a fold over several
tables, and what a module answers when it contributes no routes.

**`describe` is derived from the same vector that dispatches**, so a
route cannot be documented and unrouted, or routed and undocumented.
`router.markdown` renders that as a table, and a module's docs commit
the block between `<!-- generated: ... -->` markers with a test
asserting it still matches what the live router renders —
`docs/modules/okay-ops.md` is the worked example. That is the property
made load-bearing: prose about endpoints drifts, and this is the drift
stage 4 found by hand between `Ops` and `Site`, now caught by a test.
That property is the whole reason to reify a path instead of writing a
`case`, and it is what lets a deployment name a probe path it did not
invent:

```scala
Ops.paths                  // Set("/healthz", "/readyz", "/stats", "/metrics")
Health(livenessPath = Site.Ops.healthz.describe, ...)
```

**`isDefinedAt` does not run the handler.** It asks the method and the
path and nothing else. This matters for any handler that does work
outside the program it returns — a counter, a log line, a one-time
code — and it was not true of the first cut; see "What it found".

## A body

```scala
final case class LoginConfirm(email: String, code: String)
given Schema[LoginConfirm] = Schema.derived

Router.json[EmptyTuple, LoginConfirm](Method.Post, Route / "login" / "confirm") { (_, in) =>
  if Login.confirm(in.email, in.code) then ok(...) else refused(...)
}
```

The body is declared where the method and the handler are, because a
route describes a url and a url has no body. The schema decodes
**before** the handler runs, so a handler never sees an undecoded
body, and a body that does not decode is answered `400` with data:
`{"error": "..."}` naming what failed. The entry records the body's
JSON Schema, so `describe` carries it.

## A case class instead of a tuple

```scala
final case class UserPost(id: Int, slug: String)
val r = userPost.of[UserPost]

r.unapply("/users/7/posts/hello")   // Some(UserPost(7, "hello"))
r.url(UserPost(7, "hello world"))   // "/users/7/posts/hello%20world"
```

Worth preferring once a route has parameters: `case Get(r(p)) => p.id`
beats positional tuples at every arity. The mapping is positional, so
the compiler checks the types — and `of[C]` checks the **names**,
which position alone would never notice:

```
route parameters (id, slug) do not match the field names (a, b):
the mapping is positional, so the types already agree — rename one
side so the declaration says what it means
```

The check runs at construction rather than at compile time (the labels
are type-level, a route's names are values). Routes are `val`s, so it
fires at class initialisation: start-up, before the first request, or
never. One cost, in the open: a field deliberately named differently
from its url parameter is refused, and must be renamed on one side.

## A tool is the same shape

okay-agent's `Toolbox` is this idea for an agent's tools, where the
three interpretations of one `Schema[A]` are DECLARE (the JSON Schema
the model is told), DECODE (the arguments of a real call) and DISPATCH
(your handler, over the decoded value):

```scala
final case class Add(text: String, owner: String)
given Schema[Add] = Schema.derived

val tools = Toolbox
  .on[Add]("board_add", "Add a task. The owner is whoever asked for it.")(a => ...)
  .raw("update_state", "Merge a JSON Merge Patch...", objectSchema())(patch => ...)

tools.specs   // Seq[ToolSpec]                    -- what the model is told
tools.table   // Map[String, ToolCall => String]  -- the seam Mcp.Server takes
```

`specs` and `table` are drawn from one vector, so their name sets are
equal by construction. `raw` is for a tool whose argument is arbitrary
JSON — a merge patch has no case class to derive from, and inventing
one would be a lie about the protocol; it buys the pairing and not the
derivation, and saying which half applies is the point of having it.
A call that does not decode answers `{"error": "<tool>: <why>"}`,
because a model handed an exception learns nothing.

One inconsistency survives and is reported rather than hidden: a `Map`
keeps the last of a duplicate name and a `Seq` keeps both, so
`duplicates` names them.

**The sweep is finished, and it found a second thing.** No module
declares a tool by hand any more: `RepoAgent` was the last production
pair — two `ToolSpec` vals beside a `Map` keyed by the same two names,
handed to one MCP server as two arguments — and the five test tables
that copied it followed. What the drift argument had not predicted is
that the two ways of declaring a tool also disagreed about FAILURE:
the hand-written decode answered `bad args: ...` as prose, `Toolbox`
answers `{"error": ...}`, and a model calling one of each had to guess
which shape it was reading. Converting the declaration fixed the
answer as a side effect, which is the usual sign that the two things
were one thing.

## The terse syntax, and why it is spelled that way

```scala
import okay.http.syntax.*

val userPost = Route / "users" / "id".as[Int] / "posts" / "slug".as[String]
val search   = Route / "search" :? "q".as[String] :? "page".opt[Int]
val tagged   = Route / "posts" / "id".as[Int] :? "tag".all[String]
```

One rule: **a bare string is a literal segment, `"name".as[T]` is a
hole**, and the operator says where the parameter goes — `/` into the
path, `:?` into the query — because a named typed parameter is the
same thing in both places. Behind an import, because an extension on
`String` reaches every string in scope.

Three spellings were tried first and are impossible or worse; they are
written down so nobody spends an evening rediscovering them.

- **`:?[String]("q")` does not parse.** Scala takes an infix
  operator's precedence from its FIRST character and will not accept a
  type argument after one. That is also why the operators are `:?` and
  `+&` rather than `?` and `&`: `?` is in the "all other special
  characters" group, which binds TIGHTER than `/`, so
  `Route / "search" ? q` would group as `Route / ("search" ? q)`.
  `:` is below `/` and `+` sits between them, so the chain needs no
  parentheses. (Associativity comes from the LAST character, so `:?`
  stays left-associative.)
- **`"id"[Int]` parses but never reaches you.** `String` already has
  an `apply` through `StringOps`, and an extension method is only
  consulted when the member does not exist. It compiled in five
  isolated probes and failed on contact with the real imports — a
  probe that isolates a feature also isolates away its competitors.
- **Scala 3's generalized method syntax does not rescue the infix
  form.** `def cap(name: String)[T](using Param[T])` is legal and
  `x.cap("id")[Int]` works, but `a cap "id" [Int]` still attaches the
  type argument to the operand.

## What it found

The declarations are the smaller half of what this arc produced. Each
conversion asked a question of code that had never been asked, and
five of the seven stages turned up something already broken:

- `startsWith("/person")` also answered `/personal`, and every route
  answered every verb, so a POST to `/person` was served the JSON body
  (okay-http's acceptance fixture).
- the hand-written tool schemas never declared `required`, so no model
  was ever told that `board_add` needs both its fields.
- `Param.string` refused the empty string for the PATH's sake and
  broke `?tag=` the day the query arrived — a rule attached to the
  wrong layer, invisible until a second consumer of the same type
  appeared.
- `Ops` compared the whole url and `Site` compared only the path, so
  `/healthz?probe=1` was served by one module and missed by the other.
- `Chat.fieldOf` answered `""` both for a missing field and for a body
  that was not JSON, so a malformed login reached
  `Login.confirm("", "")` and its caller was told **401, wrong or
  expired code**.

And one was mine, four stages old: `Router.routes` was
`Function.unlift(find)`, so `isDefinedAt` called the HANDLER to
discover whether the route matched. Harmless while every handler in
the tree merely built a program — and wrong the moment one did work
outside it, which is how a one-time code came to be spent twice and a
correct code answered with a 401. It had no witness for four stages
because no handler had an effect; the property that finally made it
visible is the one that makes `Login.confirm` correct.

## Where it is used

Every route-serving module in this repository declares its routes now —
okay-ops, okay-script, okay-http's acceptance fixture, okay-demo,
okay-chat, okay-admin — which is also how most of the defects above
were found. Two places deliberately do not, and the reasons are worth
knowing before you convert something that looks similar: **okay-acme**
was already right, cutting the query with its own `path(url)` before
comparing; and **okay-security's `McpAuth`** matches a prefix because
RFC 9728 allows the resource's path as a suffix of the well-known URI,
so the prefix is the specification rather than an oversight.

## Not done yet

An OpenAPI document is still not generated, deliberately: the backlog
entry says to name the consumer that reads the output first, and
OpenAPI has none in this repository. `describe`, `params`, `queries`
and the entry's body schema are the input when one appears. What DOES
have a consumer — the module documentation — is rendered and held by a
drift test, which is what stage 8 built. Headers are not declared.
`Toolbox` handlers are pure `A => String`, because that is the seam
`Mcp.Server`, `Handlers.tools` and `Stepper` already take; widening it
is a separate decision with those three callers to carry.
