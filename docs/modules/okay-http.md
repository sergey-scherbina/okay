# okay-http

REST and WebSocket, as programs (specs/http.md).

Three specs already deferred to a "transport module" that had never
been written — `cross-platform-async.md`, `codecs.md` and `cluster.md`
all name it as an ingredient, and no ROADMAP phase owned it. This is
that module, and it is small because the vocabulary decided most of it:

- **a response body is a stream** — `Source[Chunk[Byte]]`, which is
  `Unit ! (Writer % Chunk[Byte] + Async)`. Nothing is read when the
  head arrives; the body goes `through` a decoding `Stage` exactly as
  SSE lines already did. `Pipe.scala` names that case in its own doc
  comment as "the generalization the LLM client walks by hand" — this
  is the caller that stopped walking it.
- **a WebSocket session is a `Stage[Frame, Frame, A]`** — it awaits
  incoming frames and tells outgoing ones. Not an analogy: okay-mcp is
  already `Stage[Rpc, Rpc, Unit]` for the same reason.
- **the transport is not an effect** — a trait speaking `Async`, like
  `llm.Transport`, `mcp.Link` and `cluster.Remote`. The house rule
  mints an effect signature for domain logic above the wire, never for
  the wire.

## The pieces

| | |
|---|---|
| `Request` / `Response` / `Method` / `Body` | the wire, as data — a 4xx is a `Response`, and no `Throws` appears anywhere |
| `trait Http { def send(r: Request): Response ! Async }` | the seam. One method, like `llm.Transport`, but carrying the verb, the status and the headers back |
| `Http.bytes / text / lines / json / sse` | reading a body — `lines` streams, `json` is total, `sse` IS `llm.Sse.events` |
| `Frame` / `trait Socket` / `trait Sockets` | the WebSocket side |
| `Ws.over(socket)(session)` | run a `Stage[Frame, Frame, A]` over a socket |
| `Ws.link(socket)` | a socket AS an `mcp.Link` |
| `Transports.http` / `.sockets` (JVM), `.fetch` / `.sockets` (JS) | the two platform seams |
| `Route` / `Query` / `Router` | a typed path and query: match, build and describe from one declaration |
| `Server.serve(port)(route)` | a REST server, JVM only |

## Routes: one declaration, three interpreters

`Route` (specs/optics-outside.md, stage 1) is the module's typed path.
It exists because of a test the optics arc set for public APIs: a
declaration earns an optic only when it must be handed to more than
one interpreter, and at least one of them DESCRIBES it instead of
running it. A path passes that three times.

```scala
val userPost = Route / "users" / Route[Int]("id") / "posts" / Route[String]("slug")

userPost.unapply("/users/7/posts/hello%20world")  // Some((7, "hello world"))  -- MATCH
userPost.url((7, "hello world"))                  // "/users/7/posts/hello%20world"  -- BUILD
userPost.describe                                 // "/users/{id}/posts/{slug}"  -- DESCRIBE
```

A server keeps the convention it already had — the routes are still a
`PartialFunction[Request, Response ! Async]` — and gets typed
parameters out of the same value:

```scala
def routes: PartialFunction[Request, Response ! Async] =
  case Get(userPost(id, slug)) => ...   // id: Int, slug: String
```

or assembles a table that is also its own documentation:

```scala
val router = Router.empty
  .on(Method.Get, healthz)(_ => ok("live"))
  .on(Method.Get, userPost)((id, slug) => post(id, slug))

router.routes     // the PartialFunction, unchanged convention
router.describe   // Vector((Get, "/healthz"), (Get, "/users/{id}/posts/{slug}"))
```

`describe` is derived from the same values that dispatch, so a route
cannot be documented and unrouted, or routed and undocumented — which
is the whole reason to reify a path instead of writing a `case`.

**The law is the feature.** `unapply(url(a)) == Some(a)` says the path
a client builds is the path the server matches, from ONE declaration.
A hand-written `r.url == "/users/" + id` cannot state that property,
let alone check it; `TestRoute` checks it over integers, longs,
booleans and strings that need escaping, including `"a/b"`, `"100%"`,
Cyrillic and an emoji. `route.prism` hands the same route to the core
optics as a `Prism[String, String, A, A]`, where the law is the prism
law the repository already tests.

**Three details worth knowing.**

- *Percent-encoding is per segment, after the split.* A parameter
  holding `"a/b"` builds `/users/7/posts/a%2Fb` and reads back as
  `"a/b"`; `/users/7/posts/a/b` is a different path with one segment
  too many, and does not match. Every hand-written router in this
  repository splits before it decodes and is open to that confusion —
  okay-script's `Site.resolve` still is. Malformed escaping is a MISS,
  not a silent literal `%`.
- *An empty capture is refused.* `"/x//y"` and `"/x/y"` would
  otherwise build the same URL from different parameters. That is a
  property of URLs, and refusing it is what keeps the law total.
- *A route can read a case class.* `userPost.of[UserPost]` gives the
  same three interpreters over `UserPost(id, slug)`, which is the form
  to prefer once a path has parameters: `case Get(userPost(p)) => p.id`
  reads better than a positional tuple at every arity.

**The query string.** A route reads it as a `Query`, composed with
`&` and handed over with `?`:

```scala
val search = Route / "search" :? Query[String]("q") +& Query.opt[Int]("page")
// Route[(String, Option[Int])]

search.url(("cats", Some(2)))            // "/search?q=cats&page=2"
search.url(("cats", None))               // "/search?q=cats"
search.unapply("/search?page=2&q=cats")  // Some(("cats", Some(2)))  -- order is irrelevant
search.describeFull                      // "/search?q={q}&page={page}"
```

`Query[T](name)` is required, `Query.opt[T]` optional (absent is
`None`, and `None` writes nothing), `Query.all[T]` repeated (every
occurrence in wire order, and an empty vector writes nothing).

The operators are `:?` and `+&` rather than `?` and `&` because of
Scala's precedence table, not taste: precedence comes from an
operator's FIRST character, and `?` is in the "all other special
characters" group, which binds tighter than `/` — so
`Route / "search" ? q` would parse as `Route / ("search" ? q)`. `:` is
below `/` and `+` sits between them, so `r / "s" :? a +& b` groups the
way it reads, with no parentheses anywhere. (Associativity comes from
the LAST character, so `:?` stays left-associative.)

Its own type rather than more `Route` combinators, because the path is
ORDERED and the query is not: a path parameter is found by position, a
query parameter by name, and a url that writes them in a different
order is the same request. Four consequences worth knowing:

- **`url` writes the query in declaration order**, so it is the
  canonical url among the many the route accepts. This is where a
  route stops resembling an iso and is plainly a prism:
  `unapply(url(a)) == Some(a)` holds and `url(unapply(u)) == u` does
  not, because `url` normalises.
- **Present and unparseable is a MISS, not `None`.** `?page=abc` on an
  `Int` is a request that meant something and got it wrong; answering
  it as though the parameter had been omitted hides the caller's
  mistake behind a page of results.
- **Unknown parameters are ignored**, or every `utm_source` would
  break the route.
- **A raw `+` in a value is a space** — the form-encoding convention
  every browser writes. `url` never emits one (`+` is not unreserved,
  so it leaves as `%2B`), so the round trip is unaffected.

`describe` stays the PATH (`/users/{id}`), which is the shape OpenAPI
wants, with the query as a separate list: `queries` carries `name`,
`kind`, `required` and `repeated` for each.

A handler that needs the request itself — its body, its headers, its
peer — takes `at` instead of `on`: `at(Method.Post, echo)((_, r) =>
Http.text(...))`. `at` is the primitive and `on` is written in terms
of it, because most handlers need the request.

**It has a caller in this module.** `Acceptance.routes` — the fixture
all four backends (the JDK's server, Jetty, Netty, NIO) are held to —
was three `case r if r.url.startsWith("/person")` guards and is now a
`Router`. Converting it found two defects that had been there the
whole time: `startsWith("/person")` also answered `/personal`, and
every route answered every verb, so a POST to `/person` was served the
JSON body. Both refusals are asserted now.

A new parameter type is a `Route.Param[T]` — `kind`, `parse`, `print`,
three lines. Query parameters, headers and bodies are stage 2 of the
spec (stage 3 landed the query half); `describe` and `queries` are
what an OpenAPI operation or an MCP tool declaration will be generated
from in the stage after.

## What it buys okay-mcp

MCP has two standard transports: stdio, which okay-mcp had, and
HTTP+SSE, which it did not. A `Link` is `send(line)` plus
`lines: Source[String]`, and a WebSocket is exactly that with frames
around it — so `Mcp.run(Ws.link(socket), serving)` is the same server
over a different wire, with no protocol code changed. `TestWs` carries
an `Rpc` over frames and decodes it back to the identical message.

## Two honesty constraints, in the interface rather than papered over

**Backpressure is asymmetric.** The JDK's `WebSocket.Listener` is
genuinely pull-based — demand starts at zero, `request(n)` raises it,
each call lowers it, and at zero the socket stops calling, which is
flow control down to TCP. Browser and Node `WebSocket` have no
receive-side lever at all. So `request(n)` appears **nowhere** in
`Socket`: the JVM transport spends its own demand, one `request(1)` per
frame handed on, and the JS transport buffers into a bounded `Channel`
with the bound stated. A shared method one platform silently fakes
would be worse than the asymmetry.

**Serving WebSocket is out of scope.** The JDK has no server-side
WebSocket API, and `HttpServer` does not surrender its socket, so it
would mean hand-rolling RFC 6455. The tests do exactly that, in test
scope, to exercise the client against a real socket — `WsEcho` is 120
lines and is not a library feature.

## Platforms

`crossProject(JVM, JS)`, not Native — the same call okay-llm and
okay-cluster made. Scala Native has `java.net.Socket` but no
`HttpClient`, no `HttpServer`, no WebSocket and no complete
`javax.net.ssl`, so an implementation there means hand-rolling HTTP/1.1
in plaintext or binding libcurl.

"JS" here means Node, as everywhere in this repository — `fetch` and
the global `WebSocket`, over raw `js.Dynamic` rather than
scala-js-dom, matching `llm.TransportJs` and the dependency rule. The
JS body reader is incremental (`ReadableStream.getReader`), which is
the step `TransportJs`'s own comment had left as "the stated next".

## Not done

A JVM server driven by a JS client in one shared-source program — the
acceptance shape okay-cluster established with a linked Scala.js
subprocess. It is a build fixture rather than a module concern, and it
is the honest next step here.
