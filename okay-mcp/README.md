# okay-mcp — the Model Context Protocol, both ends

MCP is JSON-RPC 2.0 over a byte stream: a client asks a server what
tools it has and calls them. This module is both ends, and it is small
because neither end needed a new vocabulary:

- **a tool call is an EFFECT**, so where it runs is a handler's
  business — an agent program does not change by one character when
  its tools come from a server instead of a local table;
- **our tools already ARE a server** — a `ToolSpec` plus a
  `Map[String, ToolCall => String]` is exactly what MCP serves, and
  the schema on the wire is the DERIVED one, so a published tool
  cannot drift from its parser.

The protocol itself is a pure `Stage[Rpc, Rpc, Unit]`: the whole of it
is exercised in a test with no process, no socket, no clock and no
thread. Only `over` touches a wire.

## Serving tools

```scala
import okay.*, okay.given
import okay.agent.{ToolCall, ToolSpec}
import okay.codec.Schema
import okay.mcp.{Mcp, Server, Stdio}

final case class Add(a: Int, b: Int)
given Schema[Add] = Schema.derived

val specs = Seq(ToolSpec[Add]("add", "add two numbers"))
val table = Map("add" -> { (c: ToolCall) =>
  ToolSpec.args[Add](c).fold(e => s"bad args: $e", x => (x.a + x.b).toString) })

// this process's own stdin/stdout — how an MCP client spawns a server
Server.run(Stdio.std, Mcp.Info("adder", "1.0"), specs, table).runWith
```

`Server.Serving` carries the rest when a server has more than tools —
resources, prompts, completions, resource templates. Capabilities are
computed from what is actually there, so a tools-only server does not
advertise resources, and a client reads the handshake rather than
guessing.

## Consuming a server

```scala
import okay.mcp.{Client, Mcp, Stdio}

val process = Stdio.spawn(Seq("npx", "-y", "@modelcontextprotocol/server-everything"))
val session = Client.connect(Stdio.of(process), Mcp.Info("okay-mcp", "0.1")).runWith

session.tools.runWith.map(_.name)                       // tools/list
session.call(ToolCall("c1", "echo", Rpc.obj(
  "message" -> Json.JStr("okay")))).runWith             // tools/call
session.has("prompts")                                  // what it declared
```

And the point of it — the same agent program, both ways:

```scala
import okay.agent.Handlers

val local: Handler[Tool] = Handlers.tools(table)
val remote: Handler[Tool] = session.handler     // needs a CanBlock

// `program` runs under either, and mentions MCP nowhere
```

## Over HTTP

```scala
import okay.http.McpHttp

val serving = Server.Serving(Mcp.Info("adder", "1.0"), tools = specs, call = table)
val route = McpHttp.route(serving)      // POST /mcp, plus the SSE GET for pushes
```

## Access control, at the granularity of a tool

A valid token should not mean the whole server. Here it does not, and
the rule is that **a tool a caller may not use is ABSENT rather than
refused** — missing from its `tools/list`, and answering "no such
tool" if named, exactly as a misspelling does. A check written inside
each handler is the other design, and the handler nobody wrote is the
hole.

**Narrow the server** when you build one per connection (stdio, or one
connection one server). The advertised list and the executable table
narrow together, so they cannot disagree:

```scala
val forThisCaller = serving.only(Set("search", "read"))
```

**Narrow the conversation** on a shared HTTP door. The policy is asked
per TOOL and per REQUEST — a permission withdrawn between two calls
stops the next call rather than the next reconnect:

```scala
import okay.security.{Decision, McpAuth, Policy}

// asked as policy(principal, Mcp.ToolsCall, toolName)
val policy: Policy = (who, _, tool) =>
  if who.claims.scopes.contains("tool:" + tool) then Decision.Permit
  else Decision.Deny("not for this caller")

val door = McpAuth.tools(verify, metadataUrl, policy)(McpHttp.route(serving))
```

**Let the holder narrow it.** Capabilities here are macaroon-shaped:
each caveat is signed with the previous signature as the key, so
adding one needs only the token itself while removing one would need a
value that was consumed and never travels. Anyone can narrow, nobody
can widen, with no issuer, no registry and no round trip:

```scala
import okay.security.{Capability, Caveat}

val grant  = Capability.issue(rootKey, "alice")            // the user's own
val agent  = grant.attenuate(Caveat.Scope("tool:search"))  // one tool
                  .attenuate(Caveat.Agent("crawler-7"))    // and whose branch it is

val door = McpAuth.capabilities(rootKey, metadataUrl)(McpHttp.route(serving))
```

Which is prevention: it happens when authority is handed over, and it
does not reach an agent misbehaving right now — the holder who would
narrow it is the one misbehaving.

**So plug in a policy from outside.** Stopping a caller already in
flight needs a list of what is switched off, and keeping such a list
well — an audit trail, an operator who can act in seconds, reach
across services — is an operational product rather than a library
feature. This module does not keep one. It takes one:

```scala
import okay.security.Revocations

val now = () => System.currentTimeMillis()
val list = Revocations(freshFor = 60_000, whileStale = Revocations.Stale.Allow)

list.refresh(source)(now).runWith        // one pass, on whatever schedule you have

val door = McpAuth.capabilities(rootKey, metadataUrl,
  revoked = list.revoked(now))(McpHttp.route(serving))
```

`revoked` is a predicate, so the list can be a constant, a column, or
somebody else's service; `Revocations` is the last case — a local
snapshot of a remote list, refreshed by a program you run and read
synchronously by the door, because a capability is checked per tool
and `tools/list` checks every tool.

Two things the seam makes you decide, because they are where an
external list actually bites:

- **the dangerous failure is not "the registry is down" but "the
  registry answered empty"** — a client that turns an error into an
  empty set un-revokes everyone at the worst moment. So a source
  answers either a list or a NAMED failure, and a failed refresh keeps
  the list it had;
- **staleness has no default** — past the freshness window the
  snapshot stops being evidence, and `Stale.Allow` keeps serving the
  last answer (a kill switch that lags beats a door that jams) while
  `Stale.Deny` refuses instead of guessing. The same rule before the
  first successful fetch.

## Further

| | |
|---|---|
| `../docs/modules/okay-mcp.md` | how the module is built: the pieces, and where each MCP capability lands in this library's vocabulary |
| `../specs/mcp.md` | the design and its decisions |
| `../specs/security.md` | stage 1 (MCP as an OAuth resource server) and stage 7 (the tool gate, capabilities, the external list) |
