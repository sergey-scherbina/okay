## mcp-tool-authorization — a valid bearer stops meaning every tool

Landed as faa4b97c (the spec, before the code), f9289e74 (the three
pieces and the two defects), 140c2539 (the boards) and 3dad0dc3 (a
failure the gate found on master, recorded rather than diagnosed).

Opened by a stranger's question about exactly this boundary: once a
caller is authenticated, can it use every tool, or can one misbehaving
caller be restricted without revoking the user's authorization? The
honest answer was that we had the hole we would have pointed at.
`McpAuth.protect` asks the policy `(principal, method, url)`, so the
finest thing it could say was "this caller may POST /mcp"; past that
door every tool was callable and `tools/list` advertised all of them.

The rule the fix keeps: **a tool a caller may not use is ABSENT, not
refused.** A check inside a handler has to be written once per tool and
remembered forever, and the one nobody wrote is the hole. So the tool
set is a value, narrowed before the server sees it.

- `Serving.only(allowed)` (okay-mcp) — pure, no transport, no security
  dependency; `tools` and `call` narrow TOGETHER, so the list and the
  table cannot disagree.
- `McpAuth.tools(verify, metadataUrl, policy)` — the policy asked per
  TOOL and per REQUEST: a bearer arrives on every request, so a
  permission withdrawn between two calls stops the next one rather
  than the next reconnect. It filters the ANSWER to `tools/list` and
  does not compose one, because the stage owns
  InvalidRequest-before-initialize and MethodNotFound and a guard that
  serves its own list drifts from them. No route cache: its natural
  key is the allowed SET, and a holder can attenuate into arbitrarily
  many subsets, each minting a session table, a channel and a fiber.
- `McpAuth.capabilities(rootKey, ...)` — the credential is a
  `Capability` and the question is the capability's OWN. No new caveat
  kind: `Caveat.Scope` already spells a tool, so a holder narrows the
  tool set for the agent it hands the capability to with no issuer, no
  registry and no round trip — and the authorization it was granted is
  untouched, because the root capability never moved. That is the
  second half of the stranger's question, and it is the half OAuth
  scopes cannot answer without going back to the issuer.

Two older defects fell out of the tests, neither in okay-security:

- **okay-mcp's tools capability guard was dead code.** It sat below
  its own handlers, so a tool-less server answered `tools/list` with
  an empty list — the "polite empty list" the file's own comment
  refuses — and `tools/call` with "no such tool". Invisible while a
  tool-less server was a curiosity; `Serving.only` makes it ordinary.
- **okay-http handed one request's answer to the next.** `McpHttp`
  decided whether to await a reply by asking whether the inbound
  message was a `Request`, while the stage also answers a `Failed`. A
  malformed body's error was left in the session's channel and
  collected by the following call: measured here, a `tools/call`
  answering `-32600` for the POST before it. One malformed body
  shifted every later reply on that session by one.
  (okay-http/BUGS.md mcp-unowed-answer-crosses-requests.)

16 tests, no port bound — a route is a function, so the whole door is
driven by calling it with a Request; the suite stays in the default
gate while TestMcpAuth, which binds, does not. The ones that matter
are the refusals: the table NEVER RUNS for a tool the caller may not
use (asserted by a tool that records having been called), a refused
tool is byte-for-byte what a misspelled one answers, a caveat removed
fails the chain, and a caveat kind the verifier cannot enforce refuses
rather than being ignored.
