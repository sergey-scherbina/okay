## auth-either-pyramids - McpAuth and Oidc: Either pyramids flattened with Throws

Found while looking for consumers of `Layered`. The Either-over-program
pyramids in the auth code are served by okay's own `Throws`, not by
`Layered`: `Throws` needs no Delim machine, and `Layered` is for monads
that are not okay effects.

- `McpAuth.discover` / `connect` (okay-mcp-http). A four-level nest of
  `case Left(e) => pure(Left(…))` is now one flat for-comprehension over
  `Throws % String + Async`, with three helpers (`when`, `present`,
  `right`). `runEither` at the edge keeps the public signatures
  (`Either[String, …] ! Async`) and every message byte for byte. `connect`
  reuses the flat `discovering` instead of re-matching `discover`'s Left.
- `Oidc.callback` (okay-security): the same shape, the same messages.
- The suites are `Live` (they bind ports), so they were run with
  `--include-tags=Live`: TestOidc 2, TestMcpAuth 4, all green.
