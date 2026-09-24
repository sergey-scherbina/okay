## wire-auth - who may speak on the wire: given WireAuth

Stage 5b, part one, of specs/polyglot-one-wire.md: the operator's
"авторизацией - через имплисииты (и соответствующие внешние механизмы -
у каждого имплисита свой)".

- okay-codec `WireAuth`: a mutual HMAC-SHA256 challenge at the
  handshake, before the stage 5a configure. No given means no
  authentication. `WireAuth.fromEnv(name)`, `fromFile(path)` and
  `secret(bytes)` each name their own source, which is read when a worker
  is opened; a missing or empty secret is refused by name.
  `ForeignWorker.over/speaking/connect` take it as `using`.
- Go `ServeTCP` and Rust `serve_tcp` read `OKAY_WIRE_SECRET` or
  `OKAY_WIRE_SECRET_FILE`. They announce the challenge with a random
  nonce, check the host's mac in constant time, prove their own, refuse
  every request before the auth, and close on a wrong mac. Rust has HMAC
  over `sha2` (offline, the only new crate).
- Tests: `TestWireGivens` (RFC 4231 vector, mutual pass, a lying server,
  refusals, fromFile) in the default gate; `TestGoTcpAuth` and
  `TestRustTcpAuth` run the conformance suite behind a secret plus four
  refusals, live. Mutant: a Go server that skips its check was still
  refused, by the host, because the proof is mutual.
- Docs: one-language.md "Who may speak: WireAuth", with HMAC's literature
  and the reflection defence; go.md; the Limits entry now says "not yet
  encrypted" instead of "unauthenticated".
