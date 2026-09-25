## foreign-one-r — one engine under every wire language, R on the network (2026-09-26)

Stage 1 of specs/foreign-one.md. `okay.py.WireSession` is the
language-neutral wire — the handshake (shim version, a `fatal` the far
side names, auth, format/compression/frames), exchange, deadline, the
Arrow road, a broken or ended wire as DEAD, `verify` — and
`ForeignWorker` (Python's values, for Python, TypeScript, Haskell, Go and
Rust) and `okay.r.RSubprocess` (R's values) are the handlers over it.
R's second copy of all of that is gone (`ForeignWorker` 410 → 237 lines,
`RSubprocess` 507 → 348, the session 265).

R gains what every other language had: `RSubprocess.connect(host, port)`
under the same `WireAuth`/`WireSecurity` givens, `RSubprocess.command`
for `ForeignGateway`, `RSubprocess.over(link)`; a timeout over TCP
reconnects to a fresh R and replays a multi-shot program onto it
(`TestRNetwork`, 4, new). docs/one-language.md's R row and "Limits"
updated; its "R is not behind the gateway" is gone.

Narrowed, and said so (spec Decision 12): R's VALUE tree (`RValue`,
`RCodec`) joins the one `Value` in stage 2 (foreign-one-protocol) and R's
own respawn-and-replay folds into the pool in stage 3 (foreign-one-pool).

Live: okay-r 90 passed (R 4.4.1 in docker), okay-py 226, okay-rust FFM and
wasm 24 — no existing test changed. Mutant: the session's `fatal` check
removed fails R's "without jsonlite refuses BY NAME". Also removed an
unused import in okay-foreign-cluster's `Stateful.scala` a cold compile
surfaced.
