# okay-conf

Configuration as data, secrets as references (specs/conf.md): a
config is a case class with a derived `Schema`; a `Secret` is a
REFERENCE (`env:PG_PASSWORD`, `file:/run/secrets/pg`) — the value
exists only in the gap between `Secrets.get` and a constructor
argument, so nothing okay-owned can ever persist it. Cross-built;
depends on okay-codec only.

| | |
|---|---|
| `Secret` | the reference; `toString` IS the ref — logging is safe by default; travels as the bare string on the wire (codec-iso) |
| `Secrets` | the resolver seam: `env:` (JVM/Node/Native), `file:` (one trailing newline trimmed — the mount artifact), `memory` (tests), `chain` (first answer wins; the one error is the specific one) |
| `Conf.read` / `Conf.load` | the codec plus a file; total, damage is data; `load` is JVM/Native (JS answers a named refusal) |

There is deliberately no `plain:` scheme. Invariants (the reason the
module exists): secrets never travel through effect operations,
never ride inside URLs, what is stored is reference-only by
construction, and errors name references, never values. Stage 2
(managed config) lives in okay-persist as `Configs` — the audit IS
the log, rollback IS a read. The linear given-chain style for edge
wiring is documented in docs/typepedia.md ("The edge patterns").
