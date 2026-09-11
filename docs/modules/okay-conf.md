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
| `Conf.envName` / `Conf.fromEnv` / `Conf.layered` | the ONE derivation of `OKAY_TLS_RELOAD` from `tlsReload`, the environment as a typed patch, and defaults < file < environment |

There is deliberately no `plain:` scheme. Invariants (the reason the
module exists): secrets never travel through effect operations,
never ride inside URLs, what is stored is reference-only by
construction, and errors name references, never values. Stage 2
(managed config) lives in okay-persist as `Configs` — the audit IS
the log, rollback IS a read. The linear given-chain style for edge
wiring is documented in docs/typepedia.md ("The edge patterns").

**A config's field list is a description, and a description needs a
consumer that can break** (optics-outside-conf, 2026-09-11). Because
`envName` derives the variable from the field, a program can publish
the whole list — okay-script's `Serve.Config.names` — and tests can
hold the list against what the deployment renders. That was done; what
was not is holding it against the PROSE, and a setting slipped through
the gap: `OKAY_ACME_EAB` was declared, deployable, read at boot, and
named in no guide, so the only user who needed it (anyone with a
commercial CA) could not find it. `TestScriptConfig` now asks the
question of `docs/okay-script-guide.md`.

The law runs in one direction only, and measuring said why: a program
reads variables its config case class does not declare. `OKAY_CONF`
names the config FILE, so it cannot be a field of what that file
parses into, and `OKAY_STAGING` is okay-staging's codec switch, which
every program shares. "Every variable in the guide is a setting" would
have been false on both.

What this repository deliberately does NOT have is a setting that
knows a nested path. `Serve.Config` says "flat and scalar on purpose":
what an environment carries is text, numbers, yes/no and a secret
reference, and the pairs a program actually wants — a certificate WITH
its key — are built where a half of one can be a named refusal. A lens
into `server.tls.port` would be machinery for a shape nothing here
has.
