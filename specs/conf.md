# okay-conf: configuration as data, secrets as references

## Overview

The rule is already in force everywhere and written down nowhere:
every module takes its credentials as plain VALUES at construction
time (`Provider.openAi(transport, apiKey, …)`, `Llm.stream(transport,
apiKey, …)`, `JdbcInterop.connection(url, user, password)`), and only
the application edge ever finds them (okay-demo and the live suites,
from `OKAY_*` environment variables). The library reads no config;
the application resolves and hands values in. This spec names that
rule, and adds the two small pieces the stack is missing: a config
SHAPE (a case class with a derived `Schema` — the codec question was
answered in P5 and is not reopened) and a SECRETS seam, so that what
gets stored, committed, logged and journaled can never contain a
password by construction rather than by discipline.

Why this must be settled now rather than when it bites: two features
already in the stack write program data to durable places — the
`Durable` journal records operation arguments to disk, and
okay-persist keeps everything appended to it, forever if asked. A
password that travels through an effect operation or sits in a
stored config is a password on disk. The design below makes that
path not exist.

## The model

Three nouns:

- **Config** — a case class with a derived `Schema`. JSON to read
  and look at, CBOR to ship, field evolution by the codec's existing
  rules (optional fields decode absent). There is no config syntax,
  no interpolation language, no override cascade — a config is a
  VALUE, and merging or defaulting configs is ordinary Scala on
  ordinary values.
- **Secret** — a REFERENCE, not a value: `env:PG_PASSWORD`,
  `file:/run/secrets/pg`. It is what a config stores, prints and
  round-trips. `Schema[Secret]` encodes the reference string, so a
  config holding secrets is committable, loggable and storable in a
  topic BY CONSTRUCTION — there is nothing in it to leak. Its
  `toString` is the reference, so accidental logging shows
  `env:PG_PASSWORD`, never a password.
- **Secrets** (the resolver) — the seam that turns a reference into
  a value, once, at the application edge, at handler construction.
  A trait, not an effect row — the same decision as okay-persist's
  `Store` and for the same reason: programs do not request secrets,
  the edge that builds their handlers does.

Resolution produces a plain `String` and hands it straight to a
constructor. The resolved value has no `Schema`, no wrapper type and
no place in any config shape — it exists in the narrow gap between
`resolve` and `connection(url, user, password)`, which is exactly
the gap the modules already define.

## Interface

```scala
package okay.conf

import okay.codec.Schema

/** a reference to a secret; the value is NEVER here */
final case class Secret(ref: String):
  override def toString: String = ref     // safe to log by default

object Secret:
  given Schema[Secret]                    // encodes the reference

/** the resolver seam. Total: a miss is an answer naming the
 * REFERENCE and what was tried — never any value. */
trait Secrets:
  def get(s: Secret): Either[String, String]

object Secrets:
  val env: Secrets                        // env:NAME
  val file: Secrets                       // file:/path (one trailing newline trimmed)
  def memory(m: Map[String, String]): Secrets   // tests
  def chain(first: Secrets, rest: Secrets*): Secrets  // first answer wins

/** reading a config is just the codec plus a file:
 * total, damage is data, same as every decode in this stack */
object Conf:
  def read[A: Schema](json: String): Either[String, A]
  def load[A: Schema](path: java.nio.file.Path): Either[String, A]  // JVM/Native
```

A connection config in this shape — the pattern, not a prescribed
type, since each module's config belongs to that module:

```scala
final case class Db(url: String,          // NO credentials in here
                    user: String,
                    password: Secret)
given Schema[Db] = Schema.derived
```

and the edge wires it (the linear given-chain style for longer
edges — load -> resolve -> connect -> migrate — is documented in
docs/typepedia.md, "The edge patterns"):

```scala
for
  db  <- Conf.load[Db](path)
  pw  <- secrets.get(db.password)
yield JdbcInterop.connection(db.url, db.user, pw)
```

## Reference schemes

Two schemes now, chosen because together they cover a bare machine,
a container and a Kubernetes pod — which is most machines a business
actually runs:

- `env:NAME` — the 12-factor answer, already this repo's practice
  (`OKAY_*` by convention, not enforcement). Works on JVM and
  Native; on JS it is Node's `process.env` (the browser has no
  secrets to resolve and should not).
- `file:/path` — Docker and Kubernetes secret mounts, and plain
  files under 0400 on a bare host. Exactly one trailing newline is
  trimmed (the universal mount artifact); a missing file or a
  directory is an error naming the path.

An unrecognized scheme is an error naming the scheme — never a
guess, never a fallback to treating the reference as the value.
Vault-class managers arrive later as one more `Secrets`
implementation behind the same trait (the interop hatch, same shape
as okay-persist stage 3) — nothing above the trait changes.

## Invariants (the reason this spec exists)

1. **Secrets never travel through effect operations.** The `Durable`
   journal writes operation arguments to disk, and okay-persist
   keeps appends durably; therefore a credential may only enter the
   system at HANDLER CONSTRUCTION, as a constructor argument —
   never as an effect's payload, a tool argument, or a record
   value. The architecture already leans this way (traits, not
   effect rows); this line makes it a rule that a review can point
   at.
2. **Credentials never ride inside URLs.** `scheme://user:pass@host`
   puts a password everywhere the URL goes — error messages, stats,
   ops events, journals. Config shapes carry `url` + `user` +
   `password: Secret` as separate fields; the URL field is for the
   address.
3. **What is stored is reference-only by construction.** A config
   with a `Secret` in it round-trips through Schema with the
   reference intact and nothing else; there is no API that writes a
   resolved value into any okay-owned durable place.
4. **Errors and toString name references, never values.** A failed
   resolution says which reference and which scheme failed; a
   resolved value appears in no okay-owned error, log line or
   `toString`.

## Config over the log (stage 2)

A config that must be managed — several services, an operator, an
audit question — goes into okay-persist as a COMPACTED KEYED TOPIC:
key = the config's name, value = its Schema encoding (references
only, per invariant 3, so the topic is as committable as the file
was). Then, with nothing built: history of every change is the log,
"who changed what when" is a read, rollback is reading an older
offset, `latest` is the same compacted-topic story ui-durable
already uses. Managed configuration stops being a subsystem and
becomes one more consumer of the one persistence primitive.

This stage ships as a thin convenience over `Store` when a consumer
names the need; it adds no new machinery and MUST not (that is the
point).

The convenience (conf-topic), living in okay-persist beside
Snapshots and Offsets, where the compacted-keyed-topic pattern
already lives (okay-conf keeps its codec-only dependency):

```scala
final class Configs(topic: Topic):        // keyed, compacted
  def put[C: Schema](name: String, value: C, ack: Ack = Durable): Long
  def latest[C: Schema](name: String): Option[(Long, Either[String, C])]
  /** rollback IS a read: the newest write at or before `offset` */
  def at[C: Schema](name: String, offset: Long): Option[(Long, Either[String, C])]
  /** the audit: every surviving write under this name, oldest first */
  def history[C: Schema](name: String): Vector[(Long, Either[String, C])]
```

Values travel as the Schema's JSON (a config is for looking at, and
the log is one more place it gets looked at); reference-only safety
is invariant 3 by construction — Schema[Secret] encodes the ref.
History is honest about compaction: the audit lives until
`Topic.compact` reclaims superseded writes, and after it `latest`
still answers — the exact records compaction keeps.

Stage-2 behavior:
- [x] three writes under one name: latest is the third, at(second's
      offset) is the second (rollback is a read), history lists all
      three oldest-first with their offsets
- [x] two names on one topic do not bleed — keys filter
- [x] a damaged stored value decodes as a Left in place, the rest of
      the history intact
- [x] after Topic.compact: latest and its offset unchanged, history
      shortened to what compaction keeps — stated, not hidden

## Module

`okay-conf`, small by design: `Secret`, `Secrets` (env/file/memory/
chain), `Conf.read`/`Conf.load`. Cross-built JVM/Native/JS like the
trait half of okay-persist; `file:` and `Conf.load` are JVM/Native
(Node's fs can join when a consumer needs it). Depends on
`okay-codec` only. Implementation is a separate claim (conf-impl in
BACKLOG); this spec is the contract it builds to.

## Behavior

- [x] a config case class loads from JSON via its derived Schema;
      absent optional fields decode absent (the codec's own rule,
      asserted here at the config seam)
- [x] a `Secret` field round-trips: read, written back, the
      reference is intact and no other representation exists
- [x] `env:` resolves from the environment; a missing variable is an
      error naming `env:NAME` — the test asserts the error CONTAINS
      the reference and nothing resembling a value
- [x] `file:` resolves file content with exactly one trailing
      newline trimmed (content ending in two keeps one); a missing
      path or a directory is an error naming the path
- [x] an unrecognized scheme (`vault:x` today) is an error naming
      the scheme; the reference is never used as the value
- [x] `chain`: the first resolver that answers wins; when all miss,
      one error names the reference once
- [x] `memory` serves tests; `Secret.toString` is the reference
      (asserted, since logs are where discipline fails)
- [x] (stage 2) a config topic: latest-by-key serves the current
      config, an older offset serves the old one, and the write that
      changed it is in the log

## Out of scope

- encryption of stored values at rest — we store no values; when a
  requirement to store them appears, the answer is an interop with
  something that already does sealed boxes well, behind `Secrets`,
  not homegrown crypto
- vault/cloud secret managers in v1 — one more `Secrets` behind the
  same trait, when a deployment names one
- hot reload and file watching — re-read the file, or read the
  config topic; a watcher is platform mess for a poll a consumer
  can own
- fighting heap forensics (char[] zeroing) — a JVM string lingers
  until GC regardless; the API damage is real and the protection is
  not, so the fight is declined openly
- an override/merge/profile cascade — configs are values; Scala is
  the override language

## Decisions

- **References, not values** — the whole design: a stored config
  cannot leak what it does not contain, and the Durable/persist
  write paths stay safe without a redaction layer. Rejected:
  secrets inline in config files (works until the first commit,
  journal or log line; redaction-by-discipline is the alternative
  that fails silently).
- **No `plain:` scheme** — a value-in-reference scheme would
  reintroduce exactly the leak the design removes, one convenience
  at a time. Tests get `Secrets.memory`; local dev gets `env:`.
  Rejected: `plain:` for dev ergonomics.
- **A trait, not an effect** — programs do not request secrets; the
  edge that constructs their handlers does. Precedent: `Store`,
  `Durable.tools(inner, journal)`. Rejected: a `Secret[A]` effect
  row (would put credentials INTO the operation stream, which is
  the one place invariant 1 forbids).
- **Schema is the config codec** — one algebra already serves JSON
  to read and CBOR to ship, with evolution rules. Rejected:
  HOCON/typesafe-config (a dependency, a second syntax, and an
  interpolation language whose only needed feature — reaching env —
  the references do explicitly).
- **Static configs in v1** — a config is read where it is needed;
  managed/dynamic config is the stage-2 topic, which also answers
  audit and rollback. Rejected: watchers and reload callbacks
  (races and platform variance for what a poll or a topic read
  states plainly).
- **`toString` = the reference** — logging a config must be safe by
  default, because it will happen. Rejected: opaque toString hiding
  the ref too (debugging needs to see WHICH secret; it never needs
  the value).

## Results

Shipped 2026-09-01 (conf-impl): okay-conf cross-built JVM/JS/Native,
depending on okay-codec only. 12/8/8 tests — the SHARED suite proves
`env:` on all three platforms (PATH resolves everywhere; a miss
names `env:NAME` verbatim), the JVM suite covers file:/load (the
Native leg compiles the same Platform source). Two small deviations
from the sketch, both stated: `Conf.load` takes the path as a String
so the signature exists on every platform (JS answers a named
refusal until Node's fs joins), and `Schema[Secret]` travels as the
BARE reference string since codec-iso landed the iso node the first
form waited for. `chain` prefers a
matched scheme's own miss over an unrecognized-scheme shrug, so the
one error is the specific one. Stage 2 (the config topic) remains
conf-topic in BACKLOG.
