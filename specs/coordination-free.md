# Coordination-free: a clock, an identity, and state that merges

## Overview

The operator's direction (2026-09-09) named three things — ULID /
UUIDv7, "распределённые структуры данных типа CRDT", and "токены или
тикеты" — and they are one direction: **acting without asking anyone.**
An identity you can issue locally, state that converges without a
lock, and authority that proves itself instead of being looked up.

What the tree holds today, grepped rather than remembered:

- **ULID / UUIDv7: nothing.** `UUID` appears only as a *column type*
  (`SqlType.Uuid`, mapped in okay-jdbc and okay-r2dbc).
  `java.util.UUID.randomUUID()` — version 4, pure random — is called
  ad hoc in four places: `McpHttp` session ids, `Smtp` message ids,
  and twice in ACME tests. Nothing sortable, nothing monotonic.
- **CRDT: nothing.** The only `lww` in the repository is a local
  function inside `okay-cache`'s `TestView`.
- **`okay-cluster` is not a cluster**: one `Acceptance` object that
  both a JS client and a JVM server compile, so the cross-platform
  policy has an acceptance test.
- **Tokens: a lot, and all central.** `okay-security` has `Jwt`,
  `Es256`, `OAuth2`, `Oidc`, `Password`, `Secure`. An issuer signs and
  a verifier checks. There is no attenuation — no way for a holder to
  narrow its own right and hand it on without the issuer.

So the third leg exists in its centralized form and the first two do
not exist at all.

**Why this fits okay better than it looks.** `okay-cache`'s `View`
says it in its own comment: *"a cache over a log is a CONSUMER, and a
consumer is never invalid, only BEHIND, by a measurable amount"* — and
`View` is built from a fold, `(Option[V], Record) => Option[V]`. A
CRDT **is** a fold with laws: commutative, associative, idempotent.
The machinery is already standing — okay-persist as the truth,
Schema/codec for the wire, `Stm` for a local cell. What is missing is
the vocabulary and the laws, not the infrastructure.

And a sortable id is not decoration for a log-first library. With the
journal as truth and the relational store derived, a time-ordered key
means **range scans over time with no secondary index** — which a v4
UUID cannot give at any price, because it is random by construction
and destroys locality in every B-tree it touches.

## The insight this arc turns on

**One clock pays for both halves.** An LWW register needs a timestamp
that survives a clock stepping backwards. A monotonic id needs exactly
the same thing. That is a *hybrid logical clock*, and it is built once
here rather than twice.

**ULID and UUIDv7 are the same 128 bits.** Both are 48 bits of
Unix milliseconds followed by entropy; they differ in six bits
(UUIDv7 spends 4 on a version and 2 on a variant, RFC 9562) and in how
they are spelled — Crockford base32, 26 characters, versus hyphenated
hex, 36. They are not two types. They are one value with two
renderings, and treating them as two would duplicate the hard part
(monotonicity) in two places.

## Interface

`Hlc` and `Uid` live in **core** (`okay`), cross-platform on JVM, JS
and Native: everything else in the library wants them — persist keys,
chat message ids, `McpHttp` sessions — and a separate module would
make okay-persist depend upward.

```scala
package okay

/** A hybrid logical clock: 48 bits of milliseconds, 16 of counter,
  * packed in one Long so comparison is a Long comparison. */
opaque type Hlc = Long

object Hlc:
  /** the next stamp from this clock: never smaller than the last one
    * it issued, whatever the physical clock does */
  def next(): Hlc

  /** merge a stamp seen from elsewhere — the HYBRID half: after this,
    * our stamps are above anything we have been told about */
  def observe(remote: Hlc): Hlc

  def millis(h: Hlc): Long
  def counter(h: Hlc): Int
  given Ordering[Hlc]

/** 128 bits: 48 of Unix milliseconds, then entropy. One value, two
  * spellings — `ulid` and `uuid`. */
final case class Uid(hi: Long, lo: Long)

object Uid:
  def next(): Uid                    // monotonic, from the ambient Hlc
  def ulid(u: Uid): String           // 26 chars, Crockford base32
  def uuid(u: Uid): String           // 36 chars, RFC 9562 version 7
  def parseUlid(s: String): Option[Uid]
  def parseUuid(s: String): Option[Uid]
  def millis(u: Uid): Long
  given Ordering[Uid]
```

The clock is **injected, not ambient-only**: `Hlc.at(source)` takes a
`() => Long` so a test can step time backwards. Every law below is
written against a controllable source; the parameterless `next()` is
the convenience over `System.currentTimeMillis`.

## Behavior

**The laws, which are the deliverable.**

1. *Monotonic.* Successive `Uid.next()` strictly increase, one thread
   or many, including many within one millisecond.
2. *Sortable as text.* For any two ids, the lexicographic order of
   their `ulid` strings equals the numeric order of the values equals
   the order in which they were issued. (This holds for `ulid` by
   construction; hyphenated hex is checked too.)
3. *A backward clock does not go back.* With a source that steps
   backwards by a minute, ids still increase. This is the law the
   whole design exists for.
4. *RFC 9562.* `uuid` renders version 7 and variant `0b10`.
5. *Round trip.* `parseUlid(ulid(u)) == Some(u)` and the same for
   uuid; garbage parses to `None` rather than throwing.
6. *Uniqueness under concurrency.* N threads, M ids each: no
   duplicates.

**What `Hlc` guarantees and what it does not.** It gives a total order
that agrees with causality for events that have exchanged stamps. It
does **not** give a global physical time, and two nodes that never
communicate can order concurrent writes arbitrarily — which is what
"last write wins" always meant, and why LWW is one choice in stage 2
rather than the only one.

## Stages

- [x] **0 — the spec and the claim.**
- [x] **1 — `Hlc` and `Uid` in core**, cross-platform, with the laws
      above. Landed 2026-09-09.
- [x] **2 — `okay-crdt`**: `Crdt[A]` with `merge`, and its laws
      (commutative, associative, idempotent) as a REUSABLE check that
      every instance runs. Instances: `GCounter`, `PNCounter`,
      `LwwRegister` (over `Hlc`), `GSet`, `OrSet`. Laws before
      instances — a merge that is not idempotent makes the type a lie.
      Landed 2026-09-09.
- [ ] **3 — the seam**: a `Crdt` is a fold, so it meets okay-cache's
      `View` and okay-persist directly; `Schema` for the wire so a
      replica ships as data.
- [ ] **4 — capability tokens**, decided 2026-09-09. HMAC-chained
      attenuation: each caveat is signed with the PREVIOUS signature
      as its key, so anyone can narrow a token and nobody can widen
      one — the key for the previous step no longer exists. A verifier
      needs neither a registry nor the issuer.

## Decisions

**ANSWERED 2026-09-09: both, capabilities first.** The question was
which of two opposite readings "tokens or tickets" meant, and the
operator's decision is to have both in that order.

- **Stage 4 is capability tokens** (macaroon-shaped): the holder
  narrows its own authority offline — "this token, but read-only, and
  only until Friday" — and passes it on without the issuer being
  present. This finishes the direction the rest of the spec is going
  in: an id issued locally, state merged locally, and now a right
  CHECKED locally.
- **Tickets and leases go to their own spec** (`specs/leases.md`, and
  filed in BACKLOG): TTL leases, fencing tokens against a zombie
  owner, numbered queues. They are legitimate and they are
  *coordination* — the thing this spec is about avoiding — and they
  belong on okay-persist, which already has Raft-backed leadership,
  rather than beside the CRDTs. Putting them here would make the spec
  mean two things.

Both want `Hlc`, which is the third time this arc's clock pays for
itself: a capability's `until` and a lease's expiry are the same
question about time that a wall clock answers badly.

**Decided: `Uid` is a case class of two Longs, not an opaque 128-bit
type.** JS has no 128-bit integer and its `Long` is emulated;
splitting explicitly keeps every platform reading the same code, and
`Ordering` compares hi then lo unsigned.

**Decided: the clock is a parameter.** The bug this design exists to
prevent is a backward clock, and a design whose central hazard cannot
be tested is not designed.

## Out of scope

- Distributed consensus. okay-persist already has Raft; nothing here
  competes with it.
- Delta-CRDTs and causal-stability garbage collection. Stage 2 ships
  state-based (convergent) types, where merge takes two whole values.
  Deltas are an optimisation with the same laws, additive later.
- Byzantine settings. Merge here trusts its inputs; a replica that
  lies is an authentication problem, which is stage 4's business.
- Applying any of it to Okay!Chat. The operator's instruction is
  library first — the chat is where it gets used, and it gets its own
  entry once there is something to use.

## Results

**Stage 1 landed (coordination-free, 2026-09-09).** `Hlc` and `Uid` in
core. 15 laws on JVM, JS and Native (`src/test/scala-cross`), 2 more
needing real threads on the JVM only. Gate green, 0 warnings — 721 /
42 / 50, where JS and Native each gained exactly the 15 shared laws.

Two things worth keeping from building it.

**The test placement was a real hole, not a formality.** The laws
first went in `src/test/scala`, everything passed, and the claim's
"every law runs on all three platforms" looked satisfied. It was not:
for core, `src/test/scala` is JVM-ONLY, and the suite that also runs
on JS and Native is `src/test/scala-cross` (build.sbt, `Test /
unmanagedSourceDirectories`). `okayJS/test` and `okayNative/test`
went green because the MAIN sources compiled there — which proves the
code builds cross-platform and says nothing about whether it behaves.
The tell was in the log: `okay.TestUid:` appeared once, not three
times. Moving the files turned 27 -> 42 on JS and 35 -> 50 on Native,
and those +15 are the evidence the claim actually asked for.

**Two spellings, one value, is cheaper than it sounds.** Because the
canonical form is a conforming UUIDv7, the ULID rendering is a base32
walk over the same 128 bits and the version/variant bits are constant
— so they contribute nothing to a comparison, and lexicographic order
of the text equals numeric order of the value equals the order the
ids were issued in. That is one law, tested once, covering both
spellings.

The 12-bit counter is what the layout has room for beside a version
and a variant, so `Hlc.Clock` grew a `counterBits` parameter rather
than `Uid` growing a second clock. The FIELD is always 16 bits wide,
so `Stamp` keeps one layout and its accessors never need to know who
made it; a clock may simply choose to use fewer and borrow a
millisecond earlier.

**Stage 2 landed (coordination-free, 2026-09-09).** Module
`okay-crdt`, JVM + JS + Native: `Crdt[A]`, `Crdt.violations`,
`NodeId`, `GCounter`, `PNCounter`, `GSet`, `OrSet`, `LwwRegister`.
14 tests, green on all three platforms, 0 warnings.

`Crdt.violations` ships in MAIN rather than in the tests, and that is
the decision the stage turns on. The laws are the content — a type
whose merge is not idempotent is not "mostly a CRDT", it is a type
that silently disagrees with itself under redelivery — so anyone
defining an instance can run the check, and every instance here does.
It answers a LIST of what broke instead of throwing at the first
failure, because a merge is usually wrong in one law and right in the
others and knowing which one is the diagnosis. Its own test proves
that: a merge that ADDS breaks idempotence and only idempotence, and
the check says exactly that, three times, once per sample.

Two things the types are shaped by rather than decorated with:

- **`LwwRegister` has a precondition, and it is load-bearing.**
  `(at, by)` must identify a write uniquely, which holds when each
  node stamps from its own `Hlc.Clock` — a clock never issues one
  stamp twice. Two hand-built registers with the same stamp, the same
  node and different values break commutativity, and the fix is NOT a
  hash of the value: hashes differ across platforms, and a merge that
  disagrees between JVM and JS is worse than one with a stated
  precondition.
- **`OrSet` is where `Uid` does a third job.** A tag has to be unique
  without coordination, which is exactly what a locally issued
  sortable id is. The add-wins rule is not a preference: the
  alternative loses writes the remover never observed, which is a lost
  update with extra steps.
