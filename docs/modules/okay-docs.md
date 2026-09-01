# okay-docs

> The document seam: get/put/delete by key with compare-and-set as
> DATA, bounded queries over declared secondary indexes, per-item
> atomicity — the one new seam specs/data.md allows itself.

Depends on: the core, `okay-codec`, `okay-persist`. Cross-built.
The Mongo adapter is a satellite (`okay-docs-mongo`, JVM) that
pays the driver dependency.

## Guide

**Cond is the load-bearing part.** `Always | IfAbsent |
IfVersion(v)` — conditional writes are how WithKey and optimistic
concurrency are spelled at this seam, and `PutResult` answers
applied-or-not as data: `Stale` carries what the engine holds NOW,
so a lost-ack retry reconciles without a second read.

**The own posture is the log.** `TopicDocs` folds a compacted
keyed topic: the version IS the record offset (the log hands out
CAS tokens for free), a delete is the tombstone, and a cold node
refolds the same store. `grants` answers Strong — a single fold
has no weaker truth to offer.

**Foreign engines keep their own CAS.** `MongoDocs` maps every
conditional write to ONE server-side operation (find-and-modify, a
filtered delete) — never read-then-write hope; declared index
fields materialize as real Mongo indexes, and a query on an
undeclared field refuses by name rather than scanning.

**No multi-document transactions, deliberately.** A multi-item
change is a journaled sequence of conditional writes — the Durable
policies over CAS, the saga pattern with machinery this stack
already has.
