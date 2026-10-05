# Durable run-scoped operation identities

## Overview

The old name/sequence/32-bit fingerprint hash key aliases independent
runs. Give each journal a stable run identity; new external attempt keys
identify that run and position, while fingerprints continue to guard drift.

## Interface

Journal gains concrete `def runId: Option[String] = None`, retaining source
compatibility for existing custom journals and read-only archived versions.
MemoryJournal keeps its zero-argument constructor (fresh UUID identity once
per journal) and gains a constructor taking an explicit run String.
TopicJournal exposes its existing run argument as the stable runId.
Callers must make run IDs unique across the provider's deduplication
namespace, including tenant/workflow identity if local run numbers repeat.
Persist that identity or reconstruct it from stable application metadata.

New `keyFor(journal, seq, op)` uses journal identity; the original unscoped
`keyFor(seq, op)` remains only for source compatibility/archived versions.
Agent adds the corresponding ToolCall helper and MemoryJournal factory.
New scoped key format: `okay-<base64url-without-padding(run UTF-8)>-<seq>`.
Run must be non-empty, well-formed UTF-8 round-trippable String, at most
96 bytes; seq must be non-negative. Keys use ASCII alphanumerics, '-' and
'_', at most 144 characters. No 32-bit fingerprint hash participates.
The provider's own limits and retention/deduplication behavior still apply.

## Behavior

- [ ] Independent runs (including tenant-local run numbers) have different
  external keys; an explicit run/step identity is stable across restart.
- [ ] Colliding old fingerprints cannot merge independent requests;
  changed input at the same recorded position still raises Drift.
- [ ] Both completed and incomplete legacy Entry.key values are reused
  verbatim; recovery/replay spans carry that stored key, not a new key.
- [ ] Fresh WithKey on a custom journal lacking runId fails before append
  or external execution. Legacy incomplete recovery still works without
  a runId. Other unscoped policies retain legacy behavior.
- [ ] MemoryJournal default identity is created once per journal;
  TopicJournal derives identity from its existing run without wire changes.
- [ ] Key constraints reject malformed/oversized/empty run identities and
  negative positions; valid Unicode IDs remain distinct.
- [ ] Existing agent/Scala2/foreign/obs APIs and first-attempt WithKey
  regressions remain valid on JVM and portable tests on JS.

## Decisions

Use reversible bounded Base64url encoding, not a hash: no accidental
32-bit collisions or crypto dependency, and no loss of run identity.
The key identifies an attempt, not its input; drift handles input changes.
Use recorded Entry.key whenever available, allowing old formats and
namespace changes on restored journals without rewriting remote keys.
Missing identity fails closed only for fresh WithKey, because that policy
requires isolated external keys. No random identity is created per replay:
MemoryJournal holds it, TopicJournal derives it, custom journals provide it.
This does not provide multi-writer fencing or change journal ownership.

## Validation

Portable tests for namespace separation, restart, old hash collision,
legacy incomplete recovery and trace identity, rejection before external
calls and key format boundaries. Topic adapter identity and wire fixtures,
legacy Tool/obs/Scala2 acceptance; then affected master staged.

## Results

Pending implementation and verification.
