# okay-durable

Generic journal, recovery and replay handlers for effect operations. JVM
and Scala.js; depends on okay and okay-codec, with no dependency on agent,
LLM or persistence engines.

## Use

Declare a `okay.codec.Journalled[Op]` instance for your operation type:
name, request fingerprint, key transport, execution/encoding and decoding.
Wrap an `Answers[Op]` with `okay.durable.Durable.over[Op]`. Provide a
`Durable.Journal` whose intent append completes durably before the external
request. `MemoryJournal` is for tests and in-process recording.

A fresh handler refolds recorded answers; `replayingOver[Op]` only answers
from the journal. Changed fingerprints raise `Drift`. Incomplete intents
follow the operation's policy: Redo, WithKey, Reconcile, Escalate, Fail or
Await. `OpTrace` connects spans without importing an observability library.
The typed Calc suite exercises this API without any agent dependencies.

## Storage and workflows

[okay-durable-persist](okay-durable-persist.md) supplies the TopicJournal
adapter. [okay-workflow](okay-workflow.md) and
[okay-persist](okay-persist.md) describe and drive longer-lived workflows;
their journals and runtime remain separate from this operation handler.

## Migration and limits

[okay-agent](okay-agent.md) retains the old `okay.agent.Durable` names and
Tool convenience methods as source-compatible facades. Recompile clients:
this extraction does not promise JVM binary compatibility.

WithKey injects the journal key on the first request and every retry;
the original request fingerprint remains unchanged. A supplied Tool key
is replaced by the journal key. External deduplication still depends on
the provider.
The module does not establish distributed writer ownership.

## Run identities and keys

A journal's `runId` must be stable across restart and unique across the
provider's deduplication namespace. Use one journal per ordered operation
sequence; coordinating concurrent handlers is outside this module.
Include tenant and workflow identity
when local run numbers can repeat. `MemoryJournal()` creates one UUID per
journal; `MemoryJournal(run)` accepts an explicit identity. The JS default
requires Web Crypto.randomUUID; other JS hosts supply an explicit identity. Memory storage
alone does not survive a process restart.

For new steps, `keyFor(journal, seq, op)` encodes that identity and position
as `okay-<base64url(run UTF-8)>-<seq>`, without padding. Run IDs must contain
1–96 UTF-8 bytes of well-formed Unicode; positions must be non-negative.
Keys contain only ASCII alphanumerics, '-' and '_', at most 144 characters.
Check the provider's key limits and deduplication retention window.
Fingerprints detect changed inputs; they do not determine scoped keys.

Custom journals implement `runId: Option[String]`. Fresh WithKey requests
without it are rejected before intent append or external execution. Other
unscoped policies retain their legacy behavior. Recorded `Entry.key` is
always authoritative, including incomplete legacy entries and replay spans;
no migration rewrites outstanding external keys. The two-argument
`keyFor(seq, op)` keeps the old unscoped algorithm for compatibility and
archived entries; use the journal-aware helper for new scoped steps.
