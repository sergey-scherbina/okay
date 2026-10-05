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
the provider, and run-scoped-key isolation remains open in the backlog.
The module does not establish distributed writer ownership.
