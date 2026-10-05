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

Behavior is preserved, including the outstanding first-attempt WithKey
and run-scoped-key issues in the backlog. Moving modules does not resolve
external exactly-once outcomes or establish distributed writer ownership.
