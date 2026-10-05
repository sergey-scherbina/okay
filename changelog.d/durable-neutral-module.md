## durable-neutral-module — agent-independent durable operation handlers

The generic per-operation journal/recovery handler now lives in
okay-durable, with TopicJournal in okay-durable-persist. Agent APIs export
or delegate to the same implementation, preserving rebuilt source usage,
including both TopicJournal constructor forms. Existing version-1 record
bytes are unchanged. Python/R durable tests now depend on the neutral
module, without agent/LLM/RAG; Journalled remains in codec.

Implementation commit: `durable: extract neutral handler and persist
adapter with agent facades`. Contract: specs/durable-neutral-module.md.
Validation: 85 scoped checks, JVM/JS portable tests, legacy agent,
Conversation/FileVersions, Python/R canned journals, overlay and docs;
agent JS/Scala2 compilation and classpath independence checks. No compile
warnings. Native and JVM binary compatibility are not claimed; downstream
clients need recompilation. First-attempt WithKey and run-scoped-key bugs
remain open in the new okay-durable backlog section.
