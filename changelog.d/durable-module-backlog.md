## durable-module-backlog — neutral Durable extraction plan

Recorded durable-neutral-module after inspecting the existing split:
generic per-operation handling is in okay-agent, workflow notation in
okay-workflow, and durable storage/runtime in okay-persist. Proposed a
neutral okay-durable core, a persist adapter and compatible agent facades;
kept Journalled in codec to avoid a reverse dependency. No implementation
moved. Validation: board/changelog shape and whitespace guards.
