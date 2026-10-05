# Neutral durable operation handler

## Overview

Extract the already-generic per-operation journal/recovery handler from
okay-agent. This follows specs/llm-agentic.md (Any operation, not only a
tool), specs/persist.md and specs/core-modules.md's dependency cut.
Workflow notation and Dialogue/Worker remain in their existing modules.

## Interface

- okay-durable, package okay.durable: Durable.over, replayingOver,
  generic keyFor[Op], Entry, Journal, MemoryJournal, OnRepeat,
  Drift/Unresolved/Awaiting, argsOf/awaiting, and OpTrace.
- Journalled remains okay.codec.Journalled. The neutral module depends
  on okay + okay-codec. The public await payload remains Json.
- okay-durable-persist, package okay.durable.persist: TopicJournal and
  its existing Rec schema over okay-durable + okay-persist.
- okay.agent.Durable exports the generic API and retains tools,
  replaying, ToolCall keyFor and KeyField. Agent OpTrace and TopicJournal
  are source-compatible aliases with companion exports where needed.
  Compatibility targets source rebuilds, not precompiled JVM binaries.

## Behavior

- [x] A custom typed operation records, resumes, replays and rejects drift
  using only the neutral module, without agent or persistent storage.
- [x] Existing agent Tool/Conversation/FileVersions/observability APIs
  compile and preserve recovery, exception and trace behavior.
- [x] TopicJournal reads the existing version-1 Intent/Complete envelope
  and retains its partition/key/ACK behavior; no storage migration.
- [x] Python and R journal tests use the neutral module without agent
  in their test dependency graph.
- [x] JVM and JS compile and run the portable neutral tests; adapters
  retain JVM/JS support. Native support is outside this extraction.

## Design

The neutral implementation is the single source of semantics. Agent
compatibility facade exports types/companions and generic methods, and
only Tool adapters delegate to it. TopicJournal similarly has one
implementation; legacy construction and Rec names resolve to it.
The generic key helper uses Journalled name/fingerprint and retains the
existing key algorithm. Legacy ToolCall helper delegates to it.

## Decisions

- Two modules: a caller with a custom journal need not import persist
  (and transitively workflow). No cycle from persist back to its adapter.
- Keep Journalled in codec: codec-owned instances cannot depend on a
  module that already imports codec.
- Preserve behavior in this lane: first-attempt WithKey and run-scoped
  key correctness remain open, explicitly retargeted backlog items.
  Mixing these fixes into a module extraction would obscure its contract.
- Keep agent convenience APIs and tested old construction syntax rather
  than force every downstream application to migrate at once.
- Do not claim binary compatibility: Scala aliases/exports preserve
  rebuilt source APIs, not the original emitted class names.

## Validation

Move the typed Calc suite into the neutral module and adopt Diagnosed;
keep legacy Tool suites as compatibility acceptance. Add an adapter
fixture using explicit pre-extraction version-1 wire bytes, independent
of the moved Rec codec. Gate these suites and the Python/R journal
consumers plus relevant agent/obs/Scala2 compilation. Build graph and
docs guards prove module independence and registration. No benchmarks.

## Results

85 scoped test results passed: typed Calc on JVM/JS (12), fixed-wire
adapter tests on JVM/JS (4), legacy agent/Conversation/FileVersions (45),
Python/R canned journal suites (9), tracing overlay (2), docs guards (11),
and explicit legacy/neutral compatibility (2). Agent JS test sources and
Scala2 agent facade compile. No compile warnings in the changed modules.
Live Python/R processes were not required; the edited live suites compile
with their existing Live tags and retain provider-dependent acceptance.

Classpath checks: neutral Compile has no persist/workflow/agent/LLM/RAG;
adapter Compile and Python/R Test have no agent/LLM/RAG. The neutral
module remains JVM/JS only. Fixed legacy CBOR envelopes decode, and new
writes match those bytes exactly. Initial compatibility compile exposed
that a type alias with a companion does not retain universal apply;
an explicit forwarding TopicJournal.apply fixes that, with both old
constructor forms now covered. Outstanding idempotency issues remain
open in backlog.d/okay-durable with updated source pointers.

Follow-ups, 2026-10-05: specs/durable-withkey-first-attempt.md fixes key
transport on the fresh request, foreign-workflow imports and actual
Scala 2 TASTy-reader compatibility (explicit agent type/factory aliases).
specs/durable-run-scoped-keys.md adds stable journal identity and new scoped
keys; the old helper is retained only for compatibility/archived entries.
The extraction's outstanding P1 key issues are resolved by those lanes;
process-crash and concurrency evidence remains a separate backlog item.
