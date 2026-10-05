- [ ] durable-neutral-module — P2 / architecture: extract generic durable
      effect handling from okay-agent into a reusable okay-durable module.
      Operator discussion 2026-10-05, following durable-execution-backlog.
      BASELINE: Durable.scala has generic `over[Op]`, `replayingOver[Op]`,
      Entry/Journal/MemoryJournal, recovery policies and neutral OpTrace;
      Tool-specific `tools`, `replaying`, keyFor and ToolCall adapters are
      in the same object. `Journalled[Op]` already lives in okay-codec;
      Py/REval/ForeignEval use that seam without a compile dependency on
      agent, but their Durable tests still depend on okayAgent. Generic
      durability currently imports the LLM/RAG/frame/actor dependency
      graph through okayAgent. This is an unnecessary packaging boundary.
      DESIGN TO SPECIFY BEFORE IMPLEMENTING:
      1. okay-durable / package okay.durable: generic handler, recovery
         policies, journal contract, entries, errors and trace seam;
         depends on okay + okay-codec, not agent, LLM, RAG or persist.
         Preserve supported platforms; Native is a separately verified
         target, not a promise implied by moving source files.
      2. okay-durable-persist: TopicJournal storage adapter over
         okay-durable + okay-persist. Keep its record envelope and bytes
         compatible. A separate adapter avoids bringing workflow/storage
         into users of the generic handler with their own journal.
      3. okay-agent: Tool Journalled instance and Tool-specific convenience
         APIs; keep forwarding facades/type aliases for existing
         okay.agent.Durable/OpTrace/TopicJournal usages during migration.
         No reverse dependency from the neutral modules to agent.
      4. Keep Journalled in okay-codec initially: moving it into a module
         depending on codec would create a cycle for codec-owned effect
         instances. Any later relocation needs a separate dependency audit.
      Existing okay-workflow (Wf/Proc/replay discipline) and okay-persist
      (Dialogue/Worker/timers/signals/snapshots) already support durable
      workflows without agent. They remain distinct from the per-operation
      handler; extraction is not a rewrite or merger of both journals.
      Audit generic users in Python/R, obs tests, FileVersions, Conversation
      and Scala2 wrappers. Preserve stored histories, span identity and
      handler semantics; use one implementation behind legacy facades.
      SEQUENCE: prefer fixing durable-withkey-first-attempt and
      durable-run-scoped-keys before extraction; otherwise explicitly
      relocate those open entries and tests so bugs are not hidden by
      package movement. Update durable-recovery-contract-tests/demo paths
      and every board/spec that the move makes stale when it lands.
      DONE: an agent-free consumer journals and replays a custom effect;
      build graph contains no neutral-to-agent or persist-to-adapter cycle;
      old Tool/Conversation/FileVersions APIs still compile and behave;
      TopicJournal reads pre-extraction records; relevant generic/adapter/
      legacy suites pass. Scope gates to affected modules; no full sweep
      or performance claims solely because the packaging changed.
