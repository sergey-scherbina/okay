- [~] nadia-platform — SPECS ONLY, one lane, for what `../nadia`'s okay
      implementation needs from the platform (nadia `docs/specs/app.md`,
      nadia `SPEC.md` §6/§7/§10, nadia BACKLOG NAD-15..18). Four files:
      `specs/agent-fleet.md` (okay-agent: agents as supervised actors with
      budgets, `delegate` as a tool, `Status` as a value, sessions on a
      topic), `specs/llm-models.md` (okay-llm: `Models.Catalog` in
      OpenAI's shape, `Residency`/`Store` in Ollama's verbs, each a
      capability a provider may LACK; adapters openAi/anthropic/ollama/
      rozum; model-id equality), `specs/identity-roster.md` (okay-security:
      a channel address bound to a Principal, a roster with roles on a
      topic — lifted from okay-chat's `okaychat.Identity`),
      `specs/telegram-live.md` (okay-telegram: throttled in-place edits,
      `setCommands` from a screen table). Each spec's implementation is
      its OWN later lane, named in the spec. Gate: docs lane —
      `affected master staged` (okayDeploy doc guards). Done-when: four
      specs on master, changelog.d/nadia-platform.md, this item deleted.
