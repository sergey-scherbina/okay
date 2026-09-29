## nadia-platform - four specs for what nadia's okay implementation needs from the platform

Specs only, no code; each names its own implementation lane. Written from
`../nadia` `docs/specs/app.md` (nadia `SPEC.md` §6/§7/§10), by the
operator's rule that general-purpose code is okay's and the leaf keeps
its business logic.

- `specs/agent-fleet.md` — okay-agent: agents as supervised actors with
  budgets, `Control` messages (tell/pause/resume/stop/kill), `Status` as a
  value, the record on a topic, `delegate` as a parent's tool whose child's
  steps debit the parent.
- `specs/llm-models.md` — okay-llm: `Models.Catalog` in OpenAI's shape,
  `Residency` and `Store` in Ollama's verbs, each a capability a provider
  either has or does not (an intersection type, not a flag); adapters
  openAi, anthropic, ollama, rozum; `ModelId` equality across spellings.
- `specs/identity-roster.md` — okay-security: a channel address bound to
  a `Principal`, roles as grants on a topic, `Roster.role` as a `Policy`;
  lifted from okay-chat's `Identity` minus its product.
- `specs/telegram-live.md` — okay-telegram: edits to one message
  coalesced (last write wins, first at once), `Command` table →
  `setMyCommands` and dispatch.
- Filed the four implementation lanes in backlog.d.
