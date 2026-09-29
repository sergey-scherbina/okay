## llm-models - the model catalog, residency and store, as capabilities a provider may lack

- `Models.Catalog` (`list`, `info`), `Residency` (`running`, `load`,
  `unload`), `Store` (`local`, `pull` as a `Progress` stream, `remove`):
  the catalog in OpenAI's `/v1/models` shape, the lifecycle in Ollama's
  verbs; an adapter's TYPE says what it can do, so `.load` on a hosted
  provider does not compile (asserted).
- Adapters: `openAi`, `anthropic` (limits and capabilities carried, pages
  followed) as `Catalog`; `ollama` as all three; `rozum` as
  `Catalog & Residency` — the resident row called by its real spec
  (`display_name` behind the Claude-shaped alias) and marked,
  `/control/switch` and `/control/unload`, a refusal in the gateway's words.
- `ModelId`: `org:repo`, `org/repo`, `hf:org/repo` are one model (equality
  by the canonical key), and the provider's spelling is kept for the wire
  (Ollama's `name:tag`).
- `Fetch` (`get`, `delete`) beside `Transport`; `Transports.http` and
  `Transports.fetch` now return `Transport & Fetch`. `Iso.epochMs` for
  Ollama's `expires_at`, cross-platform. Spec: specs/llm-models.md, all
  items checked, four decisions recorded; `TestModels` (9, cross).
  Consumer: `../nadia` `app/` Models screen.
