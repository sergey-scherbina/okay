## scala2-agent - okay-agent from Scala 2.13: Chat, Model, Tools, Policy

The fourth of the five areas (HTTP, SQL, codecs, agents, UI).

- Probed from scalac 2.13.18: okay-agent's data (`Turn`, `Reply`,
  `ToolCall`, `ToolSpec`, `Toolbox`, `Handlers`, `Provider`) is
  readable, until something touches a `Json` FIELD. The agent program
  `String ! Agent` and the union of handlers it needs are not usable.
- The new module okay-scala2-agent adds:
  - `Chat`: okay-agent's loop, with the handlers assembled once and
    the context kept between `say` calls, and an `approve` gate on
    every tool call;
  - `Model`: scripted, or `anthropic`/`openAi` through okay-agent's
    own `Provider`;
  - `Tools`: over `Toolbox`, with arguments decoded by the declaring
    `Schema`;
  - `Policy`: `all`, or `window` with a reported elision;
  - `Call`: a tool call's arguments as JSON text.
- `TestAgentFromScala2` (5 tests): a persistent conversation, a tool
  call decoded into a case class, a denied call, the declarations, and
  the window policy. The probe has 65 tests. The Live test against a
  real model is written but SKIPPED here, because no API key was set.
  So a real provider has not been exercised through the facade.
- Docs: section 8d of docs/scala2.md (copied from the probe), a module
  page, API reference, typepedia, and spec stage 9.
