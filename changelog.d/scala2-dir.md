## scala2-dir - the Scala 2 facade modules move under scala2/

Operator: "я предлагаю перенести все scala2 модули в подкаталог scala2"
(2026-09-23).

- `git mv` of all seventeen `okay-scala2*` directories (the 2.13 probe
  with them, at `scala2/okay-scala2/probe`) into `scala2/`; build.sbt's
  eighteen `file(...)` paths follow. Project and artifact names are
  unchanged, so nothing changes for a user's build.
- Links: every module README's relative links gained a level; the probe
  paths quoted in docs/scala2.md and docs/modules/okay-scala2.md now say
  `scala2/okay-scala2/probe/...`. A link check over 871 markdown links
  found the same nine hits before and after the move, none under scala2
  (four are real and older: filed as `readme-relative-links`).
- TestDocsIndex found module roots as `file("okay-x")` and treats a path
  with a slash as a sub-project, so the move would have silently taken
  every facade module out of "every module the build declares has a
  page". It now knows `scala2/` as a grouping directory; a control run
  with one facade page removed failed, naming that module.
- `scala2/README.md` lists the modules and what each wraps.
- Gated: the full matrix, GREEN (6905 tests, cold, no warnings). Master
  then gained e472a33b (scalus-executor-fetch: okay-scalus and its docs
  pages), which no scala2 module depends on; after the rebase the doc
  checks this lane changed were re-run over its pages, `okayDeploy/test`:
  GREEN.
