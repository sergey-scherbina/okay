- [x] optics-outside-tools-adopt — DONE (2026-09-11). The entry was
      wrong that only tests were left: okay-demo's `RepoAgent` is an
      application and held two `ToolSpec` vals beside a `Map` under
      the same names, handed to `RepoMcp`'s server as two arguments.
      It and the five test tables are one `Toolbox` each now. The
      finding is a BEHAVIOUR one the drift argument had not predicted:
      the hand-written decode answered `bad args: ...` as prose while
      `Toolbox` answers `{"error": ...}`, so the same agent reported
      failure in two shapes depending on which module declared the
      tool. `TestRepoTools` pins it, in the default gate — the
      existing `TestRepoAgent` is `Live`-tagged and indexes the whole
      repository, so the tools' contract had no fast test.
