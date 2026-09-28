- [ ] merge-wait-docs — a user page for the API landed by
      ready-merge-chunk-forward and drive-poll-then-park: the three
      givens a merge and a blocking runner take (`Merge` — Ready or
      Shared; `Wait` — Register, Spin, Ladder, Cycle; `Pause` — the
      platform's rungs), which calls take them, what each costs as
      measured, when to pick which, and the literature. Today the only
      description is a stretch of docs/guide.md §6 inside the streams
      narrative; the operator wants a page a reader can be sent to
      (docs/merge-and-wait.md), its examples pinned in
      `TestDocExamplesMergeWait`, linked from docs/README.md and the
      guide. Docs-only lane: gate is `TestDocSnippets` + the new suite.
