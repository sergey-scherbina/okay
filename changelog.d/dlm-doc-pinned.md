## dlm-doc-pinned - okay-dlm's "In sixty seconds" is a running test; TestDocSnippets green again

- dlm-module (21df61e) landed `docs/modules/okay-dlm.md` with its example
  block unpinned, and TestDocSnippets' ratchet ("no example line outside
  the recorded debt is unpinned") went red on master: ten lines.
- `okay.dlm.docs.TestDocExamplesDlm` holds the block verbatim and runs it
  over a two-intent file and the hashing encoder: the page's three claims
  hold (`ищю сантехника` fires `need` by Typo(1) with `what` filled;
  `спасибо большое` is `social` at the 0.5 bar; a fired route decides
  `Act`). The bare lines are the last expressions of three defs, so each
  is kept exactly and still asserted.
- Two lines changed to be pinnable: the encoder is a real one
  (`okay.rag.Vectors.hashing(256)`, where the page had `…`), and the
  artifacts go to `dir.resolve(...)` — a directory the caller names —
  rather than `Path.of("resources/...")` under the working directory.
- okayDeploy/test 183/183 (1 skipped), the new suite 1/1.
