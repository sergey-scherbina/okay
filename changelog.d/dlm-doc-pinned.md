## dlm-doc-pinned - the dlm pages' examples run, the new modules are indexed; okay-deploy's doc tests green on master's tree

Three lanes landed on master the same morning without okay-deploy's doc
suites (dlm-module, dlm-backends, app-host, okay-telegram), and four doc
laws went red on the tree: TestDocSnippets' ratchet (unpinned example
lines on two pages) and TestDocsIndex twice (a page missing from the
index, a module with no page). On the ci-runner's whole build that is a
red that reproduces, which bisects and reverts the lanes.

- `docs/modules/okay-dlm.md`, "In sixty seconds": pinned and RUN by
  `okay.dlm.docs.TestDocExamplesDlm` over a two-intent file. The block had
  gone stale under dlm-backends — `Dlm.of` takes the encoder from a given
  and answers an `Either` — so it now compiles its table by the encoder in
  scope and takes the model out of the `Either`; its artifacts go to a
  directory the caller names, not `resources/` under the working
  directory. The page's three claims hold: `ищю сантехника` fires `need`
  by Typo(1) with `what` filled, `спасибо большое` is `social` at the 0.5
  bar, a fired route decides `Act`. "Backends"' first line is run too,
  with its last claim: a table compiled by another encoder is refused by
  name.
- `docs/modules/okay-dlm-remote.md`'s block (Laya with ours behind it, a
  head over it) is RUN by `okay.dlm.remote.docs.TestDocExamplesDlmRemote`
  — building sends nothing — and okay-dlm.md's composition root (a model
  on disk, a paid judge's key from the environment) is COMPILED there,
  not run.
- `docs/modules/okay-desktop.md` written from specs/app-host.md and the
  module's own scaladoc; okay-desktop and okay-telegram rows added to the
  docs/README.md index.
- okayDeploy/test 183/183 (1 skipped); the two pin suites 2/2 each.
