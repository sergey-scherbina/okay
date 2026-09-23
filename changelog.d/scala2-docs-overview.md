## scala2-docs-overview - the overview pages catch up with stage 15

Operator: "Документацию обновил?" (2026-09-23). Each stage-15 lane had
written its own section, module page, API reference, spec entry and
changelog, but the pages that SUMMARISE Scala 2 coverage still described
the morning's state.

- docs/scala2.md: the introduction names the whole library; the contents
  list gains 8k–8q (it stopped at 8j); section 1 says how to add the other
  sixteen modules, with a table of module, section and what it wraps;
  section 11 ("What is not here") said durable agents and "the remaining
  modules" were not wrapped, and now lists only what is not, with why
  (the interop modules, Scala 3 metaprogramming, okay-ui's `Scope`).
- README.md, docs/README.md, ROADMAP.md ("stages 1–5"), docs/guide.md's
  Scala 2 paragraph: the same update, in each one's own words.
- The new build snippet was checked, not assumed: a root `publishLocal`
  (GREEN, all seventeen facade modules published), then an outside 2.13
  project built from section 1's two blocks verbatim compiled and ran a
  program using a composed `Lens`, a `Response` and `State`.
- Filed `affected-docs-run-no-doc-tests`: `affected master` ran 0 tests for this
  docs-only lane, so okay-deploy's doc tests were run by hand (156, green).
