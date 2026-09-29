## pages-maven-repo - the release wave as a Maven repository on GitHub Pages

The operator chose GitHub Pages over Maven Central for now: one resolver
line for a user, no account, token or signing key on either side.

- `build.sbt`: with `OKAY_PAGES_REPO` set, `publishTo` is a Maven-layout
  directory there; otherwise Central, as before.
- `scripts/pages-publish.sh`: a `gh-pages` worktree at `.work/pages/`
  (the orphan branch made by plumbing on the first run, since
  `checkout --orphan` trips over the plugins submodule and
  `worktree add --orphan` needs git 2.42), `releaseWaveCheck` and
  `publish` into its `maven/` folder, an `index.html` listing artifacts
  and versions, `.nojekyll`, one commit. It pushes only with `--push`,
  never overwrites a published release, and refuses a SNAPSHOT without
  `--snapshot` (each publish is about 64 MB of jars the branch keeps).
- Verified locally, nothing pushed: 35 artifacts (the 12 modules on each
  platform they build for) with sources, javadoc, POMs and checksums. A
  separate sbt project that saw only this repository over HTTP and a
  Central mirror resolved `okay`, compiled the tutorial's State example
  and printed `(40,42)`.
- `docs/releasing.md`, "GitHub Pages", and the docs index say how.
