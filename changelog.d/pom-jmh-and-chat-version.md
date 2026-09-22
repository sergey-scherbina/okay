## pom-jmh-and-chat-version - JMH out of the published poms; the chat guide's publishLocal road works again, re-run end to end

Two defects the scala2-docs consumer check found. Fixing the second
turned up a third, which was what actually broke the guide.

- **JMH is no longer a compile dependency of published modules.**
  sbt-jmh adds `jmh-core`, `jmh-generator-bytecode` and
  `jmh-generator-reflection` with no configuration, so okay_3's pom
  (and those of okay-platform and okay-stream) listed them as compile
  dependencies of a "zero-dependency" core. `project/JmhOutOfThePom.scala`
  is an AutoPlugin triggered by `JmhPlugin` that moves the three to
  `test`; `Jmh` extends `Test`, so benchmarks still see them. Checked:
  every pom after `publishLocal` has JMH only with a scope, and
  `okayJVM/Jmh/compile` followed by `okayJVM/Jmh/run ... okay.FibBenchmark`
  compiled and measured.
- **okay-js and okay-acme join the root aggregate.** They were compile
  dependencies of published modules (okay-ui on all three platforms,
  and okay-script) but missing from `.aggregate(...)`, so
  `publishLocal` never published them. The chat guide's build stopped
  at `Error downloading dev.okay:okay-js_3:0.1.1`. They were found by
  scanning every published ivy.xml for non-test dev.okay dependencies
  that were not published; after the fix that scan is empty (129
  modules). okay-acme's network suites are Live-tagged.
- **docs/building-a-chat-app.md re-run end to end** against 0.1.1, as
  its contract requires. The app was rebuilt from the page's own code
  blocks, the tests passed (2 + 2; the page said 3, which was stale),
  `fastLinkJS` ran, the server answered all three `curl`s, and `app.js`
  is now 1 606 305 bytes. The version is 0.1.1 throughout, the SNAPSHOT
  caching note is reworded for a release version, and a Troubleshooting
  entry covers the okay-js resolution error for older checkouts.
- Three scaladoc warnings, seen only on `publishLocal` (the gate never
  runs `doc`), are fixed: a `$` inside a comment's code sample
  (Throws.scala, Reader.scala, Proc.scala) is escaped as `\$`.
- Backlog: `root-aggregate-unlisted-modules` (seven more modules
  outside the aggregate with no reason written down) and
  `dottydoc-matcherror-under-publishlocal` (one Dottydoc crash that did
  not reproduce).
