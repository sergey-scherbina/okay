## affected-test-only-shared-suites - a changed shared suite runs the tests that extend it

- `affected master` mapped a TEST-only change to that project's own
  tests and no dependent's — right for an ordinary test, wrong for a
  shared SUITE: a law added to okay-stream's `ChannelLawsSuite` ran over
  okay-stream's seven channels and not over okay-clojure's
  `CoreAsyncChannel`, whose `TestCoreAsyncChannelLaws` extends it
  through `okayStream.jvm % "compile->compile;test->test"` — run by hand
  (channel-law-racing-offers, 2026-09-24).
- `project/Affected.scala` keeps a second dependency map over the
  `test->test` edges alone; a project whose test sources changed reaches
  the projects on those edges, transitively, as the second stage, and
  the ci-affected-tests-only rule stands for every other edge.
  `affected-selftest.sh`: okay-stream's `TestChannelLaws.scala` is
  okayStreamJVM then 2 dependents, okayClojure and okayLexJVM (okay-lex
  borrows the arrow laws the same way); the leaf test-only case is
  still "3 then 0".
