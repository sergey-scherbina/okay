- [ ] affected-test-only-shared-suites — `scripts/gate.sh "affected
      master"` maps a TEST-ONLY change in a module to that module's own
      tests and not its dependents', which is right for ordinary tests
      and wrong for a shared SUITE other modules extend. Found
      2026-09-24 (channel-law-racing-offers): a law added to okay-stream's
      `ChannelLawsSuite` ran over okay-stream's seven channels and not
      over okay-clojure's `CoreAsyncChannel`, whose
      `TestCoreAsyncChannelLaws` extends the same suite — it had to be
      run by hand. The fix is in project/Affected.scala: a changed test
      source that another project's TEST configuration depends on (a
      `test->test` dependency) pulls that project's tests in too. What
      would settle it: the same diff through `affected master` naming
      okayClojure.
