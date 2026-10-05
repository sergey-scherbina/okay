- [ ] release-wave-task-lint — pre-existing sbt task-lint warning on a
      cold root build (2026-10-05), project/ReleaseWave.scala:56:
      projectDependencies.value is looked up inside an if expression.
      Preserve release-wave semantics while making the static task
      dependency explicit outside the branch (and inspect its paired
      libraryDependencies lookup). A clean meta-build compile must emit
      no warning; run the release-wave focused tests, not the family.
      Found by jpms-module-layout's documentation gate; unrelated to the
      okay2 module layout, deferred behind the requested watch domain audit.
