- [ ] build-platform-processes — split full/affected gates into JVM, JS,
      Native stages in separate sbt processes, serial Native tasks.
      Add scripts/build.sh platform entry point; preserve shared CI lock,
      accumulated logs, stop-on-first-failure and explicit command chains.
      Repro context: operator sbt test OOM in Scala Native Lower; rejected
      executor callbacks follow heap exhaustion. Do not rerun the full
      matrix to reproduce memory exhaustion on the shared machine.
      Validate gate fixtures and targeted real platform probes; CI remains
      the owner of the full build after landing.
