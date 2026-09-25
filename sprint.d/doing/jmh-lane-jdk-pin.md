- [ ] jmh-lane-jdk-pin — `scripts/jmh-lane.sh` (and `ab-defaults.sh`,
      whose precedent it follows) runs a BARE `sbt`, which takes the
      PATH's java — sdkman's `current`, JDK 17 on this box — while
      `scripts/gate.sh` pins `JAVA_HOME` to what `.sdkmanrc` declares
      (25, jdk-local-default). Found 2026-09-25 by cont-stack's A/B:
      every `mine` lane failed at once with `22 is not a valid choice
      for -java-output-version` — dotc on 17 cannot emit 22 — because
      okayJVM's Jmh config extends Test, which depends on `okayJdk22`
      (`versioned`); the same happens to any run that reaches
      `okayPlatformJdk25`. Fix: the same five lines gate.sh has
      (read `java=` from .sdkmanrc, export JAVA_HOME and PATH), in
      `quiet.sh`'s neighbourhood so both scripts share it — one
      vocabulary (policy P-6), not a third copy. Until then an A/B
      exports JAVA_HOME itself (cont-stack's `ab-cont-stack.sh` did).
