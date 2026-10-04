- [ ] okay2-codec-tooling: the okay-codec files outside the dialects,
      found unported when okay2-codec-dialects closed (2026-10-04).
      JsonOptic, Policy and Journalled landed (okay2-codec-optics, spec
      stage 57). Left: `Stubs` and `StubFiles` (the other side's
      declarations, TypeScript and the like), `TsTypes` and `TsCheck`
      (TypeScript read into Schema and compared), `Wire` (the JVM wire
      format and compression choice), and `Staging` (reaching
      okay-staging's run-time generator through `Codecs.install`;
      okay-staging has no okay2 twin).
