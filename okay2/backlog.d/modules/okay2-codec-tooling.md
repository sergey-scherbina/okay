- [ ] okay2-codec-tooling: the okay-codec files outside the dialects,
      found unported when okay2-codec-dialects closed (2026-10-04), each
      to be ported when an okay2 caller needs it: `JsonOptic` (optics
      over `Json`; okay2-optics exists), `Policy` (projection policies),
      `Journalled` (what a journal needs of an operation), `Stubs` and
      `StubFiles` (the other side's declarations, TypeScript and the
      like), `TsTypes` and `TsCheck` (TypeScript read into Schema and
      compared), `Wire` (the JVM wire format and compression choice), and
      `Staging` (reaching okay-staging's run-time generator through
      `Codecs.install`; okay-staging has no okay2 twin).
