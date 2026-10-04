- [ ] okay2-codec-tooling: the okay-codec files outside the dialects,
      found unported when okay2-codec-dialects closed (2026-10-04).
      JsonOptic, Policy and Journalled landed (okay2-codec-optics, stage
      57), then Stubs, StubFiles, TsTypes and TsCheck (okay2-codec-stubs,
      stage 58), then Wire (okay2-codec-wire, stage 59).
      BLOCKED, and the one thing left: `Staging`. In okay-codec it is a
      30-line JVM seam. `autoInstall()` finds `okay.staging.RuntimeStaged`
      by name and calls `install()`, and that module compiles each
      Schema's codec at RUN time through Scala 3's `scala.quoted.staging`
      (`Compiler`, `run`, `scala3-staging`). Scala 2 has no quotes and no
      staging compiler. The twin's door, `Codecs.install(Provider)`, is
      already there (stage 54), but nothing exists for the seam to find.
      Porting the seam alone would ship a door that always answers
      `Absent`. What would unblock it is a design for an okay2-staging
      module, not a port: say, a run-time generator over scala-reflect's
      `ToolBox` that emits what `StagedMacro` emits at compile time,
      measured against the interpreter. Once such a module exists,
      `Staging` is the same 30 lines with `okay2.staging` as the class
      name.
