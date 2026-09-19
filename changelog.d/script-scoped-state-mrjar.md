## script-scoped-state-mrjar — okay.script.api.Scoped ships as a Multi-Release JAR
Landed: 2026-09-19

On JDK 25+, `Scoped[A]` loads a `java.lang.ScopedValue` backend
(JEP 506) instead of `ThreadLocal`, automatically, via the JVM's own
Multi-Release JAR mechanism (JEP 238) — one artifact, no runtime
branch anywhere. `okay-script/jdk25/Scoped.scala` is the
`ScopedValue` variant; `scripts/build-mrjar-jdk25.sh` compiles it
using an actual JDK 25+ JVM (required — `java.lang.ScopedValue`
doesn't exist on JDK 21's own runtime classes, and no `-release` flag
grants an older compiler access to a newer platform's API).
`build.sbt`'s `okayScript` settings package the script's output under
`META-INF/versions/25/` with `Multi-Release: true` IF present; a
checkout that never runs the script packages the exact jar it always
has.

Verified directly: the same packaged jar, run under JDK 21 and JDK 25
`java` binaries, loads `backend=ThreadLocal` and
`backend=ScopedValue` respectively. Confirmed the base-only path is
unaffected (byte-identical jar shape) when the script hasn't run.

Known gap, tracked in backlog.d/okay-script/mrjar-jdk25-ci-gap.md:
nothing in CI re-checks the JDK25 path automatically.

See specs/script-scoped-state-mrjar.md, specs/script-scoped-state.md.
