## scoped-to-core — okay.Scoped moved out of okay-script into core
Landed: 2026-09-19

`Scoped[A]` (script-scoped-state, script-scoped-state-mrjar) was
never okay-script-specific — moved to `okay.Scoped`,
`src/main/scala-jvm/Scoped.scala` (repo root, beside `Platform.scala`
— the established JVM-only-source-in-the-crossproject convention).
The Multi-Release JAR variant moved with it: `jdk25/Scoped.scala`
(repo root) and its `build.sbt` wiring, now on the `okay` crossProject's
`.jvmSettings` instead of `okayScript`'s.

`okay-script`'s `Api`/`Application`/`Content.scala` each gained one
import (`okay.Scoped`); no call site changed.

Added `src/test/scala-jvm/TestScoped.scala` — `Scoped` had no direct
test of its own before this, only indirect coverage through
okay-script's Web/Principal scenarios.

Re-verified the Multi-Release swap after the move, directly: same
jar (now `okay.jvm`'s), same probe, `backend=ThreadLocal` under JDK21,
`backend=ScopedValue` under JDK25.

See specs/script-scoped-state.md, specs/script-scoped-state-mrjar.md.
