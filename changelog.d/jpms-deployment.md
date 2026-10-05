## Strict Headless JPMS Deployment

Stage B is implemented in okay-watch, commit
7973a0499c05016c8e1e345fb9379babccd28a12 (pushed). Its opt-in profile
separates okay.watch from one automatic okay.platform assembly; the
JDK image omits java.sql/jdk.unsupported, denies application/platform
native access and opens no JDK internals. Existing desktop/JDBC installs
are unchanged. tools/release-jpms.sh accepts an existing trusted assembly
and prebuilt audit classpath, so packaging and scanning need no sbt then.

Verified with the shipped desktop assembly on JDK 25: all seven built-in
plugins; 4,050 synthetic transfers, 3/3 injected subjects caught and zero
baseline alerts; SQL/Unsafe/reflection/native refusals; TLS/EC providers;
missing/invalid/existing-output refusals; all three JVM-option environment
overrides rejected. Scanner text/JSON have no split packages and record
the launcher policy. Its mixed app is inventoried as handlers, not claimed
as a pure business module; audit-ready Stage 4 remains in okay-watch's
BACKLOG. Stage C is okay2/backlog.d/build/jpms-module-layout.md.
