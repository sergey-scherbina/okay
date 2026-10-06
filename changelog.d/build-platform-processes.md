## build-platform-processes — JVM default with explicit JS and Native builds

Source commit 24eea24e9: root compile/test aggregate JVM only;
jsBuild/nativeBuild are independent aggregate entry projects sharing the
module definitions. Affected/family retain all platform members explicitly,
with an executable resolved-membership guard (127 JVM, 58 JS, 37 Native).

scripts/build.sh defaults to JVM; explicit all gates use fresh sbt processes
for JVM, JS, Native in order, under one CI lock. Native tasks are serialized.
Short affected gates separate platform heaps; explicit set/task chains
preserve their session state. okay2 retains its own ordinary test. GitHub
and the local runner use the managed platform gates. README/docs explain
normal sbt entry points and managed commands.

Validation: affected inventory/selection fixtures, gate/watchdog fixtures
under bash and /bin/sh, real three-platform Diagnosed probes and docs guards
(20 results). Earlier docs scope 184 results. No introduced warnings.
Production ci-runner files are unchanged; its existing Darwin sess=0
selftest failure is filed separately in backlog.d/build. Full matrix is
the post-merge runner's check, not a repeated worktree sweep.
