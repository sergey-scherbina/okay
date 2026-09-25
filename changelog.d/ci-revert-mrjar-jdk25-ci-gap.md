## ci-revert-mrjar-jdk25-ci-gap - reverted by ci-runner

Culprit `54f135661d7e6b32711b22a6477414038376e1ee` ("mrjar-jdk25-ci-gap: the Multi-Release variant is built by the build — versioned()/multiRelease() in build.sbt, jdk25/ is project okayPlatformJdk25 packaged into okay-platform's jar under META-INF/versions/25 always, okay-platform's tests fork and run against the jar; TestScopedBackend proves ScopedValue on 26 and ThreadLocal on 17 (measured); the hand-run script and the core's stale packaging deleted", lane `mrjar-jdk25-ci-gap`) failed the
whole-build gate over `5baac6882510ebd2f86dfbd029607086a0f869bd..4433955252dd1f2aab3d1ceba98ff945a6adc1ae`. Reverted so master stays
something the next lane can rebase onto; re-land with the fix. Runner
log: `/Users/sergiy/work/my/okay/.work/ci/log/20260925T100448Z-5baac6882510ebd2f86dfbd029607086a0f869bd..4433955252dd1f2aab3d1ceba98ff945a6adc1ae.log`.
