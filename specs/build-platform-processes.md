# Platform build processes

## Overview

A root sbt test OOM report reached Scala Native Lower, followed by shutdown
executor errors. Full gates must separate platforms and release their heaps
between stages; Native tasks must not fan out across modules concurrently.
This is a resource bound, not proof of the precise cause of that report.

## Interface

scripts/build.sh [jvm|js|native|all] [task], defaults to JVM and test.
Uses scripts/gate.sh from the repository root. compile and Test/compile
are supported as single task names. Whole gates retain the CI lock.
Operator steering: the root aggregate contains ONLY JVM projects, so bare
sbt compile/test defaults to JVM. jsBuild and nativeBuild are separate
aggregate entry projects, with no dependencies between the three roots.
They share the module definitions and sources. Select with project jsBuild
or project nativeBuild. Module dependency graphs remain unchanged.
The family/affected graph explicitly includes all three roots, so splitting
the default aggregate must not silently drop JS/Native from CI. The shared gate is also used
from okay2, which has no family command: its bare test must remain untouched.
GitHub's full/affected branches use the managed gate too.

## Behavior

- [x] Root aggregate contains JVM only; jsBuild contains JS only and
  nativeBuild Native only, with no missing/duplicated previous members.
- [x] gate test defaults to JVM; family all [task] runs JVM, JS, Native in this order,
  one fresh sbt process per platform, holding the lock through all stages.
- [x] Short affected ref [staged] runs three fresh processes, retaining
  own/dependent stage order within each platform and no whole-build lock.
- [x] family native [task] runs with Global concurrentRestrictions limited
  to one task; the Native stage of automatic gates does likewise.
- [x] First nonzero process exit stops subsequent platforms; all earlier
  output is retained in the main log and verdict counts their results.
- [x] Explicit command chains remain one sbt session, including set commands;
  their implicit affected expansion separates JS/Native commands in order.
- [x] okay2/other builds without project/Affected.scala retain bare test.
- [x] build entry point dispatches all platforms/tasks and rejects malformed
  arguments without starting sbt. CI family all uses the separated path.

## Decisions

Keep scoped sbt tasks intact; split only the aggregate roots. The script is the managed full-build
entry point and already carries watchdog, RAM guard and CI locking.
Fresh processes release compiler analyses/classloaders between platforms.
Native's task limit controls inter-module fanout, not codegen's internal
parallelism. Do not increase the heap or promise OOM elimination without
measuring the offending build. Explicit set/task chains retain session state.

## Validation

Fake sbt records process PIDs, arguments and ordering, fails chosen stages,
and proves accumulated output and CI locking. Existing gate/watchdog and
runner fixtures continue to pass. One small real Native suite validates the
limit setting; docs guards validate the entry-point documentation. Full
matrix belongs to the post-landing CI runner, not this lane.

## Results

Resolved aggregate check passes after rebasing new semantic modules:
127 JVM, 58 JS, 37 Native, 222 in the complete gate, disjoint and without
missing core platforms. Affected selection fixtures pass for all platforms,
including staged dependencies, test-only sources and docs-only changes.
No introduced compiler or setting-lint warnings.

Gate fixtures pass under bash and /bin/sh: separate process PIDs, ordering,
Native task bound, fail-fast, combined logs, shared lock, explicit chain
state, default JVM selection and unchanged okay2 bare test. The busy-host
fixture resets FAKE_SPIN explicitly to prevent POSIX function-assignment
leakage from its intentionally huge stall control.

Real scoped JVM/JS/Native Diagnosed suite probes and docs link/index/snippet
checks pass: 20 results in the final probe chain; earlier documentation
scope had 184 results. The full matrix is left to the post-merge runner.
CI runner fixtures have one pre-existing Darwin session-observation failure
(case 9c, ps sess=0); other assertions pass. This unchanged production
runner issue is recorded in backlog.d/build/ci-selftest-darwin-session.md.
