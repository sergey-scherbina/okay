# Platform build processes

## Overview

A root sbt test OOM report reached Scala Native Lower, followed by shutdown
executor errors. Full gates must separate platforms and release their heaps
between stages; Native tasks must not fan out across modules concurrently.
This is a resource bound, not proof of the precise cause of that report.

## Interface

scripts/build.sh jvm|js|native|all [task], task defaults to test.
Uses scripts/gate.sh from the repository root. compile and Test/compile
are supported as single task names. Whole gates retain the CI lock.
Bare root sbt test is still sbt's raw aggregated task; use this entry point
for separated builds. No source modules or dependency graph change.

## Behavior

- [ ] gate test and family all [task] run JVM, JS, Native in this order,
  one fresh sbt process per platform, holding the lock through all stages.
- [ ] Short affected ref [staged] runs three fresh processes, retaining
  own/dependent stage order within each platform and no whole-build lock.
- [ ] family native [task] runs with Global concurrentRestrictions limited
  to one task; the Native stage of automatic gates does likewise.
- [ ] First nonzero process exit stops subsequent platforms; all earlier
  output is retained in the main log and verdict counts their results.
- [ ] Explicit command chains remain one sbt session, including set commands;
  their implicit affected expansion separates JS/Native commands in order.
- [ ] build entry point dispatches all platforms/tasks and rejects malformed
  arguments without starting sbt. CI family all uses the separated path.

## Decisions

Keep raw sbt tasks intact; changing the meaning of test in every selected
sbt project would disrupt scoped use. The script is the managed full-build
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
