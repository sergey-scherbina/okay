- [ ] cold-tasty-bad-symbolic-reference — RENAMED 2026-09-23 (backlog-audit-0923)
      from `jdk-internal-bad-symbolic-reference`, as its own third
      occurrence asked: the symbol is not the cause, a cold TASTy read
      is. Was: a COLD `okayJVM/compile`
      can fail with no source position and one error:

          [error] Bad symbolic reference. A signature
          [error] refers to StackableScope/T in package jdk.internal.vm
          [error] which is not available.

      `StackableScope` is loom's own internal class behind
      `StructuredTaskScope`; nothing of ours names `jdk.internal.vm`,
      and the grep says so. MEASURED 2026-09-11
      (optics-outside-remaining, a docs-only lane, so the tree could
      not be the cause): the gate died at 1205 of ~4081 test results,
      and an UNCHANGED `okayJVM/compile` immediately after recompiled
      the same 69 sources clean in 9 s. The same shape as
      `dotty-classfile-crash-transient` with different text: fails
      cold, passes unchanged.
      Three things before anyone teaches `scripts/gate.sh` to re-run
      on it, which is the obvious next step and the dangerous one:
      the signature must require ZERO `==> X` AND that the only
      `[error]` lines are this one plus `one error found`; the re-run
      must be scoped to the failing project, as the Native branch
      already is; and the rate belongs in a ledger here, because a
      re-run that hides a real compile failure is worse than a red
      gate.
      SEEN TWICE NOW, and the second reading WIDENS the cause rather
      than confirming it. 2026-09-18 (proc-doors), the same shape with
      a different symbol:

          [error] Bad symbolic reference. A signature
          [error] refers to RuntimeException/T in package java.lang
          [error] which is not available.

      `java.lang.RuntimeException` is not loom, not `jdk.internal.vm`,
      and not anything a classpath can plausibly be missing — so the
      first entry's reading ("loom's own internal class") was too
      narrow, and what the two occurrences share is only COLD
      SIGNATURE READING. A cold `okayJVM/Test/compile` minutes earlier
      on the same tree was clean in 20 s, and the unchanged gate
      re-run right after was GREEN with 5450 test results. Still not
      done.
      THREE TIMES IN ONE DAY, AND THE THIRD NAMES THE STDLIB ITSELF.
      2026-09-18 (dataflow-windowed-seek), in `okayFrameJS`:

          [error] Bad symbolic reference. A signature in
          .../scala-library-3.9.0.jar(scala/LowPriorityImplicits.tasty)
          [error] refers to LowPriorityImplicits2/T in package scala
          which is not available.

      Three occurrences, three DIFFERENT symbols — `jdk.internal.vm`,
      `java.lang`, and now `scala` read out of the standard library's
      own tasty file, which is on every classpath in the build by
      definition. Nothing about the SYMBOL is the cause; what the
      three share is a cold read of a TASTy signature, and the third
      makes "a missing classpath entry" untenable as a reading.
      The entry's own name is now misleading (`jdk-internal-` was the
      first symptom, not the cause) and should be renamed when
      somebody picks it up. THE BAR FOR A RE-RUN GUARD IS UNCHANGED:
      three is a pattern, not a signature, and a guard that hides a
      real compile failure is worse than a red gate — the three
      conditions in the paragraph above still have to be met first.
