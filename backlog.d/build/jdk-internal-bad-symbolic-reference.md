- [ ] jdk-internal-bad-symbolic-reference — a COLD `okayJVM/compile`
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
      gate. Not done: seen once, and once is not a signature.
