- [ ] jdk-adaptive-scheduler — the default `given Scheduler` and
      `given Timer` (src/main/scala-jvm/Platform.scala) pick Loom
      unconditionally; make them adapt to whether the running JVM
      actually has virtual threads, with no property required.

      WHY: operator asked, after the Scoped MRJar work raised "can we
      support JDK17/11 too". Original plan (this sprint item's own
      earlier draft) was an MRJar split of `Schedulers` -- WRONG,
      refuted by direct experiment before writing any of it: compiled
      a class with dotc (JDK21 host) containing one method calling
      `Thread.startVirtualThread` and one plain method; loaded it on
      JDK 17.0.19 and called ONLY the plain method -- worked, JVM
      constant-pool resolution is lazy per-method, not per-class. The
      classfile itself is already JDK17-loadable (dotc's default
      target is class file major version 61 regardless of host JDK,
      confirmed for both Scoped and this probe). So `Schedulers.loom`
      can keep existing, unchanged, in the SAME file everything else
      is in -- nothing needs splitting, no MRJar packaging, no
      duplicated scheduler internals to keep in sync. The ONLY real
      problem is the two places that call the JDK21-only API
      UNCONDITIONALLY on every run: `given Timer`'s `after` (fires
      every delay on a virtual thread) and `given Scheduler`'s
      fallback case (`Schedulers.loom`, unconditionally).

      HOW: `Schedulers.hasVirtualThreads: Boolean =
      Runtime.version().feature() >= 21` (checked once, `Runtime.
      version()` is JDK9+, safe everywhere). `Schedulers.auto:
      Scheduler` -- `if hasVirtualThreads then loom else own.build` --
      a NAMED, PUBLIC, callable-from-code adaptive pick, not buried in
      the anonymous `given` block (operator asked for "in code too").
      `given Timer`'s `after`: virtual thread if available, else a
      plain daemon `Thread` (matching `Schedulers.threads`'s own
      shape). `given Scheduler`: `-Dokay.scheduler` still overrides
      (own/adaptive/drive/threads always honored, they need nothing
      JDK21-specific); an explicit "loom" request degrades to `auto`
      if virtual threads are not actually there rather than crashing
      the first fiber fork; unset/unrecognized -> `Schedulers.auto`.

      VERIFICATION: this box's own JDK21 run (unchanged default
      behavior -- `hasVirtualThreads` must read true, `auto` must
      still pick `loom`, every existing test using `Schedulers.loom`
      directly must be unaffected since that val itself is untouched).
      Cross-JDK proof the same way as script-scoped-state-mrjar and
      spark-jdk25-guard-fix: package okayJVM, load `Schedulers`/
      `given Timer` under ~/.sdkman/candidates/java/17.0.19-tem
      directly (not through sbt's own always-JDK21+ test loop) and
      confirm `auto` picks `own` and a `Timer.after` callback actually
      fires without NoSuchMethodError.

      DONE WHEN: JDK21 behavior unchanged (measured); the JDK17 probe
      passes for real; gate green, cold.
