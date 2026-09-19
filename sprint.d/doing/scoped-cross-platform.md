- [ ] scoped-cross-platform — okay.Scoped works on Scala.js and
      Scala Native too, not just JVM.

      WHY: operator asked. Turns out unnecessary to write separate
      per-platform implementations -- VERIFIED this session:
      okay-stream/src/main/scala/AdaptiveFifo.scala already uses
      `new ThreadLocal[A] { override def initialValue() = ... }` in
      the FULLY SHARED cross-platform source set, and okayStreamJS /
      okayStreamNative both compile it today (checked directly,
      `sbt okayStreamJS/Compile/compile` and .../okayStreamNative/...,
      both green). ThreadLocal is available uniformly; only its
      `withInitial` static factory is JVM-only (Scala Native's JDK
      subset lacks it -- already documented at AdaptiveFifo.scala:243).

      HOW: git mv src/main/scala-jvm/Scoped.scala src/main/scala/
      Scoped.scala (the shared cross-platform source set); replace
      `ThreadLocal.withInitial(() => default())` with the anon-
      subclass form AdaptiveFifo.scala already established
      (`new ThreadLocal[A] { override def initialValue(): A =
      default() }`). Remove the now-redundant scala-jvm copy. The
      JDK25/ScopedValue MRJar variant stays JVM-only by necessity
      (java.lang.ScopedValue is a JVM API, no JS/Native equivalent) --
      unaffected, still lives at jdk25/Scoped.scala, still only
      applies to okayJVM's packaging.

      Move src/test/scala-jvm/TestScoped.scala to src/test/scala-cross
      (the convention for tests that run on JVM+JS both, per okay's
      own .jvmSettings/.jsSettings Test/unmanagedSourceDirectories) --
      drop the JVM-only "backend" reflection test from the cross
      suite (reflection needs java.lang.reflect, JVM-only) or keep it
      in a small scala-jvm-only leftover file.

      DONE WHEN: okayJVM, okayJS, okayNative all compile Scoped;
      cross-platform tests pass on JVM and JS; okayScript unaffected
      (208 tests); gate green, cold.
