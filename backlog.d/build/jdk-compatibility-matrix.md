- [ ] jdk-compatibility-matrix — the project has no single JDK floor
      or ceiling, and pretending otherwise is what almost drove a
      wrong decision (2026-09-19): okay-http/Server.scala and
      okay-jetty/Jetty.scala already hard-require JDK 21+
      (Executors.newVirtualThreadPerTaskExecutor, Thread.
      startVirtualThread -- non-preview, not gated), while okay-spark
      needs a JDK where java.lang.SecurityManager still exists
      (deprecated since JDK17/JEP 411, REMOVED entirely in JDK24/JEP
      486 -- Spark still touches it via Hadoop's
      UserGroupInformation). Lowering the build's floor toward 17/11
      would break the first; letting CI/dev machines run JDK24+ would
      break the second. Neither is written down anywhere.

      HOW (not attempted here): a short doc (specs/ or a table in
      AGENTS.md) naming, per module or per feature, the JDK range it
      actually needs and WHY (cite the JEP/API), so the next "can we
      bump the JDK" conversation starts from facts instead of
      re-discovering okay-http's virtual threads and okay-spark's
      SecurityManager dependency from scratch. Doesn't require any
      code change by itself.

      Related: backlog.d/okay-script/mrjar-jdk25-ci-gap.md (the
      per-feature answer -- Multi-Release JARs -- for the ONE place
      this got applied, okay.Scoped; this item is the project-wide
      bookkeeping that answer doesn't replace).
