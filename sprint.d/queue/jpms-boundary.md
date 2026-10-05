- [ ] jpms-boundary — JDK 9+ modules as the second witness of the effect
      boundary (operator ask 2026-10-05; the analysis and the decision are
      specs/okay-audit.md "JPMS"). Stage A in okay-audit: read
      `module-info.class` from inputs (`ModuleDescriptor.read`), mark each
      rule per module JVM-enforced vs scan-only (java.sql/javax.naming/
      java.net.http/jdk.unsupported/foreign are enforceable; all of
      `java.base` — sockets, files, time, Random, reflection — is not),
      detect split packages across inputs, read launcher flags from a
      jvm-options file, add `Audit.runtime()` (boot layer modules + requires
      + native-access + input arguments → an evidence-journal entry; okay-watch
      first). Stage B in okay-watch: `okaywatch.*` as a named module over okay
      as ONE automatic module (the assembly), jlink without java.sql /
      jdk.unsupported, `--illegal-native-access=deny`, no `--add-opens`; the
      scan report and the JVM must agree. Stage C (okay2): one package per
      module, module-info everywhere, handlers as `provides`. Gotcha: okay's
      modules share package `okay` — they cannot be separate modules on the
      module path, named or automatic; a shaded single jar is the only
      JPMS-legal shape today. Done-when (A): the dogfood report carries an
      "enforcement" column and names okay's split packages.
