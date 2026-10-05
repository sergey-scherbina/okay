- [ ] okay-audit — the effect boundary checked at every build: a bytecode
      scanner (constant-pool references, JVMS §4) over each module's classes
      and classpath jars; `Business` modules may reference none of the
      default rule set (network, files, console, sql, processes, time,
      randomness, threads, reflection, invoke, class loading, foreign, native
      methods — carve-out only for the three compiler bootstraps); `Handlers`
      and `Runtime` (okay core) modules are LISTED by provider, never failed.
      The listing doubles as the DORA Art. 8 dependency inventory, which is
      why this is the first "concept" row of the regulated-buyer story to
      build (operator, 2026-10-05; `~/work/my/jobs/biz/regulatory-mapping.md`
      §2a). Spec: specs/okay-audit.md — stage 1 is the module, the sbt
      `auditLayer`/`audit` task and the dogfood run on okay's own build.
      Reader to lift: `src/test/scala/TestInlineBudget.scala`'s JVMS §4
      reader (not `java.lang.classfile`, dotty 3.9 cannot load it). Gotcha:
      every lambda is an invokedynamic whose bootstrap is
      `java.lang.invoke.LambdaMetafactory` — forbid `java.lang.invoke.`
      without that carve-out and every business class fails. Done-when: the
      dogfood audit passes with the inventory written to
      `target/audit/report.txt`, and the fixture suite covers each Behavior
      item.
