- [ ] scoped-to-core — move okay.script.api.Scoped (script-scoped-state,
      script-scoped-state-mrjar) to the core module: package okay,
      src/main/scala-jvm/Scoped.scala, next to Platform.scala (the
      established JVM-only-source-in-the-cross-project convention).
      Nothing okay-script-specific about it -- any JVM module wants a
      no-public-set ThreadLocal/ScopedValue-backed scoped binding.

      WHY: operator asked where it belongs after seeing it used only
      by okay-script; core is the right answer since ThreadLocal has
      no Scala.js/Native equivalent (hence scala-jvm, not the shared
      cross sources) and virtually every JVM module already depends
      on core, so no new dependency edge is needed anywhere.

      HOW:
        - git mv okay-script/src/main/scala/okay/script/api/Scoped.scala
          src/main/scala-jvm/Scoped.scala; change `package
          okay.script.api` to `package okay`.
        - git mv okay-script/jdk25/Scoped.scala jdk25/Scoped.scala
          (repo root, sibling to okay-script/, .jvm/); same package
          change.
        - Api.scala, Application.scala, Content.scala (okay-script):
          add `import okay.Scoped`, drop nothing else -- current/where
          call sites are unqualified either way.
        - build.sbt: move the Compile/packageBin/mappings +
          packageOptions MRJar wiring from okayScript's settings to
          `okay`'s .jvmSettings, pointing at
          (ThisBuild / baseDirectory).value / "jdk25" / "target" /
          "classes" (okayScript's copy of these settings is deleted,
          not duplicated).
        - scripts/build-mrjar-jdk25.sh: source/output paths move to
          the repo root (jdk25/Scoped.scala -> jdk25/target/classes).

      VERIFICATION: same probe as script-scoped-state-mrjar, against
      okay_3's own jar this time (packageBin), under JDK21 and JDK25 --
      backend=ThreadLocal / backend=ScopedValue. okayScript's 208
      tests unaffected (proves the relocation didn't change behavior,
      only location). scripts/gate.sh "affected master" green, cold.
