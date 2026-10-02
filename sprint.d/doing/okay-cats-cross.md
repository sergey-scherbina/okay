- [ ] okay-cats-cross — okay-cats is JVM-only while cats, cats-effect
      and okay's core are all cross-built (JVM, Scala.js, Native). The
      class bridges (CatsClasses, the program `Monad`, conversions) need
      nothing JVM-specific; `toIO` (`IO.blocking` + `runWith`) and the
      scheduler park a thread and stay JVM/Native (the CanBlock evidence
      decides). Make the module a crossProject, the parking doors in a
      jvm-native source set. Cats-depth audit, 2026-10-02.
