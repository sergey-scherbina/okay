- [ ] okay2-cross — Scala.js and Scala Native for okay2: nothing in
      stage 1 is JVM-specific (no reflection but `ClassTag.runtimeClass`
      and `Class.isInstance`, both available on both). The lane: make
      `okay2` a `crossProject(JVMPlatform, JSPlatform, NativePlatform)`
      with `CrossType.Pure` in ITS OWN build (okay2/build.sbt is a
      separate sbt build, not a root project), and run the same nine
      suites on each from `cd okay2`. (2026-09-24)
