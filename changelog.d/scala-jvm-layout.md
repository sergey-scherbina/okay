## scala-jvm-layout - okay-bayes and okay-refine in the repository's JVM-only layout

Operator ask ("Почему .jvm? Что это значит?" — then "Да"). okay-bayes and
okay-refine kept their JVM-only sources where sbt-crossproject puts them
by default for CrossType.Pure, the HIDDEN `<module>/.jvm/src/…`; the rest
of the repository (okay-arrow, okay-actor, okay-blob, okay-cache, …) uses
`src/main/scala-jvm` and `src/test/scala-jvm`, added by `jvmSettings`.
Moved with `git mv` (history kept): okay-bayes's PyMC.scala and nine JVM
suites, okay-refine's Documents.scala and three JVM suites. Being hidden
had a cost: TestDocSnippets skips dot-directories, so both modules needed a
hand-written `.jvm/src/test` root there — removed. `.jvm/` itself stays as
the JVM build's `target/`, ignored. Checked: a clean build of both modules
on JVM, Scala.js and Native ran every suite it ran before (525 tests, no
warnings), and okay-spark's tests, which depend on okay-refine's JVM
classes, compile. okay-js and okay-rust still use `.jvm/src`.
