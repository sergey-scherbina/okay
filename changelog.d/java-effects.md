## java-effects - okay programs, effects and handlers written in Java

- okay-java gains `Eff<A>` (a program, stored at an erased row as
  okay-scala2's `Eff` is), `Op<R>` (a Java effect's operation), and
  handlers in the core's four forms: `Handler.answer`/`Handler.into`,
  `StateHandler.of`, `Control.of` with a `resume` callable once, twice or
  never. Each is the core's own machinery, split by the `Class` the Java
  caller names. The core Reader/State/Throws/Async are reachable from Java;
  `Eff.from` and `toScala[F]` cross the seam both ways.
- Java cannot spell a row, so `run()` refuses an unhandled operation BY
  NAME (a mutant on the core's `Answers[Pure]` turns that into a
  ClassCastException, caught by the suite).
- Proven by Java sources (`okay-java/src/test/java/.../JavaEffects.java`)
  run by `TestJavaEffects`, 12 tests, including 1 000 000-deep Java
  recursion. specs/java-effects.md; docs/modules/okay-java.md.
- `TestDocSnippets` reads `.java` sources too, so the Java example on
  docs/modules/okay-java.md is pinned line by line to `JavaEffects.java`
  (it had no way to pin a Java line before).
