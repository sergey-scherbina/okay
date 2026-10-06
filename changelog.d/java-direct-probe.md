## java-direct-probe - checked exceptions and JVM continuations for Java effects, measured

- A research probe with no API. `TestJavaThrowsRow` compiles nine snippets
  with the JDK's compiler. `throws` is a checked effect row: a missing
  handler does not compile and a handler infers the ONE effect left
  exactly. With two left, the inferred row is their lub, `Exception`,
  unless the caller writes the witness. Lambdas of `java.util.function`
  refuse effects, and a catch-all discharges one statically.
- `JavaDirectEffectsBenchmark`, one operation (tail-resumptive) on three
  machines: okay `Free` 14.3 ns, `jdk.internal.vm.Continuation` 104 ns
  (7.3x), virtual threads 1.51 µs with both sides virtual (105x) and
  7.46 µs with a platform handler (522x). Recorded in history.d
  `java-direct-probe`.
- Verdict: no direct-style Java API now. `Eff` + `Cap` stay. The possible
  shape (throws + internal continuations, one-shot, opt-in) is filed as
  backlog polyglot `java-direct-style` with its trigger.
  specs/java-direct-effects.md.
