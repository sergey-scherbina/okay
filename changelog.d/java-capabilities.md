## java-capabilities - a static row for Java, as capability parameters

- okay-java gains `Cap<O>`. A handler (`Cap.answer`/`state`/`into`/
  `control`, the four forms) makes a capability and hands it to its body,
  and operations are performed through it and taken by its IDENTITY. A
  Java program's effects are now its method's parameters: it cannot be
  called without them, and only a handler makes one. This is
  capability-passing style (Brachthäuser, Schuster, Ostermann, OOPSLA 2020).
- Identity rather than class means two instances of one effect coexist
  (two `Var<Integer>`, nested handlers of one effect). Built-ins: `Var`,
  `Env`, `Raise` (typed error), `Io` (Async, only from `Io.run`).
- An escaped capability is refused by name at `run()`, also inside a new
  handler of the same effect, at no cost on the hot path.
- `TestJavaCapabilities`, 11 tests over `JavaCapabilities.java`. A mutant
  turning the identity test into a class test fails four of them.
  specs/java-capabilities.md; docs/modules/okay-java.md.
