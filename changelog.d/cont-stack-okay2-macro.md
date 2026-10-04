## cont-stack-okay2-macro - okay2's Cont.shift reads a tail body: no frame, no room, no switch

Layer 1 A of specs/cont-stack.md in Scala 2, the Scala 3 core's
`ContMacro` tail case. Operator: "Делай то что нужно для okay2 только
сразу всё" (do what okay2 needs, all of it at once).

**`Cont.shift` is a blackbox macro.** A body that calls its continuation
only in TAIL position, with an argument free of it, is rewritten to
`tailShift(() => v)`, or `tailPure(v)` for a literal. It reaches the
tail position through blocks, `if` and `match`, and a `throw` branch is
allowed. The runner walks such a node in its own loop.

- The rewrite needs `S <:< R`, which the macro summons. Without it, the
  body stays the leaf.
- Every other body, and a function passed as a value, is the leaf as
  before (`shiftLeaf`).
- okay2's own core cannot expand a macro it defines (Scala 2), so its
  nine call sites write `shiftLeaf`. None of their bodies was tail-shaped.

**Tests:**
- TestContMacro (JVM): a million tail shifts on 128 KB, plain and under
  `if`, `match`, a block and a throw branch, plus a literal. All give the
  answer with ZERO stack switches. Red first: StackOverflowError.
- TestContMacroMeaning (cross): statements run once, in order, when the
  runner reaches the shift; an exception surfaces at run time; non-tail
  bodies and multi-shot keep their meaning; the answer types hold.
- The runtime-layer probes (TestContStack and its Native twin,
  TestCont's absorption tests) now build leaves explicitly, so they still
  test the switch and absorption.

**Left (backlog cont-stack-okay2-macro):** Layer 1 B, the answer-using
bodies. It needs okay2's runner to have the Scala 3 frame machine's lazy
`k` first.
