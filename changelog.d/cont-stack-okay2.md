## cont-stack-okay2 — Cont past the stack in the Scala 2.13 core

specs/cont-stack.md plan stage F, okay2's half of stack-safety stage
1c: the Scala 3 core's Layers 2 and 3, ported.

- `okay2.Cont`'s runner carries the room as a parameter of `step` and a
  field of `Reentry`; at zero `StackSwitch.more` grants a slice where
  the stack can be read, and `fresh` hands the rest to a parked worker
  with a 1 GB stack (`StackPool`, shared by JVM and Native, spin then
  park). The gauge is attached at the chain root on a run's first
  exhaustion, never allocated before.
- Per platform (`platformSources` on the core, new): JVM counts on
  every JDK — no FFM variant in okay2's build yet; Native reads the
  runtime's `ThreadInfo` exactly (`stackBottom` the highest address,
  `stackTop` the lowest); JS keeps the engine's bound.
- MEASURED: a Scala 2 level is ~4 KB cold (11 frames: `$adapted`
  wrappers, `at`, two specialisation bridges), so the first room
  derives from 4.8 KB — ~218 levels on a 2 MB thread. At the Scala 3
  core's 1.2 KB (873 levels) the first test of a fresh JVM overflowed
  before the first look. The test fork runs with `-Dokay.cont.room=64`,
  as the root build's core suite does.
- TestContStack (6, red first — 20 000 levels overflowed a 2 MB thread —
  then green: tail, answer-using, absorbed, multi-shot, an exception
  across the switch, a 256 KB thread switching) and
  TestContStackNative (3). okay2 JVM 362, JS 349, Native green, no
  warnings; `specs/stack-safety-okay2.tsv` lost its two `Cont` rows —
  the inventory only shrinks, and it did.
- Open, filed as okay2/backlog.d/row/cont-stack-okay2-macro: the
  tail-body macro in Scala 2, and the JDK 22+ reader as okay2's own
  Multi-Release variant.
