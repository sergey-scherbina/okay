## continuations-tidy - what was extra in the continuations, removed

The operator's ask (2026-10-02): remove everything extra from the
continuations. Almost nothing was unused; what was extra was prose.

- Gone: `Delim.Stacked.contShift` and its suite `TestContOnMachine` (a
  stage-7 probe of Cont's `shift` on Delim's machine, moot since Cont
  itself runs on that machine), and the unused `given samePrompt`.
  `Delim.in`/`out` are package-private.
- Comments one or two lines, the code byte-identical (compared with
  comments stripped): Delim.scala 1117 -> 492 lines, Lexical.scala
  332 -> 209, Layered.scala 119 -> 63, ContMacro.scala 275 -> 229,
  StackSwitch (JVM/Native/JS) 65/45/16 -> 42/35/8. The reasons and the
  history are in the specs.
- Kept on purpose: `Delim.pausing` (no caller, but the documented
  nested half of `resumable`, as `scope` is of `delimited`).
