## freer-kont-migrate - the frame machine is the core's continuation runtime: `Frames`, `Cont0 = Shift0 | Reset0`, `Delim` over it

- `Frames`, `Cont0` and `Frames.run` move from the probe's `okay.kont`
  into package `okay` (Cont.scala). `Freer` is untouched and keeps its
  `resume`; the machine is a second interpreter, orthogonal to the tree
  (operator: "Freer самодостаточный, Cont ортогональный"). `Cont0` in its
  final shape: `Prompt[Y]` is Delim's, `Reset0` carries `plain` and
  `shots`, `Shift0` carries `bare`; `Frames.apply` answers
  `Delay(Resume(a, fs))` — the continuation carries its own interpreter
  (forced by an outer loop it runs the machine, met by the machine it is
  spliced). Commits e33df450d..f217262ee, specs/freer-kont.md stage 2.
- `Delim` over the machine: `type Delim[+A] = Cont0[?, ?, ?, A]`, every
  unstacked door a re-typing over a `Cont0` door (`in`/`out`, the one
  claim), `run` under a BOUNDARY reset that turns an unanswered capture
  into `NoPrompt` with the delimiters passed, `runNested` without one;
  the `Stacked` doors over `Cont0` with `rebase` as their one claim,
  `contShift` a strict `k` forcing the resumption; `Op`, `Segs`, Delim's
  `Frames`/`Hole`/`Cut`/`Step` and `loop`/`split`/`copy`/`reify` deleted
  (459 lines to 245). Lexical's deep instance is a `Reset0`. A
  `dollarResumed` count is a FRAME (`Enter`) on the captured segment,
  popped once per resumption, never on re-entry — found by
  TestLexicalTail's guard in the full gate.
- Delim.scala's header: the "opaque forwarding" reason for `push` as an
  operation refuted; the two real reasons named (an operation passes a
  handler loop, a frame does not; forwardability), and the one-machine
  rule's true reason, stack depth.
- Measured (history.d `…-freer-kont-migrate.tsv`, DelimBenchmark's own
  lanes): delimiter install 0.76-0.79x, capture+resume 0.82x, generator
  0.94x, Lexical's deep lane 0.87x with the old machine's JIT modes gone.
  `affected master staged` green.
- Left (backlog `cont-step-on-frames`): whether `Cont.step`'s strict
  runner with its `StackSwitch` rooms becomes `Shift0` with a strict `k`
  on `Frames.run`; docs/continuations-in-practice.md's "one machine"
  rule re-read against the machine.
