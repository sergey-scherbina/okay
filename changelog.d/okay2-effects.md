## okay2-effects - Resource, Once, Delim and Provide for the Scala 2 core

Four more effects into okay2 (specs/okay2.md stage 6): `Resource`
(acquire/open/scoped/run/runAt, `Failing` with `pure` and an explicit
`never`, `bracket`; okay2-async adds the `Async` and any-row
instances), `Once` (`!.once`, cells threaded through `run`), `Delim`
(multi-prompt: prompt/push/shift/shift0/control/control0/abort/reset,
the evidence doors `Prompted.Aux`/`scope`/`delimited`/`exit`/
`onReturn`, `Emitting.Aux` with collect/collecting/collectUntil/emit,
`Asking.Aux` with resumable/pausing/pause/drive/answer/replay,
`OneMachine`, `runNested` forwarding by witness, `Replayable`
inductive, `NoPrompt`, `At`, `Same`) and `Provide` (provide 1–8
curried, `Providing`/`and`/`providing`/`wire`, `Fact`/`Facts`,
`Module` with both `and` forms/installing/use/declare/declaring,
module/moduleAs/prototype/`New`/fresh). 88 tests; 274 in
the okay2 gate.

- The evidence doors take the evidence FIRST as a value —
  `Delim.shift[Int, Int](in)(k => k(5))`, `Delim.emit(e)(a)`,
  `Delim.pause(s)(q)` — and the evidence carries its row as a type
  member, so no door needs the inline Scala 3 doors' casts.
- The machine's typed chain keeps its types; `Done` carries `A =:= Z`
  as a value because scalac 2 cannot prove a method's type equality
  from a pattern.
- `provide` bodies are curried, nearest-wins is by NAME (Scala 2
  shadows implicits by name), `Module.and` is overloaded on plain
  and context-built operands.
- Not ported: plan/exports/shadowed (macros; backlog
  okay2-module-plan), Delim.Stacked (backlog okay2-delim-stacked),
  SharedOnce, the direct-block doors.

Docs: docs/okay2.md section 13; specs/okay2.md stage 6.
