# handle-frames — one loop for every handler, so nesting takes no host stack

Status: FINDING, 2026-10-02; design open. Lane `handle-frames` (operator:
"Берись сейчас", after "а зачем вообще вложенные вызовы машины? Почему не
та же самая?").

## What was believed, and what the test showed

Believed (shift-stacked-key's note): a machine forced by ANOTHER
interpreter's loop, e.g. a `State.run` between two resets, is the nested
call left. Measured (TestHandleInMachineSmallStack, red on 128 KB): the
cycle has NO machine in it —

    State.loop -> Freer.resume (forces a Delay) -> user code -> reset
      -> (builds its body) State.handle -> State.loop -> ...

and `State.handle` nested in `State.handle` with no `reset` at all
overflows the same way (frames: `State$$anon$1.loop$1`, `State.handle`,
the user's function, `Freer.resume`). EVERY handler in okay is an EAGER
loop run at the call: a handler called from code that another handler's
loop forces (a `tailcall`, a continuation) runs inside it. The depth is
the program's dynamic handler nesting (a handler per recursion level —
`Reader.local` in a tree walk, a `State` per level), not its iteration
count; iteration is already safe.

## Why a handler cannot simply forward the nested run outward

A loop meeting a nested run could return it as residual, as it forwards
an unknown effect. Wrong when the nested run's row holds the effect this
loop handles (a `State.handle` inside a `Writer` handler emits `Writer`
operations that must reach the Writer loop, which would have returned).
Re-wrapping the forwarded run in the outer handler re-descends every
active handler per level: the depth comes back, plus O(n^2) work.

## The design space

1. ONE MACHINE WITH HANDLER FRAMES (OCaml 5, Koka): a handler is a
   delimiter on the machine with its clause and its state; an operation
   searches the stack for its frame. Constant host stack for any nesting.
   Measured once already as handle-on-machine: 1.74x time and bytes on a
   10 000-operation program, so not as the only road.
2. UPGRADE ON NESTING: handlers stay fast folds; a fold that meets a
   nested run (a `Frames.Own` or a handler node) turns ITSELF into a frame
   of a machine and hands over. The 1.74x is paid only where handlers nest.
   Every loop that walks a program must recognise the node: ~50 loops in
   the core (Writer 10, Gen 9, Effects 8, Generate 7, State 3, ...) and 31
   files in the modules.

## Decision: upgrade on nesting (operator, 2026-10-02)

### The pieces

1. **A run as a value (`Frames.Run`)**, generalising `Frames.Own`: a
   `Delay` thunk with `program` (what a running machine steps into) and
   `apply()` (what anything else does when it forces it). `Own` is the run
   of a program; `Handled` is a handler over a program — `apply()` its fast
   fold, `program` the handler as a FRAME.
2. **Handler frames on the machine**: a frame is `ret $ body` (a `Dollar`)
   whose delimiter is a `Handler.Prompt` — it knows which operations are
   its own and has the clause. The machine meets an operation that is not
   `Cont0`: it looks for a handler frame on its stack that takes it, and if
   there is one the operation is a `shift0` to that frame with the clause
   for its body (`k` as data, deep: `k` re-installs the frame). None: out,
   as today. State-like handlers are parameter-passing: the frame's answer
   is `S => program`, the clause applies the continuation's answer to the
   next state (the encoding handle-on-machine validated against the fold).
   Only a machine that has a handler frame looks (a flag set when one is
   pushed or a resumption enters from outside), so a `Shift`-only machine
   forwarding an operation pays nothing new.
3. **Lazy handlers**: the handler forms' `run` answers `Delay(Handled)`,
   not the loop's result. Forced outside a machine (`!.run`, an unconverted
   loop) it is the fold, as now.
4. **Upgrade**: a converted fold walks with `resumeRun`, which stops at a
   `Delay` of a `Run` instead of forcing it. Meeting one, the fold answers
   `Delay(Own(itself as a frame over the rest, at its current state))` and
   is done: the machine takes the rest, the nested run and every handler
   it meets after, as frames. An UNCONVERTED loop forces the node — the
   right answer, nested as today — so loops convert one at a time.

### Stages

1. `Run`, `resumeRun`, handler frames and their dispatch in the machine;
   the state form (`Handler.stateOf`, so `State`) and the control form
   (`Effects.handle`) lazy and upgrading.
2. The answer form (`relay`) and the into form (`translate`).
3. The bespoke loops, one per lane, each with its red test: Writer, Gen,
   Generate, Resource, Lexical, Throws, Supply, ... and the modules' 31
   files.

## Behavior

- [ ] stage 1: 100 000 nested `State.handle` on 128 KB, no `reset`
      (TestHandleInMachineSmallStack)
- [ ] stage 1: 100 000 levels of reset / `State.handle` / reset on 128 KB
- [ ] stage 1: the same two on Scala.js (cross)
- [ ] stage 1: nested `Effects.handle` (a control clause resuming twice)
      on 128 KB, and its answers equal the fold's on a multi-shot program
- [ ] the fold's own cost unchanged where nothing nests (HandlerBenchmark
      / the State lanes, alternated); the nested road priced
