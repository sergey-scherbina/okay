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
