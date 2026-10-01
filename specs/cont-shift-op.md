# Cont with its own operation: `Shift = Strict | Cps`

Operator ask, 2026-10-01 (names his): Cont stops riding `Cont0`. A
Cont program is a freer tree over ITS OWN signature, one operation in
two forms; the frame machine is its trampoline; `c / k0` is the
handler that answers the operation. No flag, no case, no root prompt
in the core for Cont's sake.

## Overview

```scala
enum Shift[S, R, +X]:
  case Strict[A, S, R](body: (A => S) => R)           // direct style: k is a function to a value
  case Cps[A, S, R](body: (A => S) => Program[R])     // after ContMacro: k(x) is a node of a program
```

- **A Cont program** is `Freer` over `Cont0.Row[Shift]`; it holds no
  `Cont0` operation (no prompt, no delimiter): the machine runs its
  binds on its stack, and a `Shift` is FOREIGN to it, so it leaves as
  the head form `Bind(Inject(shift), k)`, `k` the whole stack of frames.
- **`c / k0`** runs `Bind(c, k0)` — `k0` the bottom frame, so every `k`
  a head form hands out already ends in it — and answers the head form
  in a loop:
  - `Return(v)`: the answer, `v`;
  - `Strict(body)`: `body(Resumption(k))`, its answer the run's — a
    `Resumption`'s `apply` runs `k` from `x` (`Frames.enterAt`) and
    answers ITS head forms the same way, nested and counted (levels,
    `StackSwitch.fresh` at zero, the room the run's);
  - `Cps(body)`: `body(k)` is a program; the loop runs it next, in the
    same loop — `call(k, a, rest)` is `Bind(k(a), rest)`, `k`'s frames
    pushed by the machine.
- **`reset(c)` is `c / identity`** (Danvy and Filinski: reset is the
  eliminator, not an operation), so a `reset` nested in a Cont program
  is a nested run, as it is today.
- **`Program[R]`** is the Cps body's answer type (was `Cont.Lazy`),
  built by the macro's `call` and `done`; `cpsLeaf` (was `lazyLeaf`).

## What leaves Cont

The root prompt and its delimiter (`root`, `rootAt`, `Root`, `rootOf`),
the clause lambda each opaque leaf built, Cont's use of `Delimited`
and `Cont0` operations. `Cont0`/`Delimited` stay λ$ alone.

## Behavior

- [ ] `enum Shift = Strict | Cps`, a Cont program over it; the core untouched
- [ ] `c / k0` the handler loop; `reset` = `/ identity`
- [ ] the Cont suites green (TestCont, TestContMacro with its 1M nested case and zero switches, TestContStack, TestContOnMachine, TestStackSafetyCore, TestDocExamplesContStack) and the 24
- [ ] A/B on contAnswer, statePara, fib100 against master: the verdict of the probe

## Decisions

- Names `Shift`, `Strict`, `Cps` (operator). `Program` for the Cps body's
  answer (operator's sketch).
- No `Reset` operation: reset is the eliminator. A stack-safe deeply
  nested reset would need a stack of delimiters, which is `Cont0` again.

## Results — REFUTED (2026-10-01)

Probed on feature/cont-shift-op (9d85c5105, not landed). `Strict`
works. `Lazy` does not, and cannot without the thing it was to remove:
D-F's `k` is `λx. reset(K[x])` — every call of `k` is delimited. A
strict `k` gets the boundary for free (each call a separate machine
run); a lazy `k` pushes its nodes into the SAME machine the body's own
rest runs in, so the next `Shift` inside `k` leaves as a head form
carrying the body's rest too (`k2 = [k0, restA]` where it must be
`[k0]`), and `k(x+1) + k(x+1)` at d=2 loops to OutOfMemoryError. The
boundary is a delimiter in the machine — a `shift0` to the run's root,
which is master's design. A boundary drawn by the handler would be the
machine's `cut` written again outside it; the clause lambda per leaf
it was to save is only removable by the machine knowing Cont (a flag
or a case in `Cont0`), refused for layering. Verdict: master's Cont
(root `$` + `shift0` through `Delimited`) is the correct one.

