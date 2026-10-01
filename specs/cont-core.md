# The continuation core: two operations, one machine, everything else on top

Operator ask, 2026-10-01: "очистить дизайн продолжений от всего лишнего и
оставить только самое необходимое и сделать это необходимое идеально
правильным". Design first; speed is measured after, against master, and
optimizations come back one at a time, each with its number.

## Overview

The machine on master (specs/freer-kont.md) is right in its MODEL — a
type-aligned list of segments, each a type-aligned list of frames, cut at
delimiters (Dybvig, Peyton Jones & Sabry 2007) — and carries a year of
fast paths and features that each rewrite one of its rules a second or
third time: a fourth stack node (`Kept`), a capture without the walk
(`nearest`), five entries, a resumption thunk class, flags on `Shift0`
(`under`, `strict`), a per-delimiter re-entry count (`Shots`). Cont's
facade adds a second runtime around it (a root delimiter class with a
stack room and a gauge, `force`/`nested`/`enter`/`rootOf`).

This spec fixes the core to what the calculus needs (Materzok &
Biernacki's λ$, APLAS 2012/ICFP 2011) and puts everything else on top of
it as derived operations or libraries, with no flag in the machine for
any of them.

## The core

### Prompt

`Prompt[Y]`: a delimiter's identity, compared by `eq`, labelled for
error messages. Unchanged.

### Two operations (`Cont0`)

```
Reset0(p: Prompt[Y], ret: A => Freer[Row, T, T, Y], body: Freer[Row, T, R, A])   -- ret $_p body
Shift0(p: Prompt[Y], f: Stack[X, T, T, Y] => Freer[Row, T, R, Y], bare: Boolean) -- shift0_p / control0_p
```

`bare` is the one flag, and it is the calculus's own: control0's `k`
is the segment WITHOUT the delimiter, shift0's is the segment WITH it
(the `$/S0` rule: `k` carries `ret`). Lexical's shallow handlers are
control0 (shallow handlers ≙ control0, Hillerström & Lindley), so it is
not removable. A bare capture to a delimiter whose `ret` is not the
identity is refused (its `k` would answer the body's type, not the
prompt's: specs/shift0-dollar.md), so "plain" stays: `ret eq identity`.

### State: two type-aligned lists

```
Frames = End | Frame(f: A => Freer[..], rest: Frames)        -- one segment
Stack  = Done | Run(frames, below) | Reset(p, ret, frames, below)
```

`Reset` carries the segment under it (DPJS's layout: each prompt heads
the frames waiting for its answer). A captured `k` is a `Stack`; it is a
function `A => Freer` (a resumption is `Bind(Return(a), k)` or `k(a)`),
recognised by its class at a bind.

### The loop: one entry, five rules

`run(p): Freer` — run to a head form: `Return(z)`, or `Bind(op, stack)`
for the first operation no delimiter here answers.

1. `Bind(m, f)`, `f` not a Stack: push `Frame(f)` and go into `m`
   (`Bind(Return(a), f)`: apply, push nothing — the tree's own rule).
2. `Bind(m, k)`, `k` a Stack (a resumption): link `k`'s nodes onto the
   live stack and go into `m`.
3. `Return(a)`: pop a frame and apply it; at the end of a segment pop the
   node below — `Run`: its frames become the segment; `Reset`: apply
   `ret`, its frames become the segment; `Done`: the answer.
4. `Reset0`: push `Reset(p, ret, segment, stack)`, the segment empty,
   go into `body`.
5. `Shift0`: walk the stack to the first `Reset` of `p`. `k` = the nodes
   above it, with it (not bare) or without it (bare); the body `f(k)`
   goes on in the delimiter's place, over the segment and stack that were
   under it. No delimiter of `p` on this machine: hand it out as a head
   form (`Bind(op, stack)`), for a machine outside.

`Delay(t)`: force `t`, go on. Any other operation: hand it out as a head
form. That is the whole machine.

`Rev` (the reversed prefix a cut builds, and `link`/`reverse`) stays: it
is how rules 2 and 5 stay O(nodes crossed) with frames shared.

## Derived, not in the machine

| operation | as |
|---|---|
| `reset_p e` | `Reset0(p, identity, e)` |
| `shift_p f` | `Shift0(p, k => Reset0(p, identity, f(k)))` — APLAS 2012's `S k.e = S0 k.⟨e⟩` |
| `control_p f` | `Shift0(p, k => Reset0(p, identity, f(k)), bare = true)` |
| `abort_p v` | `Shift0(p, _ => Return(v))` |
| boundary / `NoPrompt` | `Delim.run` installs a `Reset` of a prompt nobody can name; a cut reaching it throws `NoPrompt` with the prompts it passed. The one place the machine looks at a specific prompt — an error, not a feature. `runNested` installs none. |

## Cont on the core

`Cont.shift[A, S, R](f: (A => S) => R)` keeps its signature (operator,
2026-10-01). A Cont program is a core program of one root prompt per
run: `run(c)(k)` installs `Reset(root, k)`, and a leaf is a `Shift0` to
the nearest root.

The leaf's body gets `k` as an OBJECT: `final class Resumption(stack)
extends (A => S)`, whose `apply(a)` runs the core machine on `stack` with
`a` at its top — a loop, so `k`'s own frames cost no JVM stack however
many there are (the operator's "трамплининг по его стеку").

What a trampoline cannot remove, and so what stays as a separate, small
mechanism: an opaque body that USES `k`'s answer (`k => k(x) + 1`) keeps
`+ 1` in its own JVM frame across `k.apply`, and n nested such bodies
are n nested machine runs. That depth is bounded (operator rule: no
unbounded recursion) by a LEVEL COUNTER on the run: each nested
`Resumption.apply` takes one level, and at zero it continues on a fresh
stack (`StackSwitch.fresh`). A fixed number of levels; the measuring
gauge (`Gauge`, `StackSwitch.more`, `StackRoom`) is an optimization and
leaves the core.

`ContMacro` (a tail body to a value; an answer-using body CPS-transformed
into a program over a lazy `k`) is an OPTIMIZATION over this: it stays
as a separate layer, emitting core programs, and the core does not know
it exists — no `strict` flag: the strict leaf is `Shift0(root, k =>
Return(f(Resumption(k))))`.

## What leaves the machine, and where it goes

| on master | where |
|---|---|
| `Stack.Kept`, `delimiterOf`, `keptUnder`, `relinkOrSplice`, `close` | gone; a capture builds `Run`/`Reset` nodes. Back later only with a number |
| `nearest` | gone; the walk finds the head delimiter in one step anyway |
| `Shift0.under` | gone; `shift`/`control` derived (table above) |
| `Shift0.strict`, `Cont.strictBody` | gone; the strict leaf is a closure over `Resumption` |
| `Shift0.at` | the error label rides on the prompt and the `NoPrompt` site |
| `Resume`, `Delay` arm for it, `runUnder`, `runOn`, `enterIn`, `enterAt` | one `run`; a resumption from outside is `k(a)` |
| `Reset0.shots`, `Shots`, `Enter`, `counts`, `dollarResumed` | gone from the core; see Decisions (Lexical's guard) |
| `Root.room`/`gauge`/`node`, `force`, `nested`, `enter`, `rootOf`, `Gauge` | `Resumption` + the level counter |

Libraries on top keep their API and move to the core's: `Delim`'s doors
(`push`, `dollar`, `shift*`, `control*`, `abort`, `run`, `runNested`),
`Prompted`/`scope`, `Stacked`, `Emitting`/`collect*`,
`Paused`/`Asking`/`drive`/`replay`, `Layered`, `Lexical`, `Cont.direct`,
`Cont.Monadic`, `Control[Cont]`, `Control[Func]`.

## Behavior

- [x] the five rules are the loop's only arms (plus `Delay` and the head form)
- [x] `Stack` has three cases, `Frames` two, `Cont0` two, `Shift0` one flag
- [x] shift/control/abort/reset are derived in `Cont0`'s companion, not matched in the loop
- [x] Cont's leaf body gets a `Resumption`; nested opaque bodies bounded by levels + `StackSwitch.fresh`
- [x] TestKont, TestCont, TestContOnMachine, TestContStack, TestContMacro (its 1M nested case included), TestDelim*, TestDollar*, TestHandlersAsDollar, TestLexical*, TestStackedShift0, TestLayered, TestBookFourCaptures, TestScopedEffects, TestStackSafetyCore, TestHandleForward, TestCollectUntil green
- [x] the linear cases stay linear: n captures from a deep stack, n nested resumptions (TestKont's depth tests)
- [ ] `affected master staged` green
- [ ] A/B against master recorded in history.d — a number, not a gate: this lane's verdict is the design

## Decisions

- **Model kept** (operator, earlier the same day): segments of frames,
  immutable and shared; the array stack is parked (backlog
  frames-array-stack).
- **Cont.shift keeps `(A => S) => R`** (operator, 2026-10-01): `k` an
  object trampolining over its own stack. Recorded with it: a trampoline
  bounds `k`'s frames, not nested opaque bodies; those need the level
  counter (above).
- **Lexical's tail guard** (OPEN, operator to confirm): the guard counts
  re-entries of a captured context through the machine (`Shots`), and
  that cannot be seen from outside the machine: a resumption re-enters
  at the capture point, inside the body, and nothing at the installation
  runs. Proposed instead: a tail installation in a row WITH `Delim` (the
  only case the guard exists for — the `unguarded` case is already the
  no-Delim row) is installed DEEP, its state carried by the continuation,
  which is correct under multi-shot by construction. The fast tail road
  stays for rows without `Delim`. Correct by design instead of detected
  at run time.
- **Optimizations return one at a time**, each its own lane with an A/B:
  `Kept`/`nearest` (capture), `under`, the entries, the gauge.

## Results

Nine steps, each a commit with the 24 continuation suites green (238
tests). One slip: step 2's Lexical half was left out of its commit
(6c222e806) and landed as 63f4cc84c, so the commits from step 2 to
step 7 do not compile one by one; the branch tip does.

1. Lexical `tail` in a Delim row is installed deep (4c4917ac9).
2. The re-entry count left the machine: `Shots`, `Enter`, `dollarResumed` (6c222e806, 63f4cc84c).
3. `shift`/`control` derived; `Shift0.under` gone (6331f4cf6).
4. Cont's strict leaf an ordinary clause; `Shift0.strict` gone (d8a5d600a).
5. `Kept`, `nearest`, `close`, `relinkOrSplice` gone (e97c291b5).
6. One entry `Frames.run`; Cont's `Resumption`, levels + `fresh`, the gauge no longer asked (c5ce9fb9d).
7. The runner's stack reader gone: `StackSwitch.more`, `Cont.Gauge`, Native's `ThreadInfo` probe, `ContStackRoad`; `StackRoom` and `okayJdk22` KEPT (operator) (dc04840bc).
8. `Reset` unfused: `Reset(p, ret, below)`, DPJS's `EmptyS | PushSeg | PushP` (e05a3c943).
9. `rebase` gone from the machine: `Cont0.Delimiter[Y, I]` (84c9e2eb5).
10. `plainly` gone: the plain `reset` is its own operation (`Reset0(p, body)`) and node (`Reset(p, below)`, DPJS's `PushP`), `$` is `Dollar0`/`Dollar`; a bare capture to a plain delimiter is typed by the node, a bare capture to a `$` refused by its case, and `Cont0.identity`/`plain` (the eq-trick) gone. `Stack = Done | Run | Reset | Dollar`.
11. `control`/`control0` gone (operator: "Удаляй"), and with them `Lexical.shallow`, `ShallowClauses`, `Delim.Stacked.Plain`, the `bare` flag — and the separate plain node of step 10, whose one reason was a typed bare capture: the core is λ$ exactly, `Dollar0` and `Shift0`, `Stack = Done | Run | Dollar`, a `reset` is `pure $ ·` (a non-capturing lambda, one object per call site). Book chapter 11 rewritten as "Two captures"; TestBookFourCaptures, TestDollar, TestDelim, TestHandlersAsDollar, TestLexical, TestStackedShift0 and DelimBenchmark.stateShallow lose their control/shallow cases.

### What the machine still claims, and why each stays

| claim | why |
|---|---|
| `as`, `resume` | a class test on a function: JVM erasure |
| `noFrames`, `noStack`, `Rev.nil`, `identity`, `boundary` | one empty value at every phantom index, `Nil`'s pattern; the alternative allocates per use |
| `identical` | two prompts that are one object are one type: the generative-prompt axiom (DPJS's `eqPrompt` is the same `unsafeCoerce`) |
| `splice`'s `Done` case | GADT refinement does not reach through the `@unchecked` test; small, open |

### Found on the way

- **`Resume` is the core's, not an optimization.** A resumption must be
  pushed by the machine (it stays in the run) and run by an outer
  interpreter that is handed `k`; a bare `Bind(Return(a), k)` loops
  under `Freer.resume` (`k(a)` again, forever).
- **The boundary stays in `cut`.** It is a node of the stack, so it
  travels in every `k`, and a run of `k` started by an outer
  interpreter meets it after the door has returned. It is the
  continuation barrier of Flatt et al. (ICFP 2007).
- **The index belongs to the delimiter.** `rebase` existed because the
  machine could not relate the leaf's index to the delimiter's; a
  prompt that carries its index makes `eq` type both. The claim is now
  a statement at each door about the index it installs at.
- **`$` is not `reset ∘ map`.** `ret $ v` runs `ret` OUTSIDE the
  delimiter, `⟨ret v⟩` inside: a return clause that captures to its
  own prompt tells them apart. So `$` stays a primitive with `ret`.

### Open

- The full `affected master staged` gate, and the A/B against master
  (a number for the record; the verdict of this lane is the design).

## Literature

- Materzok & Biernacki, "Subtyping delimited continuations" (ICFP 2011)
  and "A dynamic interpretation of the CPS hierarchy" (APLAS 2012): λ$,
  `shift0`/`$`, `S k.e = S0 k.⟨e⟩`.
- Dybvig, Peyton Jones & Sabry, "A monadic framework for delimited
  continuations" (JFP 2007): the stack as segments split at prompts.
- Hillerström & Lindley, "Shallow effect handlers" (APLAS 2018):
  shallow handlers and control0.
- Danvy & Filinski, "Abstracting control" (LFP 1990): shift/reset with
  answer-type modification — Cont's signature.
