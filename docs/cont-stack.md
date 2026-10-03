# Cont and the stack: what nests, what does not, and what it costs

`Cont` is direct style: a shift's body receives its continuation `k` as an
ordinary function and calls it whenever it likes — `k(1) + k(10)`,
`a :: k(x)`, `s"answer: ${k(20)}"`. Calling `k` runs the *rest of the
program* inside the body's call, so shifts in a row whose bodies call
their continuation nest one stack frame each. Twenty thousand of them
overflow a small stack; a million overflow any stack. Since
`cont-stack-switch` (2026-09-25) that does not happen, and this page
says how, what it costs on each platform, and where the written
bounds are. The design and its measurements are in
[specs/cont-stack.md](../specs/cont-stack.md).

## Three kinds of body, three prices

**Since 2026-10-01 Cont runs on the frame machine** (cont-on-frames,
[specs/freer-kont.md](../specs/freer-kont.md)): a run is a frame of its
own whose `ret` is your `k`, and every leaf an operation (`Cont.Op`) the
nearest run's frame answers — each `reset` delimits its own shifts
(cont-run-prompt, 2026-10-03) — on the same machine `Shift` runs on,
with its stack on the heap. The three
kinds below are unchanged in what they cost the *stack*. What changed
is who interprets them: the second kind is a program over a lazy `k`
the machine pushes (no pending stack of the runner's own any more), and
the third forces `k` as a nested run of the machine, counted and
switched exactly as below. Against the runner this replaced, measured
the day it landed: the second kind (`HandlerBenchmark.contAnswer`) at
1.09x, `PState` (statePara) at 1.85–1.90x, a generator over `Cont`
(`FibBenchmark.fib100`) at 2.62x — the third kind's strict `k` is where
the work is. The same day's next lane (cont-strict-k) brought statePara
to 1.67–1.71x and fib100 to 2.45x, and had the macro emit the second
kind's program itself — simpler, fewer bytes, contAnswer at 1.20x.

**A body that only ever calls `k` last** — `k => k(v)`, also under
`if`/`match` and after statements that do not mention `k` — is the
value it passes, decided at compile time: `shift` is a macro, and such a
body becomes `pure(v)` evaluated when the runner reaches it. No frame,
no bookkeeping, no switch, on any platform. A million of them run on a
128 KB stack:

```scala
val deep = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1))))
Cont.reset(deep) // 1000000 — no frame per level: the body is the value it passes
```

**A body that uses the answer** — `k(x + 1) + 1`, `k(1) + k(10)`,
`a :: k(x)`, `s"${k(a)}"`, a block with `val a = k(1)`, an `if` or
`match` in tail position with calls in its branches — is CPS-transformed at compile time (since
`cont-stack-layer1-b`): each call of `k` becomes a step naming what is
left to do with its answer, and those pending parts live on the
machine's own stack (a frame each) instead of the JVM's. Still no frame per
level, on any platform; what ran before a call still runs before it;
`k` is multi-shot as before. A million of these run on a 128 KB stack
too. The price, measured on a thousand-level program of exactly this
shape (`HandlerBenchmark.contAnswer`): 1.24x the time of running it
direct on a JVM that has room, at 0.81x the bytes — the walk is data
the runner interprets where the direct road was calls the JIT inlines.
What it buys is that such a program never touches the stack: on a
small thread, and on Scala.js, where there is no fresh stack to switch
to.

```scala
val used = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => k(x + 1) + 1)))
Cont.reset(used) // 2000000 — no frame per level either: the pending `+ 1`s live on the runner's own stack
```

**Read since 2026-10-02 (cont-stack-layer1-c):** a call under a
conditional that is not in tail position (`1 + (if c then k(1) else
2)`, a `match` feeding an expression) — the rest after it becomes one
local function every branch ends in, a join point — and a call inside
the lambda of `map`, `foreach` or `foldLeft` on an immutable collection —
`List`, `Vector`, `Seq`, `Set`, `Map` (`List(1, 2).map(x => k(x)).sum`,
`Set(1, 3).map(x => k(x) % 2)` still a `Set`, or `k` itself passed:
`List(1, 2).map(k)`), the traversal a chain of binds the machine runs;
an assignment from `k` (`v = k(1)`, `seen += k(x)`); the KNOWN methods —
`flatMap`, `exists`, `forall` (stopping at the first element that
decides — over an infinite `LazyList` too), `find`, `foldRight` on the
same collections, and `Option`'s `getOrElse`/`map`/`flatMap`/`fold`/`orElse`,
`Either`'s `fold`/`getOrElse`/`map`/`flatMap`, `Try`'s `getOrElse`,
`&&`, `||`, rewritten into a `match`/`if` (the
receiver once, a by-name argument only in its branch); an `inline def`
helper with `k`, or `k`'s answer, among its arguments (`applyTo(k, x)`,
`plusOne(k(x))`); and a `while` loop
with `k` in its condition or body, each iteration a step the machine
forces (a million iterations that never call `k` hold no frame either).
All are programs over a lazy `k`, no frame per level.

A `direct { }` block as a body whose answer is a program
(`k => direct { !k(x) + 0 }`) never needed reading: the block is binds
by the time `shift` sees it, so `k` is called later, from the
interpreter's loop (TestContDirectDepth, a million on 128 KB).

**A body the macro cannot read, or should not** — `k` handed to an
unknown (not `inline`) function as a value, in a by-name
argument, under `try`, or in `Try(…)` and `Try`'s `map`/`flatMap`/`fold`/
`recover` (on purpose: they catch what their code throws, and with a lazy
`k` the rest would run outside them), in the lambda of a MUTABLE
collection's traversal (on purpose: the traversal would read the
collection after the body could have changed it), in a `LazyList`'s lazy
`map` (it forces no element the program does not ask for), in a lambda, `k` passed into Java or an
abstract method, a body passed to `shift` as a value rather than a
literal — runs direct, and each level is a frame. The
runner counts the levels the current stack has room for; when the
count runs out it hands the rest of the program to a fresh stack — a
parked worker thread with a 1 GB stack — and waits for the answer. No
exception unwinds anything and nothing runs twice: the frames below
stay where they are until the answer comes back. Multi-shot bodies
keep working across the switch.

**A function answer — state passing.** `PState.get` is
`k => s => k(s)(s)`: the answer is a function, and applying it applies
the rest inside its own frame, so a chain of n steps is n host frames
OUTSIDE the machine, where no switch can reach. `PState.get` and `set`
build their answer as a `PState.Bounce`: it hands back the next function
and its state instead of applying them, and its `apply` is the loop — a
million steps on a 128 KB thread, a hundred thousand on Scala.js, at the
same cost (statePara 1.00x). A state-passing body of your own is written
the same way, a `Bounce` with `next` and `arg`.

**A helper of your own that calls `k`:** the macro reads only what it can
see, and a plain `def`'s body is compiled elsewhere. Make the helper
`inline` (its body is then part of the shift body and is read like it),
or write it in Cont style — have it take the value and answer it, and
call `k` in the body yourself (`k(helper(x))` rather than
`helper(k, x)`). Reading a non-inline `def` from its TASTy tree would
need `-Yretain-trees` in every caller's build; it was not done.

```scala
val opaque = (1 to 20_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => Cont.shift[Int, Int, Int](k => try k(x + 1) catch { case _: ArithmeticException => 0 })))
Cont.reset(opaque) // 20000 — `k` under `try`: each level a frame; past the room the rest runs on a fresh stack
```

## How much room, per platform

The stack is COUNTED, never read: a fixed number of levels per stack,
then a switch. (Until 2026-10-01 a JVM 22+ with native access, and
Scala Native always, READ the stack pointer and granted more levels on
the caller's thread; the runner gave that up for one rule on every
platform — specs/cont-core.md, step 7.)

| platform | first room | what a switch costs |
|---|---|---|
| JVM 17+ | the VM's default thread stack over a cold level (1.2 KB), halved for the caller: ~870 levels on a 2 MB thread | one switch per first room on the caller's stack, then ~500 000 levels per segment: ~4 µs to hand off to a parked worker, ~0.01 µs a level after |
| Scala Native | 16 levels (a first room derived from the main thread's 8 MB would be wrong for every other thread) | as the JVM's |
| Scala.js | — no thread to switch to | **the bound**: nested bodies of the second kind are limited by the engine's stack (~10 800 frames on Node's default; `node --stack-size` raises it) |

## A body that calls `k` and answers a program: no nesting at all

When the answer type `S` is a PROGRAM (`Int ! Pure`, any `A ! F`) and the
body calls `k` itself — `k(1).flatMap(a => k(10).map(b => a + b))`, a
`val p = k(1)` used later — the macro gives the body a LAZY `k`
(cont-program-answer, 2026-10-02): `k(a)` returns at once, a `Delay`
holding a run of `k`'s rest that has not started. Whoever runs the
answer program starts it: the Free fold forces it (one bounded run,
whose answer is the program that goes on), and a running machine
(`Shift.run`, a handler on the machine) steps into it and continues in
its own loop. No nested run, so no switch and no bound — a million
nested such bodies run on a 128 KB JVM thread and on Scala.js, where the
strict `k` fails with "Maximum call stack size exceeded":

```scala
    val c = Cont.shift[Int, Ans, Ans](k => { val p = k(1); log += "body"; p.flatMap(v => k(v)) })
```

**The contract it changes:** host side effects written after `k(a)` in
such a body run BEFORE `k`'s rest (here `"body"` is logged before the
rest of the program runs), where a strict `k` ran the rest first. A body
that only PASSES `k` on (`perform(e).flatMap(k)`, a `foldM` step) is
unchanged: its calls already happen later, from the loop.

## The knobs

- `-Dokay.cont.room=N` — the levels the caller's stack is asked to hold
  before the switch (default: the VM's `ThreadStackSize` over 1.2 KB,
  halved; 16 on Native).
- `-Dokay.cont.idleWorkers=N` (2), `-Dokay.cont.idleMillis=N` (30 000)
  — parked workers kept for the next switch, and how long.
- `-Dokay.cont.spinMicros=N` (50) — how long a caller and a worker spin
  before parking, so a short segment costs no OS wake-up.

## The written bounds

- **A thread with an explicit stack smaller than the VM default** (say
  `new Thread(…, 256 KB)`): the count assumes the default size and may
  overflow before the switch. Set `-Dokay.cont.room` for such threads.
- **A level fatter than ~2.4 KB** (twice the cold constant): the first
  room is halved for exactly that margin; an opaque body whose own
  frames take more than that per level can overflow before the switch.
  Lower `-Dokay.cont.room` for such a program.
- **Scala.js**: the engine's stack, as above — for a body whose answer is
  not a program; one whose answer is a program has no bound (above).

## What it costs when it does not switch

Bookkeeping on a program that never goes deep: fib100 (a hundred
generator steps) reads 1.04x its pre-switch number in JMH, at the same
bytes per run (since cont-stack-fastpath round 3, 2026-09-26; it read
1.08–1.17x before, when a `map`'s continuation escaped the JIT's
escape analysis). A program built of `flatMap`s over a state answer —
`PState`, the statePara lane — read 1.11x with no switch and 21 more
bytes per operation until cont-stack-fastpath round 4 (2026-09-28)
counted them: one `java.lang.Long` per `get`, the state unboxed into a
lambda specialised to `Long` and boxed again, which the JIT had folded
away before. `get` and `set` now pass the state through as the object
it already is, and the lane allocates 12% less than master did and
less than it did before the switch existed (`specs/cont-stack.md`,
plan stage C). The lesson generalises to any shift over a primitive
state: a lambda written at an inline call site is specialised to the
caller's type, and a boxed value that crosses it is boxed again. A thousand-level program
that fits its stack pays one stack reading, 0.3 µs.

## Literature

- Danvy & Filinski, "Abstracting Control" (LFP 1990): `shift`/`reset`
  and the answer-type discipline `Cont[A, S, R]` keeps.
- Rompf, Maier & Odersky, "Implementing First-Class Polymorphic
  Delimited Continuations by a Type-Directed Selective CPS-Transform"
  (ICFP 2009): the compile-time road the macro takes — the tail body,
  then the selective transform of a body that uses the answer — and the
  same wall at code it cannot read.
- Pettyjohn, Clements, Marshall, Krishnamurthi & Felleisen,
  "Continuations from Generalized Stack Inspection" (ICFP 2005): capture
  by exceptions and re-entry — the road not taken (it replays).
- JEP 444 (virtual threads) and HotSpot's freeze/thaw: measured and
  refuted as the fresh stack (a segment capped at 64 levels by the
  humongous-chunk limit, 15–20x slower a level than a platform thread).
