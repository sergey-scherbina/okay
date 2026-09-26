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

**A body that only ever calls `k` last** — `k => k(v)`, also under
`if`/`match` and after statements that do not mention `k` — is the
value it passes, decided at compile time: `shift` is a macro, and such a
body becomes `pure(v)` evaluated when the runner reaches it. No frame,
no bookkeeping, no switch, on any platform. A million of them run on a
128 KB stack:

```scala
val deep = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1))))
reset(deep) // 1000000 — no frame per level: the body is the value it passes
```

**A body that uses the answer** — `k(x + 1) + 1`, `k(1) + k(10)`,
`a :: k(x)`, `s"${k(a)}"`, a block with `val a = k(1)`, an `if` or
`match` in tail position with calls in its branches — is CPS-transformed at compile time (since
`cont-stack-layer1-b`): each call of `k` becomes a step naming what is
left to do with its answer, and the runner keeps those pending parts on
an explicit stack of its own instead of the JVM's. Still no frame per
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
val used = (1 to 1_000_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => k(x + 1) + 1)))
reset(used) // 2000000 — no frame per level either: the pending `+ 1`s live on the runner's own stack
```

**A body the macro cannot read, or should not** — `k` handed to `map`
or any other function as a value, a call under a conditional that is
not in tail position (`1 + (if c then k(1) else 2)`), in a by-name
argument, under `try`, in a loop, in a lambda (`PState`'s
`s => k(s)(s2)`: a function answer walked measured 2.8x its direct
cost, so it is left direct on purpose), `k` passed into Java or an
abstract method, a body passed to `shift` as a value rather than a
literal — runs direct, and each level is a frame. The
runner counts the levels the current stack has room for; when the
count runs out it either *reads* how much stack is really left (below)
and continues here, or hands the rest of the program to a fresh stack —
a parked worker thread with a 1 GB stack — and waits for the answer. No
exception unwinds anything and nothing runs twice: the frames below
stay where they are until the answer comes back. Multi-shot bodies
keep working across the switch.

```scala
val opaque = (1 to 20_000).foldLeft(Cont.Pure[Int, Int](0): Int /> Int)((m, _) => m.flatMap(x => shift[Int, Int, Int](k => List(x + 1).map(k).sum)))
reset(opaque) // 20000 — `k` handed to `map`: each level a frame; past the room the rest runs on a fresh stack
```

## How much room, per platform

| platform | how the room is known | first look | what a switch costs |
|---|---|---|---|
| JVM 22+ **with** `--enable-native-access=ALL-UNNAMED` | exactly: the stack pointer and the thread's bounds through the FFM API (macOS arm64, and Linux aarch64 and x86_64 on glibc; macOS x86_64 and musl/Alpine count) | after ~870 levels on a 2 MB thread (1.2 KB a level, cold) — then the exact reading grants the rest | never, while the stack has room: a 1000-level program on a default thread switches **zero** times |
| JVM 17–25 **without** the flag (a library on a classpath, by default) | counted: the VM's default thread stack over a cold level, halved for the caller | ~870 levels | one switch per ~870 levels on the caller's stack, then ~500 000 per segment: ~4 µs to hand off to a parked worker, ~0.01 µs a level after |
| Scala Native | exactly, from the runtime's own thread info, always | 64 levels | as the JVM's |
| Scala.js | not at all: no thread to switch to | — | **the bound**: nested bodies of the second kind are limited by the engine's stack (~10 800 frames on Node's default; `node --stack-size` raises it) |

*Native access decides WHERE a deep program runs, not how fast.*
Measured on `HandlerBenchmark.statePara` (a state-passing program of
~2 000 levels, 2026-09-26, both roads on the same lane): with the flag
it never leaves the caller's thread, and it costs what the counted
road's one hand-off to a parked worker costs — 0.99x the time, +0.9%
the bytes, at the default first room. What the flag buys is the
caller's own thread for the whole run: its thread-locals, its stack
traces, no second thread involved. A library cannot enable it for you — a JVM
prints warnings on the first restricted call unless the launcher said
`--enable-native-access` — so the flag is yours to pass:

```
java --enable-native-access=ALL-UNNAMED -jar your-service.jar
```

Without it `okay` never touches the native API and never prints the
warning; it counts.

## The knobs

- `-Dokay.cont.room=N` — the levels the caller's stack is asked to hold
  before the first look (default: the VM's `ThreadStackSize` over
  1.2 KB, halved; 64 on Native).
- `-Dokay.cont.idleWorkers=N` (2), `-Dokay.cont.idleMillis=N` (30 000)
  — parked workers kept for the next switch, and how long.
- `-Dokay.cont.spinMicros=N` (50) — how long a caller and a worker spin
  before parking, so a short segment costs no OS wake-up.

## The written bounds

- **A thread with an explicit stack smaller than the VM default** (say
  `new Thread(…, 256 KB)`) on the counted road: the count assumes the
  default size and may overflow before its first look. Set
  `-Dokay.cont.room` for such threads, or enable native access, where
  the reading sees the real size.
- **One frame more than twice the fattest seen so far** within one
  64 KB slice of stack, on the exact road: the grant is sized to the
  worst level measured, with the slice's own margin absorbing a 2x
  overshoot; a body whose single frame is larger than that can still
  overflow.
- **Scala.js**: the engine's stack, as above.

## What it costs when it does not switch

Bookkeeping on a program that never goes deep: fib100 (a hundred
generator steps) reads 1.08–1.17x its pre-switch number in JMH (1.00
against the tree before the answer-using walk, which added nothing
fib100 can see), with an
exact allocation count identical byte for byte — the difference is the
JIT's escape analysis on a slightly bigger hot path, not objects
(`specs/cont-stack.md`, plan stage C). A thousand-level program that
fits its stack pays one stack reading, 0.3 µs.

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
