# java-direct-effects — checked exceptions and JVM continuations (probe)

## Overview

A RESEARCH probe (operator, 2026-10-06), with no API. The question: can
Java have DIRECT-style effects (no `Eff`, no `flatMap`, plain loops and
locals) by combining two things the JVM already has?

- checked exceptions as the static row, where an effect is an exception
  type that is never thrown, only written in `throws`;
- the JVM's continuations for the resumption.

It follows java-effects (`Eff<A>`, no static row) and java-capabilities
(the row as parameters).

## Behavior

- [x] javac's thrown-type inference, pinned by compiling snippets
      (`TestJavaThrowsRow`, 9 cases through `javax.tools`)
- [x] one operation priced on three machines, one JMH lane each, a control
      for the slowest (`JavaDirectEffectsBenchmark`; history.d
      `java-direct-probe`)

## Results

### 1. The row: `throws` IS a checked effect row — up to one unknown

An effect is `class Counter extends Exception {}`. An operation is
`static int next() throws Counter`. A handler takes the effect off:
`<A, R extends Exception> A counter(Body<A, R> b) throws R`, where
`Body.run() throws Counter, R`. What javac says:

| case | javac |
|---|---|
| `next()` outside any handler | error: unreported exception Counter |
| handler, ONE other effect left (`log`) | compiles, `R` inferred = Log exactly |
| the same with the leftover not declared | error: unreported exception Log (the row is checked, not dropped) |
| TWO left (`log`, `put`), declared `throws Log, St` | error: unreported exception **java.lang.Exception**: `R` = lub |
| the same declared `throws Exception` | compiles, and the row has degraded to "anything" |
| two inference slots (`Body2<A, R1, R2>`) | the same lub error: every slot is bounded by every thrown type (JLS 18.2.5) |
| an explicit witness `Fx.<Integer, Log, St>counter2(…)` | compiles: a union cannot be INFERRED, but it can be WRITTEN |
| `xs.forEach(x -> next())` | error: `java.util.function` declares no `throws` |
| `try { next(); } catch (Exception e)` | compiles: a catch-all discharges the effect statically, and nothing handles it at run time |

So the row is exact for the common nesting, where each handler leaves
one unknown effect, and degrades to `Exception` beyond that unless the
caller writes the witness. The JDK's own functional interfaces refuse
effects even where they call synchronously. That is the
"exception transparency" JSR 335 dropped.

### 2. The resumption: one operation, three machines

`N` = 1000 operations, a tail-resumptive handler answering 1, JDK 26.
Every lane was gated quiet at both ends (`jmh-lane.sh`).

| machine | per operation | vs `Free` |
|---|---|---|
| okay `Free` (what `Eff`/`Cap` run on) | **14.3 ns** | 1.00 |
| `jdk.internal.vm.Continuation`, `yield`/`run` | 104 ns | 7.3x |
| virtual-thread body + virtual-thread handler (`vthreadBoth`) | 1.51 µs | 105x |
| virtual-thread body, platform handler thread (`vthread`) | 7.46 µs | 522x |

`vthread` is what a naive direct style gets: the caller's own platform
thread is the handler and parks in the OS twice per operation.
`vthreadBoth` is its control, a 5x improvement, and still two orders of
magnitude over `Free`. Each handoff goes through the virtual-thread
scheduler. The JVM's internal continuation is the only machine within
an order of magnitude.

## Verdict

- **Possible, and cheaper than expected on the right machine.** `throws`
  as the row with `jdk.internal.vm.Continuation` as the resumption costs
  ~7x `Free` per operation. That is the price of real direct style: no
  allocation per bind, ordinary Java control flow.
- **But it is a one-shot machine on an unsupported API.** It needs
  `--add-exports java.base/jdk.internal.vm=ALL-UNNAMED` from every user,
  it has no multi-shot resumption (Flip and search are impossible without
  replay), and an abort that drops a continuation runs none of its
  `finally` blocks (it needs a `discontinue`).
- **The public road (virtual threads) is not a road for effects.** 105x
  at best. It is a fine machine for `Async`, which is what okay already
  uses it for, and not for a per-operation handler.
- **The row's limit is the one-unknown rule.** Fine for one handler
  layer, a witness beyond.

Recommendation: no direct-style Java API now. `Eff` + `Cap` stay the Java
facade: every handler form, multi-shot, a static row, all on public API.
If a direct style is wanted later, the shape is the one measured here:
`throws` rows, an opt-in module over the internal `Continuation`,
one-shot forms only (answer, state, into, abort with discontinue).
`backlog.d/polyglot/java-direct-style.md` holds it with its trigger.

## Decisions

- **The probe compiles snippets with the JDK's compiler rather than
  quoting the JLS.** The claims about inference are what javac does today
  (JDK 25, the build's compiler). Rerun the suite on a new JDK to see
  whether a rule moved.
- **`LoomEffects` is Java**: the API is in a package `java.base` does not
  export. scalac's `-java-output-version` reads only exported API, and
  javac needs `--add-exports`. That is `compare`'s `javacOptions`, the
  one build.sbt line of the lane.
