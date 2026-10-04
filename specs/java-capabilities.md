# java-capabilities — a static row for Java, as parameters

## Overview

java-effects gave Java `Eff<A>`: okay programs, effects and handlers, but
no static row. Java cannot write `State % Int + Throws % E`, and not even
okay-scala2's phantom intersection, since Java allows an intersection only
as a bound. A missing handler was therefore a refusal at `run()`.

Java CAN write a method's parameters, and those become the row here. A
handler makes a CAPABILITY and hands it to its body. An operation is
performed THROUGH the capability, and the handler that made it takes
exactly the operations tagged with it:

```java
static Eff<Integer> two(Counter c) { ... c.next() ... }   // its row: Counter

Cap.answer(CounterOp.class, op -> 21, c -> two(new Counter(c)));   // the handler gives the Counter
```

A program that needs an effect does not compile without the capability,
and a capability exists only inside the handler that made it. So a
program built from capabilities alone has nothing left to refuse at
`run()`. This is capability-passing style: Brachthäuser, Schuster and
Ostermann, "Effects as Capabilities: Effect Handlers and Lightweight
Effect Polymorphism" (OOPSLA 2020), and Effekt as a Scala library
(Brachthäuser, Schuster, Ostermann, JFP 2020), which is the same move in a
language with no effect types.

## API

- `Cap<O>`: `perform(op)` an operation of `O` through it, refused at once
  if `op` is not an `O`.
- `Cap.answer` / `state` / `into` / `control`: the four forms of
  java-effects, each with a `body` taking the capability it makes. A
  capability is fresh for each RUN of the handler.
- Built in: `Var<S>` (`get`/`put`/`modify`, `Var.run(init, body)`),
  `Env<E>` (`ask`, `Env.run(env, body)`), `Raise<E>` (`raise`,
  `Raise.recover(onError, body)`, typed `E`), and `Io` (`async`/`sleep`,
  `Io.run(main)`, the platform's Async).
- A user's effect gets a typed face as a record over its capability:
  `record Counter(Cap<CounterOp> cap) { Eff<Integer> next() { … } }`.

## Behavior

- [x] each of the four forms through a capability, its program's row a
      parameter
- [x] TWO instances of one effect, nested, each answering its own:
      `Cap.answer(Ask, 1, a -> Cap.answer(Ask, 2, b -> …a… …b…))` answers
      12, not the 22 a class test gives (the mutant)
- [x] two `Var<Integer>` in one program, and a `Var<Integer>` beside a
      `Var<String>`
- [x] Env + Var + Raise in one program; `recover` keeps the state the
      program reached
- [x] `Io.run`: sleep then async
- [x] multi-shot `control` through a capability (every flip)
- [x] an escaped capability, used after its handler is gone, is refused by
      name ("escaped its scope"), also when a NEW handler of the same
      effect surrounds the use
- [x] 1 000 000 `Var` steps by Java recursion, constant stack

## Decisions

- **Identity, not class.** The test a capability's handler splits by is
  `tagged.cap eq cap`, the same cost as a class test (one load, one
  compare). It is what makes two instances of an effect work. Every row is
  distinct by construction, so `Distinct.unchecked` is now literally true.
- **A fresh capability per run.** It is made in a `Free.delay`, so a
  handler value run twice, or recursively nested, never shares one.
- **Escape is caught at the top, not at the operation.** An operation
  through a dead capability matches no handler, since no live one has its
  identity, and reaches `run()`. `run()` recognises the tag and says the
  capability escaped. That costs nothing on the hot path. The
  alternative, a per-capability `open` flag checked at every perform,
  would add a node per operation and say nothing more.
- **The old untyped statics stay.** `Eff.perform`, `Eff.get()`,
  `Eff.async()` and the class-based handlers still work. The static
  guarantee holds for programs that reach effects through capabilities
  only. Everything ambient (`Eff.async`) is still refused by name, as
  before.
- **Not checked by a compiler test.** "Does not compile without the
  capability" is Java's own rule for a missing argument. A test would
  only test javac.

## Results

- 11 tests in `TestJavaCapabilities`, every program in
  `JavaCapabilities.java` (Java 17 source), green beside
  `TestJavaEffects`' 12, which are unchanged.
- Mutant: the capability's test made a class test (`case t: Tagged =>
  true`). Four tests fail: nested instances (22 instead of 12), two Vars,
  Env+Var+Raise (a ClassCastException, `Env$Ask` taken by `Raise`'s
  handler), and an escaped capability taken by a new handler.
