# java-effects — okay programs, effects and handlers from Java

## Overview

okay-java bridges the JDK's TYPES — `java.util.stream`, `java.util.function`,
gatherers, collectors — but a Java programmer could not write an okay
program, perform an operation of an effect of their own, or handle one.
This lane adds that facade, in `okay.java` (operator ask, 2026-10-04).

okay's row is a union of type constructors, `State % Int + Throws % E`.
Java has neither higher-kinded types nor union types, so the row cannot be
spelled there. okay-scala2 faced the same wall and stored its programs at
an erased row (`Rows.Top`) with the real row a phantom intersection
(`Eff[State[Int] with R, A]`). Java cannot do even that: an intersection is
legal only as a type-variable BOUND, never as a type argument. So the
Java facade keeps the erased storage and gives up the static row:

| | Scala 3 | okay-scala2 | Java |
|---|---|---|---|
| program | `A ! F` | `Eff[R, A]`, `R` phantom | `Eff<A>` |
| row checked | at compile time | at compile time | at `run()`, by name |
| an operation | `enum F[+A] derives Effect` | `extends Op[A]` + `Effect[F]` | `implements Op<R>` |
| split by | the class (`TypeableK`) | a `ClassTag` | a `Class<O>` |

What is NOT given up: every handler is the core's own machinery
(`Handler.answerOf`, `stateOf`, `intoOf`, `Effects.handle`), so the
operations split by class exactly as in Scala, and multi-shot resumption,
stack safety and the core effects behave identically.

## API

- `Op<R>` — a Java effect's operation answering `R`:
  `record Ask() implements Op<Integer> {}`. A sealed interface of records
  is one effect; a handler names the interface's class.
- `Eff<A>` — a program. `Eff.pure(a)`, `Eff.perform(op)`, `Eff.defer(() ->
  …)` (a tail call: deep recursion in Java is stack-safe through it),
  `map`, `flatMap`, `andThen`; `run()` with nothing left; `runAsync()`
  with only `Async` left (blocks this thread).
- Core effects: `Eff.ask()` (Reader), `Eff.get()`/`put(s)`/`modify(f)`
  (State), `Eff.raise(e)` (Throws), `Eff.async(supplier)`/`sleep(ms)`
  (Async); their handlers `Handler.reader(env)`, `StateHandler.state(s0)`,
  `p.recover(e -> …)`.
- Handlers, the core's four forms (specs/handler-forms.md) by power:
  1. `Handler.answer(Cls.class, op -> value)` — each operation answered;
  2. `StateHandler.of(Cls.class, s0, (s, op) -> Stated.of(s2, answer))` —
     a state threaded, the result `Eff<Stated<S, A>>`;
  3. `Handler.into(Cls.class, op -> eff)` — each operation a program in
     other effects;
  4. `Control.of(Cls.class, ret, (op, k) -> …)` — the continuation in
     hand: `k.resume(x)` once, twice or never.
  `p.handle(h)` is overloaded on the three handler classes, so Java's own
  overload resolution picks the result type.
- Scala side: `Eff.from(p: A ! F)` and `eff.toScala[F]` (a `Member[F]`
  checks each operation as it is performed and refuses a stranger by name).

## Behavior

- [x] a Java effect (sealed interface of records) performed and handled
      by `answer`; by `into`; by `state`; by `control`
- [x] control resumes TWICE (all answers of a `Flip`), and NEVER (an
      abort that answers the return clause's type)
- [x] an operation no handler takes: `run()` throws
      `IllegalStateException` naming its class — not the operation
      returned as a value (the core's `Answers[Pure]` would do that)
- [x] two Java effects in one program, handled in either order
- [x] core effects from Java: Reader, State, Throws (`recover`), Async
      (`runAsync`, `sleep`)
- [x] a Java effect and a core one in one program
- [x] stack safety: a Java loop of 1 000 000 `defer`-ed steps over State
- [x] Scala ↔ Java: a Scala `A ! F` handled by a Java handler, and a Java
      `Eff` run by Scala handlers via `toScala[F]`; a stranger refused

## Decisions

- **No static row.** See the overview: Java cannot write the type. The
  refusal is by NAME at run time, the same promise the Clojure and Frege
  bridges make for a foreign program (`Member`, interop-shared).
- **Answers are `Object` from Java.** A Java lambda cannot be polymorphic
  in the operation's answer type, so a clause answers `Object` and the
  answer reaches the performing site at `R` by erasure — a wrong answer is
  a `ClassCastException` at its use, as in any generic Java code. The one
  function that makes that claim is `Rows.answered`.
- **Three handler classes, not one generic.** A handler's result type is
  polymorphic in the program's (`A`, `(S, A)`), which Java generics cannot
  abstract over: `Handler` keeps `A`, `StateHandler<S>` makes
  `Stated<S, A>`, `Control<A, B>` fixes both at construction (its return
  clause names them).

## Results

- 12 tests in `TestJavaEffects`, every program written in
  `JavaEffects.java` (Java 17 source: records, sealed interfaces,
  `instanceof` patterns). All Java sources compiled on the first javac run;
  the only fixes were on the Scala side of the test.
- Mutant: `run()` on the core's `Answers[Pure]` instead of the refusing
  one — the refusal test fails with a `ClassCastException` where the
  `IllegalStateException` naming `Counter$Next` belongs.
- Not done, on purpose: no Java-side `Choose`/`Writer`/concurrency
  (`par`, `race`) — a Java effect plus `Control` already expresses them,
  and none was asked for. The control form allocates a capture per
  operation even when the clause resumes once in tail position (the core
  `Handler.control` elides that); a tail-resumptive Java effect should use
  `answer`/`state`/`into`, which are the core's fast loops.
