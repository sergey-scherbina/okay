# `Delimited`: the continuation machine as an interface

Every control operator in okay — `reset`, `shift`, `shift0`, `abort`,
`$`, the handlers of `Lexical`, `Cps` — is built from four
primitives. They are a trait, `Delimited[M]`, and the frame machine is
one implementation of it. This is Dybvig, Peyton Jones and Sabry's
design \[[JFP 2007](#ref-dpjs-2007)\], in the variant λ$ calls for
\[[APLAS 2012](#ref-dollar-2012)\].

## The four primitives

| primitive | DPJS | what it does |
|---|---|---|
| `delimiter` | `newPrompt` | a fresh name for a delimiter |
| `dollar(d)(ret)(body)` | `pushPrompt` | run `body` under `d`; its value goes through `ret`, outside `d` |
| `shift0(d)(f)` | `withSubCont` | capture the stack up to `d`, `d` included; `f`'s answer takes `d`'s place |
| `resume(k)(m)` | `pushSubCont` | run the computation `m` inside the captured `k` |

## One door into the machine

Running is part of the interface too, and it is the ONLY way into the
machine: `runHead(m)` runs `m` to its head form — a value, or the first
operation the machine does not answer (an operation of the program's
own signature, or a capture to a delimiter `m` does not hold, which
leaves for the run outside), with the rest of `m` as its continuation.
`run(m)` is `runHead` under a boundary, where such a capture is
`NoPrompt`. `runHeadAt(k)(a)` is the same door entered from a captured
`k`: `runHead(k(a))` without building `k(a)`, which is how a strict `k`
resumes. The machine's loop itself is closed: `Cps`'s strict `k`,
`Shift`'s nested runs and `Stacked` all enter through `runHead` (or its
lazy form, the machine's `owned`), so the reference implementation —
whose `runHead` is the program itself — checks exactly the door every
caller uses. `Delimited` is also a `ParaMonad` (Atkey's order, the value
first): `pure[A, R]`, and `flatMap` is `bind`.

`reset`, `shift` and `abort` are written once, in the trait, over these
four. One difference from DPJS: their capture leaves the delimiter out
of `k` (it is `control0`); ours keeps it in, together with `ret`,
which is what λ$'s `shift0` does and what a deep handler's return
clause needs.

## Writing against the interface

A program that only needs delimited control can be written against
`Delimited[M]` and run on any instance. Here `k` is called twice, and
the delimiter's `ret` runs once per call:

```scala
val body: Str = D.shift0[String, String, String, String, String](p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
assertEquals(D.run(D.dollar[String, String, String, String](p)(s => D.pure(s"<$s>"))(body)), "<a!><b!>")
```

`Delimited.machine[F]` is the frame machine (stack-safe, the one the
library runs on). The test suite has a second instance, a reference
interpreter over a plain list of frames, and runs the same programs on
both: they must agree.

## Resuming with a computation

`k(a)` puts a VALUE into the captured stack. `resume(k)(m)` runs a whole
computation there, so `m`'s own captures see the delimiters `k`
carries. Below, `q` is installed inside the captured context; nothing
outside names it, yet the computation handed to `resume` captures to
it and resumes that inner continuation twice:

```scala
val inside: Str =
  D.shift0[String, String, String, String, String](q)(k2 => k2("y").flatMap(a => k2("z").map(b => a + b)))
val prog: Str =
  D.reset[String, String, String](p)(
    D.dollar[String, String, String, String](q)(s => D.pure(s"<$s>"))(
      D.shift0[String, String, String, String, String](p)(k => D.resume(k)(inside))))
assertEquals(D.run(prog), "<y><z>")
```

This is DPJS's "throwing into a continuation": the same shape with an
`abort(q)` in place of `inside` leaves `q` from inside `k`.

## References

- <a id="ref-dpjs-2007"></a>R. Kent Dybvig, Simon Peyton Jones, Amr
  Sabry. *A monadic framework for delimited continuations.* Journal of
  Functional Programming 17(6), 2007.
- <a id="ref-dollar-2012"></a>Marek Materzok, Dariusz Biernacki. *A
  dynamic interpretation of the CPS hierarchy.* APLAS 2012, LNCS 7705.

Spec: specs/delimited.md. The machine itself: specs/cont-core.md.
