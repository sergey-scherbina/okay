# 11 · Two captures: `shift` and `shift0`

> Compiled in `src/test/scala/TestBookFourCaptures.scala`. Every claim
> below was **discovered by running a program**, not copied from a
> table.

---

## The practical answer first

**Use `shift`.** If you never read the rest of this chapter you will
be right almost every time, and Part II's four shapes are all built on
it. `shift0` is for one shape, below.

## The one switch

The two differ on one yes/no question:

|  | the handler's body runs **under** the delimiter | the captured continuation **re-installs** the delimiter |
|---|---|---|
| `shift` | yes | yes |
| `shift0` | **no** | yes |

"Under the delimiter" means: while your handler runs, is the boundary
still on the stack — so that a capture *inside the handler* finds it?
Both continuations re-install the boundary when you call `k`, so a
capture *inside the rest of the program* finds it either way.

## On ordinary programs, both agree

This is worth establishing first, because it is why the choice rarely
matters:

```scala
Delim.push(p)(capture(c, p)(k => k("a")).map(s => s"[$s]"))
// "[a]" for both
```

and so does invoking the continuation twice:

```scala
capture(c, p)(k => k("a").flatMap(x => k("b").map(y => x + y)))
// "<a><b>" for both
```

If your handler calls `k` zero, one or many times and does nothing
else clever, **the two are interchangeable**. Every recipe in Part II
is in this category.

## The switch, seen

The difference appears when the handler body captures **again, to the
same prompt**:

```scala
Delim.push(p)(
  capture(c, p)(_ => capture("shift", p)(_ => okay.pure("inner-caught"))))
```

| | result |
|---|---|
| `shift` | `inner-caught` |
| `shift0` | **NoPrompt** |

`shift` leaves the boundary in force while the handler runs, so the
second capture finds it. `shift0` consumes it, so the second capture
has nothing to aim at and says so.

**That is what the `0` means.** It is not "version 2"; it is "the
delimiter is used up". A handler that wants to re-signal *outward*
rather than be caught by its own boundary is what `shift0` is for —
which is exactly the shape of a handler that handles some cases and
passes the rest along. It is also the more primitive of the two:
`shift` is `shift0` whose body runs under a fresh `reset` of the same
prompt, and that is how okay defines it.

## What happened to `control` and `control0`

The literature has a second switch: does the captured continuation
re-install the delimiter? Felleisen's `control` and `control0` say no —
`k` is the bare segment, and a capture inside it reaches past where
the delimiter was. okay offered all four until 2026-10-01. They left
with the cont-core-design lane, for three reasons:

- **Nothing used them.** No module of the library, only their own
  tests and one shallow-handler demonstration.
- **The core is λ$** — Materzok and Biernacki's calculus has exactly
  `$` and `shift0`. A bare continuation is not in it, and every rule
  of the machine that served it (a flag on the capture, a refusal at a
  `dollar` whose bare segment answers the wrong type, a second
  delimiter node so that the refusal could be typed) went with it.
- **The second switch was hard to see.** For `control` the handler
  body runs under the delimiter, so invoking the bare `k` from inside
  it finds the boundary anyway; only a stored `k`, invoked after the
  handler left, could tell `control` from `shift`.

If you need them, Kiselyov showed the dynamic operators are
expressible through the static ones \[[2005](#ref-kiselyov-2005)\],
and a shallow handler can be written over `shift0` in user code.

## The delimiter: `dollar`, with a way out

The two captures differ in where their body runs. `dollar` is about
what the DELIMITER does when its body finishes. Materzok and Biernacki's λ$
\[[APLAS 2012](#ref-dollar-2012)\] has one delimiter, `v $ e`: run `e`,
and when it returns `x`, leave the delimiter and continue with `v x`.
In that calculus a plain reset is only `(λx.x) $ e`. okay has it as an
operation of the machine:

```scala
def dollar[R0, R, F[+_]](p: Prompt[R])(ret: R0 => R ! Delim + F)(body: R0 ! Delim + F): R ! Delim + F =
```

This is not `push(p)(body).flatMap(ret)`. A `shift0` captures the
delimiter TOGETHER WITH `ret`, so `ret` runs once per resumption:

```scala
val p = Delim.prompt[String]
val body = shift0(p)(k => k("a").flatMap(x => k("b").map(y => x + y))).map(_ + "!")
assertEquals(run(dollar(p)(angle)(body)), "<a!><b!>")
```

With `flatMap` the answer would be `<a!b!>`, one `ret` around both
resumptions. A capture that drops `k` never runs `ret` at all. If this
reminds you of a handler, it should: `ret` is a deep handler's RETURN
CLAUSE, and shift0 is performing an operation. Piróg, Polesiuk and
Sieczkowski show the two are inter-definable with types
\[[FSCD 2019](#ref-pps-2019)\].

The body and the delimiter may answer different types, which is what a
return clause is for:

```scala
Delim.shift0[String, Int, Pure](p)(k => k(1).flatMap(a => k(2).map(b => s"$a|$b"))).map(_ * 10)
assertEquals(run(Delim.dollar[Int, String, Pure](p)(i => okay.pure(s"n=$i"))(body)), "n=10|n=20")
```

The correspondence runs as code in TestHandlersAsDollar. A deep State
handler written as `ret $ body` with shift0 operations is
indistinguishable from `State.handle` by `Bisim.check` (see
[equivalence](../equivalence.md)). It is also 4x slower and allocates
7x more, which is why the library's handlers keep their own loop. The encoding is a reference to
check a handler against, not a replacement for it.

Two rules to know. `shift` runs its body under a PLAIN delimiter, not
under `ret` (λ$ defines `S k.e` as `S0 k.⟨e⟩`). And `abort` to a
`dollar` SKIPS `ret`: an abort drops its continuation, and `ret` rides
inside the continuation, so `abort(p)("gone")` under `ret $ …` answers
`"gone"`, not `ret("gone")`. The measurements and the rest are in
specs/shift0-dollar.md.

The same door exists with the evidence in scope instead of a prompt in
hand, as `scope` does for `push`: `Delim.dollar(ret) { body }` makes a
fresh delimiter, runs `body` with `Prompted[R]` in scope, and leaves
through `ret`. Inside a `direct` block, `!Delim.shift0[A](k => …)` is
the one-type-argument spelling of the 0-capture, beside `!Delim.shift[A]`.

## References

- <a id="ref-dollar-2012"></a>Marek Materzok, Dariusz Biernacki. *A
  dynamic interpretation of the CPS hierarchy.* APLAS 2012, LNCS 7705.
- <a id="ref-pps-2019"></a>Maciej Piróg, Piotr Polesiuk, Filip
  Sieczkowski. *Typed equivalence of effect handlers and delimited
  control.* FSCD 2019 (LIPIcs, article 30).
- <a id="ref-kiselyov-2005"></a>Oleg Kiselyov. *How to remove a
  dynamic prompt: static and dynamic delimited continuation operators
  are equally expressible.* Indiana University TR 611, 2005.

---

← [10 · Prompts](10-prompts.md) ·
[Contents](index.md) ·
[12 · One machine, one prompt stack →](12-one-machine.md)
