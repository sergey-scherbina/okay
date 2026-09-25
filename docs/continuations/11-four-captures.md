# 11 · Four captures: `shift`, `shift0`, `control`, `control0`

> Compiled in `src/test/scala/TestBookFourCaptures.scala`. Every claim
> below was **discovered by running a program**, not copied from a
> table — and one of them is that the difference is harder to observe
> than the names suggest.

---

## The practical answer first

**Use `shift`.** If you never read the rest of this chapter you will
be right almost every time, and Part II's four shapes are all built on
it.

The other three exist because the literature found them, they are
genuinely different, and once every few years somebody needs one. This
chapter is so that when you meet one in code you know what it changes.

## The two switches

The four are not four ideas. They are two independent yes/no
questions, and every combination has a name:

|  | the handler's body runs **under** the delimiter | the captured continuation **re-installs** the delimiter |
|---|---|---|
| `shift` | yes | yes |
| `shift0` | **no** | yes |
| `control` | yes | **no** |
| `control0` | **no** | **no** |

"Under the delimiter" means: while your handler runs, is the boundary
still on the stack — so that a capture *inside the handler* finds it?

"Re-installs" means: when you call `k`, does the boundary come back for
the duration of that call — so that a capture *inside the rest of the
program* finds it?

## On ordinary programs, all four agree

This is worth establishing first, because it is why the choice rarely
matters:

```scala
Delim.push(p)(capture(c, p)(k => k("a")).map(s => s"[$s]"))
// "[a]" for all four
```

and so does invoking the continuation twice:

```scala
capture(c, p)(k => k("a").flatMap(x => k("b").map(y => x + y)))
// "<a><b>" for all four
```

If your handler calls `k` zero, one or many times and does nothing
else clever, **the four are interchangeable**. Every recipe in Part II
is in this category.

## The first switch, seen

The difference appears when the handler body captures **again, to the
same prompt**:

```scala
Delim.push(p)(
  capture(c, p)(_ => capture("shift", p)(_ => okay.pure("inner-caught"))))
```

| | result |
|---|---|
| `shift` | `inner-caught` |
| `control` | `inner-caught` |
| `shift0` | **NoPrompt** |
| `control0` | **NoPrompt** |

`shift` and `control` leave the boundary in force while the handler
runs, so the second capture finds it. `shift0` and `control0` consume
it, so the second capture has nothing to aim at and says so.

**That is what the `0` means.** It is not "version 2"; it is "the
delimiter is used up". A handler that wants to re-signal *outward*
rather than be caught by its own boundary is what `shift0` is for —
which is exactly the shape of a handler that handles some cases and
passes the rest along.

## The second switch, seen — and not seen

Now a capture inside **the rest of the program**, after the first one
returns:

```scala
Delim.push(p)(
  capture(c, p)(k => k("a"))
    .flatMap(s => Delim.shift(p)(_ => okay.pure(s + "-second"))))
```

| | result |
|---|---|
| `shift` | `a-second` |
| `shift0` | `a-second` |
| `control` | `a-second` |
| `control0` | **NoPrompt** |

Only `control0` differs — and `control`, whose continuation is
supposed to be bare, behaves exactly like `shift` here.

**This is the honest part, and the suite asserts it rather than hiding
it.** The reason is a small piece of reasoning worth following: for
`control` the *handler body* runs under the delimiter, so when the
bare continuation is invoked from inside that body, the boundary is in
force anyway — from the body's context rather than from `k`. The
second switch is invisible because the first one is covering for it.

To see `delimitK` alone you would have to invoke the continuation
somewhere the delimiter is not otherwise present — store `k`, leave
the handler, call it later. That is a program almost nobody should
write, and if you are writing it you are past what this chapter can
tell you.

**So the practical taxonomy is not four, it is three:**

- `shift` — the handler is inside its own boundary, the continuation
  brings the boundary with it. **The default.**
- `shift0` / `control0` — the handler is outside its own boundary. Use
  when the handler must escape outward, typically to an enclosing
  handler of the same kind.
- `control0` further drops the boundary from the continuation, which
  is observable and is almost always a bug waiting to happen.

## How to choose, in practice

1. **Start with `shift`.** Change only when something forces you.
2. **If your handler must re-raise to an outer boundary of the same
   prompt, you want a `0` variant.** That is the one legitimate common
   reason to move.
3. **If you are choosing between `shift` and `control` for a reason
   you cannot state in one sentence, choose `shift`.** The difference
   will not show up in your program, per the table above, and if it
   ever does it will show up as a `NoPrompt` in production rather than
   as a wrong answer — which is the better failure, but still.

## A fifth word: `dollar`, the delimiter with a way out

The four captures differ in what they take. `dollar` differs in what
the DELIMITER does when its body finishes. Materzok and Biernacki's λ$
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
handler written as `ret $ body` with shift0 operations, and a shallow
one written with control0, are both indistinguishable from
`State.handle` by `Bisim.check` (see [equivalence](../equivalence.md)).
They are also 4x slower and allocate 7x more, which is why the
library's handlers keep their own loop. The encoding is a reference to
check a handler against, not a replacement for it.

Three rules to know. `shift` and `control` run their body under a PLAIN
delimiter, not under `ret` (λ$ defines `S k.e` as `S0 k.⟨e⟩`).
`control` and `control0` are refused at a `dollar`, because their bare
continuation answers the body's type rather than the prompt's; in
`Delim.Stacked` that refusal is a compile error (`control` asks for a
plain `Reset`), everywhere else it is the machine's. And `abort` to a
`dollar` SKIPS `ret`: an abort drops its continuation, and `ret` rides
inside the continuation, so `abort(p)("gone")` under `ret $ …` answers
`"gone"`, not `ret("gone")`. The measurements and the rest are in
specs/shift0-dollar.md.

The same door exists with the evidence in scope instead of a prompt in
hand, as `scope` does for `push`: `Delim.dollar(ret) { body }` makes a
fresh delimiter, runs `body` with `Prompted[R]` in scope, and leaves
through `ret`. Inside a `direct` block, `!Delim.shift0[A](k => …)` is
the one-type-argument spelling of the 0-capture, beside `!Delim.shift[A]`.

One more thing `ret` cannot tell you: how many times it was RESUMED.
A resumption that leaves the body by `abort` drops its continuation,
so `ret` never runs for it, yet the body did run again. A delimiter
that must refuse that (a handler keeping its state in a cell, as
`Lexical.tail` does) uses `Delim.dollarResumed(p)(ret, resumed)(body)`,
and the machine calls `resumed(n)` each time it enters the delimiter
with `n` counting the runs of one captured context. `n > 1` is a
second resumption, whether or not the first returned.

## Why the library offers all four

Two reasons, and neither is completeness for its own sake.

**They cost nothing to provide.** All four are one constructor with
two booleans — `Capture(p, f, underPrompt, delimitK)`. Offering three
of them and hiding the fourth would be an opinion enforced by
omission.

**The literature names them**, and somebody reading Danvy & Filinski
or Felleisen alongside this library should find the words they expect.
A library that renames the standard vocabulary makes its users
translate.

## References

- <a id="ref-dollar-2012"></a>Marek Materzok, Dariusz Biernacki. *A
  dynamic interpretation of the CPS hierarchy.* APLAS 2012, LNCS 7705.
- <a id="ref-pps-2019"></a>Maciej Piróg, Piotr Polesiuk, Filip
  Sieczkowski. *Typed equivalence of effect handlers and delimited
  control.* FSCD 2019 (LIPIcs, article 30).

---

← [10 · Prompts](10-prompts.md) ·
[Contents](index.md) ·
[12 · One machine, one prompt stack →](12-one-machine.md)
