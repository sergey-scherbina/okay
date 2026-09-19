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

---

← [10 · Prompts](10-prompts.md) ·
[Contents](index.md) ·
[12 · One machine, one prompt stack →](12-one-machine.md)
