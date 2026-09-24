# 14 · A new effect from a prompt

> **Part IV is about building.** Compiled in
> `src/test/scala/TestBookNewEffect.scala`. This chapter pays off the
> promise made in chapter 4: *if it can be an effect it should be —
> and the mature use of a capture is to write one once, in a library,
> so that everybody else gets an effect.*

---

## What we are making

A **budget**. A block is given an amount to spend; code inside it
spends; if it overspends, the block ends immediately with a fallback
and the rest is discarded.

The call sites should read like this, and this is the whole test of
whether the exercise worked:

```scala
def shop(using Budget[String]): String ! Row = direct:
  !Budget.spend[Pure](30)
  !Budget.spend[Pure](30)
  !Budget.spend[Pure](30)
  s"bought three, ${Budget.remaining} left"
```

No prompt. No `k`. No `shift`. Nothing about continuations except the
row they were already in. A reader who has never opened this book can
maintain that function.

## The library, in full

Three pieces, and none of them is long.

**The evidence** — what a budgeted block hands to the code inside it:

```scala
final class Budget[R] private (val left: AtomicInteger,
                               val orElse: () => R,
                               val prompt: Prompt[R]):
  type Res = R
```

Note `type Res = R`. That is the trick `Prompted` uses, and it is
worth understanding rather than copying: with the answer type as a
**member**, a caller can write `Budget[?]` and still name the type
when it matters (`b.Res`). Without it, `spend` would need the answer
type as an explicit argument at every call site — and every extra type
argument at a call site is a reason somebody reaches for the untyped
version instead.

**The boundary** — installs the prompt, makes the evidence, runs:

```scala
def within[R, F[+_]](limit: Int)(orElse: => R)
                    (body: Budget[R] ?=> R ! Delim + F)
                    (using Delim.OneMachine[F], At): R ! F =
  val p = Delim.prompt[R]
  Delim.run(Delim.push(p)(body(using new Budget(AtomicInteger(limit), () => orElse, p))))
```

This is the only place that mentions `Delim.run`, and it takes
`OneMachine[F]` — chapter 12's rule, applied: *a library that starts a
machine passes the obligation on to its caller.*

**The operation** — the only thing users write:

```scala
def spend[F[+_]](n: Int)(using b: Budget[?], at: At): Unit ! Delim + F =
  if b.left.addAndGet(-n) >= 0 then okay.pure(())
  else Delim.abort[b.Res, Unit, F](b.prompt)(b.orElse())
```

Spend and carry on, or leave the block with the fallback. `b.Res` is
what makes the prompt and the fallback agree **without a cast** — an
earlier draft of this chapter had an `asInstanceOf` there, which is
the sort of thing a type member exists to remove.

And a read-only question, to show that not everything has to be a
capture:

```scala
def remaining(using b: Budget[?]): Int = b.left.get
```

## It behaves

```scala
Budget.within[String, Pure](100)("over budget")(shop)
// "bought three, 10 left"
```

and over the limit, with a flag proving the tail did not run:

```scala
def greedy(using Budget[String]): String ! Row = direct:
  !Budget.spend[Pure](60)
  !Budget.spend[Pure](60)        // 120 > 100: leaves here
  reached = true
  "never"
// "over budget", and `reached` is false
```

## The three decisions that made it an effect rather than a capture

This is the transferable part. Anything you build this way faces the
same three.

**1. Who holds the prompt.** Not the user. It goes inside the
evidence, which is a `using` parameter, so the user cannot see it,
cannot store it, and cannot aim at it by accident. The evidence is
*also* the capability: a function whose signature says
`using Budget[String]` is declaring, in public, that it may spend and
may leave.

**2. What the operation is named.** `spend`, not `abortIfOver`. The
name belongs to the user's domain, not to the mechanism. The moment an
operation is called something like `exitWith`, the abstraction has
leaked and the user starts reasoning about control flow instead of
about budgets.

**3. Which parts are not captures at all.** `remaining` is a plain
read. It would have been easy to route everything through the
mechanism for uniformity; it would also have made a simple question
cost a capture. **Use the mechanism only where the control flow is the
point.** Chapter 4 said it about choosing; it applies inside a library
too.

## What the user cannot do wrong any more

Worth listing, because this is the actual return on the work:

- They cannot **aim at the wrong boundary**: there is only one, and
  they never name it.
- They cannot **start a second machine** by accident: `within` is the
  only door, and it holds `OneMachine`.
- They cannot **leave the prompt behind and use it later** — chapter
  10's open edge — because they never hold it.
- They cannot **forget to install the boundary**: without a
  `Budget[R]` in scope, `spend` does not compile.

The mechanism's sharp edges have been absorbed by the library, which
is the entire argument for putting them there.

## The polish this example skips

Two, so nobody copies the example and wonders.

`spend[Pure](30)` names the row. The doors in `Delim` itself do
better — `!Delim.exit(value)` needs no type argument at all — by
taking the direct block's own colouring as evidence
(`DirectCtx[F]` and `Reader.RowOf[F]`, both visible in `Delim.exit`'s
signature). It costs an inline definition and some plumbing, and it is
what you do when an operation graduates from useful to used
constantly.

The fallback here is a `() => R` evaluated at the moment of
overspending. A real budget would want the amount that overflowed, the
running total, perhaps a partial result — all of which are ordinary
parameters to the fallback, and none of which change the shape.

## The general recipe

```scala
// 1. evidence: what the block hands its body, with the answer type as a member
final class Cap[R] private (state, fallback, prompt: Prompt[R]):
  type Res = R

// 2. the boundary: the ONLY place that runs a machine, passing OneMachine on
def within[R, F[+_]](...)(body: Cap[R] ?=> R ! (Delim + F))
                    (using Delim.OneMachine[F], At): R ! F

// 3. the operations: named in the user's vocabulary, hiding the capture
def op[F[+_]](...)(using c: Cap[?], at: At): A ! (Delim + F)
```

Chapter 15 applies exactly this recipe to the oldest problem in the
book — an error mechanism whose handler may put the failing
computation back to work.

---

← [13 · Multi-shot](13-multi-shot.md) ·
[Contents](index.md) ·
[15 · Resumable exceptions →](15-resumable-exceptions.md)
