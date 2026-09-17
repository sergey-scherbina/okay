# 8 · Do something on the way back

> Compiled in `src/test/scala/TestBookOnTheWayBack.scala`. The
> smallest of the four shapes, and the one with the sharpest edge:
> two of its tests were written as *questions* and answered by running
> them, because the chapter must state the behaviour rather than imply
> it.

---

## The problem, again

From chapter 1: every request should be timed, and the timing recorded
**with the outcome** — including when the outcome is a failure.

`finally` runs on both paths and cannot see the result. Seeing the
result means being in the expression, which means running only on the
path you are on. The two halves you want live in different constructs,
so the code gets written twice.

## The shape

Register a function on the boundary. Whatever comes back passes
through it.

```scala
def handle(fail: Boolean, clock: () => Long): Outcome ! Pure =
  Delim.delimited[Outcome, Pure]:
    direct:
      val started = clock()
      !Delim.onReturn(o => o.copy(took = clock() - started))
      if fail then Outcome(500, -1) else Outcome(200, -1)
```

One registration, both paths. The success case and the failure case
are ordinary values in the block; each of them leaves through the hook
and comes out timed.

`onReturn` is registered *from the middle* — after `started` exists,
which is the point. It is not a wrapper you have to put around the
whole function before you know what you will need; it is a line you
write once you know.

## Two facts you must know, and a third that follows

These were open questions when this chapter was drafted. Each is now a
test.

### An early exit DOES pass through the hook

```scala
val r = !.run(Delim.delimited[Int, Pure]:
  direct:
    !Delim.onReturn(n => n + 100)
    !Delim.exit(7)
    1)
// r == 107
```

Chapter 5's exit discards the rest of the block — but the hook is on
the **boundary**, not in the block, so the value still leaves through
it. That is what makes the two shapes compose: you can leave early and
still be timed, logged, or compensated.

### A thrown exception does NOT

```scala
intercept[RuntimeException](
  !.run(Delim.delimited[Int, Pure]:
    direct:
      !Delim.onReturn(n => { hookRan = true; n })
      throw new RuntimeException("boom")))
// hookRan == false
```

**This is the sharp edge.** A Scala `throw` is not a value coming back
through the boundary; it unwinds past everything, including the
machine. The hook never sees it.

So if your requirement is "record the timing *even when it blows up*",
`onReturn` alone does not meet it. Your options, in the order worth
trying:

1. **Make the failure a value.** `Outcome(500, …)` rather than a
   throw. Then it passes through the hook like any other answer, and
   the requirement is met by the shape. This is usually the right
   answer, and it is why the example above returns a status rather
   than throwing one.
2. **Use `Resource`** for anything that must be released whatever
   happens. Its handler sits outside the machine and releases on both
   paths — including an abandoned continuation. Chapter 18 has the
   pinned behaviour.
3. **Catch at the boundary** if the exception is genuinely
   exceptional, and convert it to a value there.

What you must not do is assume. The rule in one line: **`onReturn`
sees values, not exceptions.**

### It follows that the hook is not `finally`

They overlap and are not the same:

| | `finally` | `onReturn` |
|---|---|---|
| runs on a normal return | yes | yes |
| runs on an early exit from the block | yes | yes |
| runs on a thrown exception | **yes** | **no** |
| can see the value coming back | **no** | **yes** |
| can change the value | no | **yes** |

Neither is a superset. Chapter 1's requirement — see the value *and*
run on both paths — is met by `onReturn` exactly when "both paths"
means two answers rather than an answer and a throw.

## Hooks nest, last registered applied first

```scala
!Delim.onReturn(s => s"outer($s)")
!Delim.onReturn(s => s"inner($s)")
"x"
// "outer(inner(x))"
```

The value walks outward through the hooks in reverse order of
registration, which is the order you would draw if you thought of each
as a wrapper placed around what follows it. If two hooks must run in a
particular order, register them in the reverse of that order — and,
better, do not put two on one boundary unless the nesting is the
point.

## The compensation shape

This is what people actually reach for it for. Halfway through a
procedure, once you know a thing has happened, register what to do
about the outcome:

```scala
def charge(amount: Int, ok: Boolean): String ! Pure =
  Delim.delimited[String, Pure]:
    direct:
      !Delim.onReturn(s => if s.startsWith("failed") then s"$s; refunded $amount" else s)
      if ok then s"charged $amount" else "failed: card declined"
```

- `charge(90, true)` → `"charged 90"`
- `charge(90, false)` → `"failed: card declined; refunded 90"`

The code that follows the registration is untouched by it: no `if`
about refunds, no flag, no second exit path. The compensation is
attached to the *boundary*, and every way of reaching the boundary
goes through it.

This generalises: **anything of the form "if this goes wrong later,
undo what I just did" belongs on a hook at the point where the thing
was done**, not in an error branch far below.

## When this is the wrong tool

- **You need it on a thrown exception.** See above — `Resource`, or
  make the failure a value.
- **It is one exit.** A line after the call is clearer.
- **The hook is doing real work.** A hook that logs, times, or folds
  is right. A hook that performs the next step of the business process
  is a control-flow decision hiding on a boundary; write it in the
  open.
- **You want it to run before the block.** That is a wrapper, not a
  hook. Write the wrapper.

## The recipe, condensed

```scala
Delim.delimited[Answer, Row]:
  direct:
    val context = ...                        // 1. whatever the hook needs
    !Delim.onReturn(a => transform(a))       // 2. register, from the middle
    ...                                      // 3. untouched code
    answer                                   // 4. leaves through the hook
```

Part II's four shapes are now all in hand. Chapter 9 is the one that
matters most in a real codebase: what happens when you need two of
them at once.

---

← [7 · Stop in the middle](07-stop-in-the-middle.md) ·
[Contents](index.md) ·
[9 · Composing the shapes →](09-composing.md)
