# 5 · Leave early with an answer

> **Part II is the recipes.** Four chapters, one shape each, and a
> fifth on making them nest. Every runnable snippet from here on is
> compiled: this chapter's code lives in `src/test/scala/TestBookLeaveEarly.scala`,
> in the same order and under the same names, so the page cannot drift
> from the library without the gate going red.

---

## The problem, again

Chapter 1 left this one unfinished. An invoice has lines; lines have
allocations; allocations name a cost centre. **If any cost centre is
closed, the whole invoice is rejected, naming that centre.**

```scala
final case class Alloc(centre: String, amount: Int)
final case class Line(sku: String, allocations: List[Alloc])
final case class Invoice(id: String, lines: List[Line])
final case class Rejected(centre: String)
```

Two nested walks, one decision that ends both, and an answer that has
to travel out.

## The shape

```scala
def check(inv: Invoice): Option[Rejected] ! Pure =
  Delim.delimited[Option[Rejected], Pure]:
    direct:
      for line <- inv.lines; alloc <- line.allocations do
        if closed(alloc.centre) then !Delim.exit(Some(Rejected(alloc.centre)))
      None
```

Three things are happening, and only the middle one is new.

**`Delim.delimited[R, F]` installs the boundary.** It says: *this
block is where an exit lands, and what comes out of it is an `R`.*
The type is the contract — a reader sees `Option[Rejected]` and knows
what the block can produce, whether it finishes normally or leaves
early.

**`!Delim.exit(value)` leaves.** Everything between the exit and the
boundary is discarded — the rest of the allocation loop, the rest of
the line loop, and the `None` at the bottom. Not skipped conditionally:
*discarded*, because the rest of the block was the thing the exit did
not call.

**`direct:` and the `!`** are this library's direct style: inside a
`direct` block, `!` runs an effectful expression where it stands.
Without it you would be writing `flatMap` by hand. It is not part of
the continuation idea; it is how this library lets you write one
without nesting lambdas.

## What it replaces

The usual version — and this is the honest comparison, not a straw
man, because it is what the same test asserts agrees with it:

```scala
def checkThreaded(inv: Invoice): Option[Rejected] =
  def line(l: Line): Option[Rejected] =
    l.allocations.foldLeft(Option.empty[Rejected]):
      case (found @ Some(_), _) => found
      case (None, a) => if closed(a.centre) then Some(Rejected(a.centre)) else None
  inv.lines.foldLeft(Option.empty[Rejected]):
    case (found @ Some(_), _) => found
    case (None, l) => line(l)
```

It is correct. Read the two and notice what is different:

| | the exit version | the threaded version |
|---|---|---|
| the loops | are loops | became folds |
| "have we finished?" | asked nowhere | asked in every iteration, twice |
| the failure type | appears once, in the signature | appears in both helpers and both accumulators |
| adding a second failure mode | one more `exit` | every accumulator and both `case`s change |

The second column is not bad code. It is code whose **shape was chosen
by a limitation** rather than by the problem, and that is the thing to
learn to see.

## It really does stop

Worth checking rather than assuming, and the suite checks it:

```scala
var seen = 0
// ...count each allocation as it is examined...
assertEquals(!.run(counting(invoice)), Some(Rejected("CC-OLD")))
assertEquals(seen, 4)          // the fifth allocation was never reached
```

Four allocations examined, the fifth never touched. The exit did not
set a flag that later iterations consult — there were no later
iterations.

## Three things to know before using it

**It is not an exception.** It cannot be caught by a `catch`, it does
not unwind through `finally` the way an exception does, and it does
not appear in a stack trace. It is an ordinary value leaving through a
boundary you declared. The suite pins the consequence that surprises
people:

```scala
val r = Delim.delimited[Int, Pure]:
  direct:
    !Delim.exit(1)
    ranAfter = true      // unreachable: the rest was discarded
    2
// r is 1, and ranAfter is false
```

Code after an exit does not run. Obvious when written like this;
less obvious when the line after the exit is a `logger.info` or a
counter increment twenty lines down. **If something must happen on the
way out, it cannot be a statement after the exit** — that is chapter
8's shape, and chapter 18 lists what else changes.

**The boundary is where you put it.** An exit goes to the *nearest
enclosing* `delimited`. If you nest them — chapter 9 — you get to
choose which one, and choosing wrongly is a real mistake with a real
name (chapter 25, group B).

**The type of the boundary is the contract.** `delimited[Option[Rejected], Pure]`
says everything a caller needs: this produces an `Option[Rejected]`,
and it needs no effects (`Pure`) to do it. A reader does not have to
find the exits to know what can come out.

## When this is the wrong tool

From chapter 4, applied here:

- **One layer.** If the decision and the caller are adjacent, an
  `Either` and a `return` are clearer to more people.
- **A combinator fits.** If the whole walk is
  `inv.lines.flatMap(_.allocations).find(a => closed(a.centre))`, then
  write that. It is shorter than either version above, and this
  chapter's example is only interesting because real walks stop being
  expressible that way — a recursive descent over four node types,
  two different failure values, an early exit from inside somebody
  else's callback.
- **You wanted an effect.** If what leaves the block is a *report* to
  be collected rather than a decision that ends the walk, you want
  `Writer` or an error effect, not an exit.

## The recipe, condensed

```scala
Delim.delimited[Answer, Row]:          // 1. name the boundary and its type
  direct:
    ...                                 // 2. ordinary code, however deep
    if condition then !Delim.exit(a)    // 3. leave, with the answer
    ...
    fallback                            // 4. what it means to finish normally
```

Four lines of structure around code that is otherwise untouched. That
is the whole shape, and it is the smallest of the four.

---

← [4 · Deciding](04-deciding.md) ·
[Contents](index.md) ·
[6 · A push producer, read as a pull →](06-push-as-pull.md)
