# 9 · Composing the shapes

> Compiled in `src/test/scala/TestBookComposing.scala`, including one
> test that asserts something **does not compile**. This is the
> chapter that matters most in a real codebase, because a real
> codebase never wants exactly one shape.

---

## The four do not compose naively

Here is the mistake, and it is the obvious thing to write:

```scala
Delim.delimited[String, Pure]:
  direct:
    val x = !Delim.delimited[Int, Pure]:      // ← a second one, inside
      direct:
        !Delim.exit(7)
        0
    s"inner said $x"
```

Two boundaries, one inside the other. It reads correctly. It is wrong,
and the compiler says so:

```scala
val e = compileErrors("""
  okay.Delim.delimited[Int, okay.Delim + okay.Pure](okay.pure(1))""")
assert(e.contains("SECOND machine"))
```

**Why it is wrong.** Each of `delimited`, `collect` and `resumable`
does two jobs: it *installs a boundary* and it *runs the machine* that
interprets captures. A second one inside the first starts a second
machine — and a prompt lives in the machine that pushed it. An exit
aimed at the outer boundary from inside the inner machine cannot find
it, because it is looking at a different stack.

Getting that as a compile error rather than a runtime surprise is not
an accident; it is the reason the two halves have separate names.

## The rule, in one line

> **The outermost combinator runs the machine. Everything under it
> installs only.**

| runs the machine (outermost) | installs only (nested) |
|---|---|
| `Delim.delimited` | `Delim.scope` |
| `Delim.collect` | `Delim.collecting` |
| `Delim.resumable` | `Delim.pausing` |

Written correctly, the example above is:

```scala
Delim.delimited[String, Pure]:              // runs
  direct:
    val inner = !Delim.scope[Int, Pure]:    // installs
      direct:
        !Delim.exit(7)
        0
    s"inner said $inner"
// "inner said 7"
```

The exit leaves the **inner** boundary and hands `7` to the line that
asked for it. The outer block carries on.

## Crossing a boundary, which is the whole point

Nested boundaries are not just isolation. Because a prompt is a
first-class value, an inner scope can leave through an **outer**
boundary — the thing nested handlers cannot express:

```scala
Delim.delimited[String, Pure]: outer ?=>
  direct:
    val inner = !Delim.scope[Int, Pure]:
      direct:
        !Delim.exit(using outer)("straight out")   // not this boundary — that one
        0
    s"inner said $inner"
// "straight out"
```

The inner scope's own boundary is skipped. The value goes to the outer
one and becomes the answer of the whole thing.

This is what "multi-prompt" means in practice, and it is why the
boundaries are values with types rather than anonymous markers: to
cross one deliberately you must be able to *name* the one you mean.
Naming the wrong one is a real mistake — chapter 27 lists it — and
naming none of them is what `exit` does by default, which is why the
default is the nearest.

## Two shapes at once

The common case. A walk that emits, and stops early:

```scala
def upToBig(x: Tree)(using Delim.Emitting[Int], Delim.Prompted[Unit]): Unit ! Row =
  direct:
    x match
      case Tree.Leaf(n) =>
        if n > 50 then !Delim.exit(())
        !Delim.emit(n)
      case Tree.Node(l, r) => { !upToBig(l); !upToBig(r) }
```

Two pieces of evidence in one signature: *I emit ints* and *I can
leave a boundary*. Both are `using` parameters, and the walk is
otherwise ordinary.

Assembling it: `collect` is outermost (it runs), and a `scope` inside
it provides the boundary the exit aims at.

```scala
val got = !.run(Delim.collect[Int, Pure](
  direct:
    !Delim.scope[Unit, Pure](direct(!upToBig(t)))))
// List(1)
```

On the tree `Node(Node(Leaf(1), Leaf(99)), Leaf(3))` the answer is
`List(1)`:

- `1` was emitted and is **kept** — an exit discards the rest of the
  computation, not the effects that already happened;
- `99` triggered the exit and was never emitted;
- `3` was never reached.

That is worth pausing on, because it is the question everybody asks
about this combination: **what survives an early exit is everything
that already left the block.** The same walk without the exit gives
`List(1, 99, 3)`, and the suite asserts both so the difference is
documented rather than described.

## A hook sees an exit from a nested scope

Chapter 8 promised this, and here it is composed:

```scala
Delim.delimited[Int, Pure]:
  direct:
    !Delim.onReturn(n => n + 1000)
    val inner = !Delim.scope[Int, Pure]:
      direct:
        !Delim.exit(5)
        0
    inner
// 1005
```

The exit leaves the inner boundary with `5`; the block continues,
produces `5`, and that value leaves the outer boundary through the
hook. Timing, logging and compensation therefore survive early exits
from any depth — which is what makes them usable at all.

## How to get this right without memorising it

Three habits, in order of usefulness.

1. **Write the outermost one first, and only once.** If you are
   adding a boundary inside code that already has one, you want the
   nesting half (`scope`, `collecting`, `pausing`). The compiler will
   tell you if you are wrong, but knowing the rule makes the error
   unsurprising.

2. **Put the boundary around the smallest span that needs it.** Not
   only for cost (chapter 20 measures it) but for reading: a boundary
   marks the region where a capture can land, and a region that spans
   a whole file tells the reader nothing.

3. **If you must cross a boundary, name the one you mean.** Passing
   the outer `Prompted` explicitly, as above, makes the crossing
   visible at the point where it happens. An implicit search that
   silently picks the nearest is right by default and wrong exactly
   when you meant something else.

## What Part II established

Four shapes, each a few lines of structure around ordinary code:

| | shape | the door |
|---|---|---|
| 5 | leave early with an answer | `delimited` / `scope` + `exit` |
| 6 | a push producer read as a pull | `collect` / `collecting` + `emit` |
| 7 | stop in the middle, carry on later | `resumable` / `pausing` + `pause` |
| 8 | do something on the way back | `onReturn` |
| 9 | and how they nest | outermost runs, inner installs |

Part III opens the machine: what a prompt is, what the four captures
differ in, why one machine and one stack, and what it really means
that a continuation is a value you can call twice.

---

← [8 · Do something on the way back](08-on-the-way-back.md) ·
[Contents](index.md) ·
[10 · Prompts, and why they are first class →](10-prompts.md)
