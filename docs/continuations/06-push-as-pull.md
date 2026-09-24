# 6 · A push producer, read as a pull

> This chapter's code is compiled in `src/test/scala/TestBookPushAsPull.scala`.
> It is also the chapter with the clearest ceiling: the shape here
> solves half of chapter 1's second problem, and says so. The other
> half is chapter 7.

---

## The problem, again

Something produces items by calling you: a tree walk, a parser, a
scanner, a visitor. It holds the loop; you hold a function it calls.

```scala
def walkCallback(e: Entry, prefix: String = "")(onFile: String => Unit): Unit =
  e match
    case Entry.File(n, _) => onFile(s"$prefix$n")
    case Entry.Dir(n, es) => es.foreach(walkCallback(_, here)(onFile))
```

Perfectly good code. The trouble is not in the producer — it is in
what the *consumer* has to become.

## What the callback does to the consumer

Inside `onFile` you are not writing ordinary code. You are writing a
function that will be called, which means:

- **no local control flow.** You cannot `return` from the enclosing
  method, cannot `break`, cannot stop the walk, cannot decide "I have
  enough".
- **state has to escape into a mutable cell.** A `List.newBuilder`, a
  `var`, an accumulator declared outside and written from inside.
- **and the two are related**: because the consumer is inside-out, its
  variables must outlive it.

That is the tax. It is small for one call site and it is why most
people pay it without noticing.

## The shape

The producer says `emit` and nothing else; the boundary collects:

```scala
def walk(e: Entry, prefix: String = "")(using Delim.Emitting[String]): Unit ! Row =
  direct:
    e match
      case Entry.File(n, _) => !Delim.emit(s"$prefix$n")
      case Entry.Dir(n, es) =>
        val here = if n == "/" then "/" else s"$prefix$n/"
        for child <- es do !walk(child, here)
```

and the consumer is ordinary code again:

```scala
val files = !.run(Delim.collect[String, Pure](walk(tree)))
// List("/a.txt", "/src/Main.scala", "/src/Util.scala", "/b.log")
```

Three things to see.

**The producer did not get harder.** Compare it with the callback
version: same recursion, same structure. `emit` replaced `onFile`, and
it takes no parameter for "where to send this" — the boundary decides.

**The consumer got its control flow back.** `files` is a list, in a
local, in a method that can `return`, loop, or stop. No cell outlives
anything.

**`Delim.Emitting[String]` in the signature is the contract.** The
producer declares *I emit strings*, and cannot be called except under
a boundary that accepts them. A `using` parameter rather than a
lambda, which is what makes the recursive call read as `!walk(child, here)`
rather than threading a function through every level.

## The comparison, and why it is fair

The suite asserts the callback version and this one produce the same
list. Not an implication — a test:

```scala
val buf = List.newBuilder[String]
walkCallback(tree)(buf += _)
assertEquals(buf.result(), !.run(Delim.collect[String, Pure](walk(tree))))
```

So the argument is not about correctness. Both are correct. It is
about which one leaves the consumer able to be ordinary code.

## The ceiling, stated plainly

**`collect` runs the producer to the end.** Every item, always. The
suite pins it with a counter:

```scala
val all = !.run(Delim.collect[String, Pure](counting(tree)))
assertEquals(all.size, 4)
assertEquals(emitted, 4, "collect did not run the whole producer")
```

So `collect`/`emit` does **not** solve chapter 1's version of this
problem — "the first ten tokens of a two-gigabyte file". It builds the
whole list. Against a `toList` over a callback it has bought you the
consumer's control flow, and nothing about memory.

If you need to stop early, or to take items one at a time, or to read
two producers in step, this is not the shape. That is the **pause**,
chapter 7, where the producer stops between items and hands you the
rest of itself as a value. This chapter's walk appears there again,
pulled one file at a time.

Why have both, then? Because most producers are small and most
consumers want all the items; `collect` is four lines and no ceremony,
and reaching for the heavier shape when this one fits is chapter 4's
first mistake.

## When this is the wrong tool

- **The producer is already lazy.** If it hands you a `LazyList`, an
  `Iterator` or a `Stream`, you have what this shape produces, with
  laziness as a bonus. Use it.
- **You need memory bounds.** See the ceiling above. Chapter 7.
- **The producer really is concurrent** — reading a socket, running
  ahead of you. Then you want a fibre and a channel. What this chapter
  removes is a thread used as a *control-flow device*; a thread used
  as concurrency is not a workaround for anything.
- **One call site, one consumer, three lines.** A builder and a
  callback are fine. Chapter 4 said this and it keeps being true.

## The recipe, condensed

```scala
// the producer: declare what it emits, and emit
def produce(...)(using Delim.Emitting[A]): Unit ! Delim + F =
  direct:
    ...
    !Delim.emit(item)
    ...

// the consumer: install the boundary, get the items
val items: List[A] = !.run(Delim.collect[A, F](produce(...)))
```

Two lines of structure, and the recursion in between is untouched.

---

← [5 · Leave early](05-leave-early.md) ·
[Contents](index.md) ·
[7 · Stop in the middle, carry on later →](07-stop-in-the-middle.md)
