# 19 · What a capture does to everything else

> **Part V is the limits** — the half that makes the rest
> trustworthy. Compiled in `src/test/scala/TestDelimLimits.scala`,
> which is this chapter's real text, plus
> `src/test/scala/TestBookCaptureAndTheRest.scala` for the `var` cases.
> Every answer below was a question nothing in the tree could answer
> until somebody ran it.

---

## Why this chapter exists

A capture does not only affect the code that captures. It changes what
happens to the state around it, the resources it holds, the `finally`
it is wrapped in, and the exceptions thrown through it. Those are the
questions that arrive on day two, after the first capture worked.

Every claim here is pinned by a test that fails if the behaviour
changes. That matters more than the claims: behaviour nobody tested is
behaviour nobody promised, and a book that described it from the
implementation would age into fiction.

## State: it depends on the handler order

Chapter 17's whole subject, in one line: a handler **outside** the
delimiter is shared by the branches; **inside**, each branch re-runs
it. Reread that chapter if this is news. It is first here because most
"my state did something strange" reports are this and nothing else.

## A plain `var` is not state, and has no order to choose

The contrast that makes chapter 17 land:

```scala
var seen = List.empty[Int]
Delim.delimited[Int, P]:
  direct:
    val x = !Delim.shift[Int, Int, P](k => direct { !k(1) + !k(10) })
    seen = seen :+ x
    x
// seen == List(1, 10)
```

Both branches wrote the same cell. There is no handler, so there is no
inside or outside — a `var` is one location on the heap, and every
branch that reaches it writes it. If you want per-branch state, an
effect is not a stylistic preference; it is the only thing that can
give it to you.

Two more facts about `var`s, both pinned:

- **The prefix runs once.** A `var` incremented *before* the capture
  point is incremented once, no matter how often the continuation is
  invoked. The continuation is the *rest* of the block, not the block.
- **A loop counter survives a pause.** A `var` inside the captured
  region is part of the continuation's closure, so pausing at
  iteration 2 and resuming later finds `i` and `acc` exactly as they
  were. This is the reassuring case, and it is why chapter 6's
  generators and chapter 23's workflows can be written as ordinary
  loops.

## Resources: they survive, and they accumulate

Three facts, and the third is a cost:

```scala
// an abandoned continuation still releases
!Delim.exit(r * 2)          // log: acquire, release
```

`Resource`'s handler is outside the machine, so dropping the rest of
the block does not leak: what was opened is closed.

```scala
// a multi-shot capture opens one per branch
// log: acquire1, acquire2, release2, release1
```

**Two branches hold two handles at once.** Releases are LIFO at the
end of the program, not between branches. A capture invoked *n* times
is *n* open resources, and if that resource is a database connection
from a pool of ten, a search with eleven branches deadlocks. This is
the single most expensive surprise in the chapter.

And the pair that explains both:

```scala
!Delim.exit(1)
cleaned = true      // never runs
```

A line of ordinary Scala after the capture point is just part of the
continuation that was dropped. `Resource` survives because it is an
effect with a handler outside the machine; your hand-written cleanup
line survives nothing.

> **The rule:** cleanup that must happen goes in an effect, never in a
> line of code after the thing that might not come back.

## `bracket` is refused, and `try`/`finally` is a compile error

Two shapes that would be wrong are not available:

```scala
okay.bracket[Int, Int, okay.Delim + okay.Pure](1)(_ => ())(r => okay.pure(r))
// error, mentioning: Handler
```

`bracket` runs its body to completion inside one suspension, which is
exactly what a capture breaks. It needs a `Handler` for the row, and
`Delim` has none.

```scala
try { !okay.Delim.exit(1); 0 } finally { closed = true }
// error, mentioning: finalizer
```

A finalizer around a mark is refused **at compile time**. This is the
design decision worth noticing: the dangerous mix is not documented
and warned about, it is unwritable.

## `try`/`catch` compiles — and catches nothing

The trap that survives review, so read it twice:

```scala
val prog: Int ! Async = direct:
  try !okay.async[Int](throw new RuntimeException("boom"))
  catch case _: RuntimeException => -1
// the exception escapes; the catch never fires
```

A `catch` in a `direct` block guards the **building** of the program.
The throw happens when the program is **run**, one stack away. The
block is a description; the `try` wrapped the description.

Handle failure with `Throws`, which is in the row and therefore in the
program. Its behaviour under a capture is also pinned: a raise from
inside a captured continuation reaches the handler; a handler that
raises instead of resuming leaves the rest of the block unrun.

## A second machine is a compile error, with one hole

```scala
okay.Delim.delimited[Int, okay.Delim + okay.Pure](okay.pure(1))
// error, mentioning: SECOND machine
```

One machine, one prompt stack (chapter 12). Nesting two used to be a
runtime `NoPrompt` and is now refused by the row guard.

The hole, stated by its own test and named `THE LIMIT`:

```scala
def generic[F[+_]](p: Int ! Delim + F): Int ! F = Delim.run(p)
```

A row-polymorphic helper with **no witness in its signature** compiles,
because `NotGiven` reads an unknown `F` as "absent". Instantiated at a
`Delim` row it throws `NoPrompt` at runtime. The fix available today
is for the helper to take `using Delim.OneMachine[F]`, which propagates
the obligation so its call site is refused instead. Stages 1 and 2 of
`specs/delim-safety.md` exist for this line, and stage 2 is open on
purpose — the cost is a type parameter on every signature carrying
evidence.

A book that only showed the guarantees would be selling something.
This is the edge, it is known, it has a test that asserts the bad
behaviour so that fixing it will break the test loudly.

## Depth: it is not a problem

```
10 000 emits        -> 10 000 collected
 3 000 pauses       -> driven, and replayed
```

Both run. The continuation is a value on the heap, not frames on the
stack, so "how deep can this go" has the same answer as "how much
memory do you have". Chapter 13's multi-shot is what makes this true
and chapter 20 prices it.

## Shapes that do work, and are worth knowing

```scala
val xs = List(1, 2, 3).map(n => if n == 2 then !Delim.exit(n * 100) else ())
// the whole block answers 200
```

`exit` leaves from inside a lambda the block does not own. Chapter 5
promised that; here it is pinned.

```scala
val a = !Delim.pause("q1")
val b = !okay.async(a * 2).at[Row]
val c = !Delim.pause(s"q2:$b")
```

A dialogue pauses **across** an async operation. This is the shape all
of chapter 23 rests on, and it works because the async handler sits
outside the machine — chapter 17's rule, paying off.

## The short version

| you had | under a capture |
|---|---|
| an effect with a handler **outside** | one shared timeline |
| an effect with a handler **inside** | re-run per branch |
| a plain `var` | one cell, shared, always |
| a `Resource` | survives; **n branches = n open handles** |
| cleanup written as a line of code | dropped with the continuation |
| `bracket` | compile error |
| `try`/`finally` | compile error |
| `try`/`catch` | compiles, catches nothing |
| a second machine | compile error — unless the row is abstract |
| depth | not a problem |

Three of those are compile errors and one is a known hole. That ratio
is the actual claim of this chapter: the dangerous combinations were
made unwritable rather than documented, and the one that escaped has a
test with its name on it.

---

← [18 · What belongs in a library](18-what-belongs-in-a-library.md) ·
[Contents](index.md) ·
[20 · The costs, measured →](20-the-costs-measured.md)
