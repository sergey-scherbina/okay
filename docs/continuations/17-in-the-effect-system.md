# 17 · In the effect system

> **Part IV is about building.** Compiled in
> `src/test/scala/TestBookInTheSystem.scala`. Every chapter so far put
> a capture *somewhere*. This one is about what "somewhere" means when
> there are handlers around it, and about the one question to ask when
> a capture and an effect disagree.

---

## The question

A delimited capture runs up to its delimiter. An effect runs up to its
handler. Both are brackets, and the only thing that matters is which
bracket is inside the other:

> **Is the handler inside the delimiter, or outside it?**

Everything in this chapter is a consequence of that question, and most
confusion about "why did my state do *that* under a capture" is the
question having been answered by accident.

## The experiment

A program that captures, invokes the continuation **twice**, and adds
to a state in between. Handler outside the delimiter:

```scala
def outside(using Delim.Prompted[Int]): Int ! Row = direct:
  val x = !Delim.shift[Int, Int, State % Int](k => direct { !k(1) + !k(10) })
  !State.modify[Int](_ + x).at[Row]

State.run[Int, Int](0)(Delim.delimited[Int, State % Int](outside))
// (11, 12)
```

The state handler is **outside** the machine, so the two branches are
one timeline. `k(1)` adds 1 to a state of 0 and answers `1`. Then
`k(10)` adds 10 **to the 1 the first branch left behind** and answers
`11`. Total: `12`. The branches are not a fork; they are a sequence
that happens to be written as a fork.

Now the handler inside:

```scala
def inside(using Delim.Prompted[(Int, Int)]): Int ! Row = direct:
  val x = !Delim.shift[(Int, Int), Int, State % Int]: k =>
    direct:
      val (s1, a1) = !k(1)
      val (s2, a2) = !k(10)
      (s1 + s2, a1 + a2)
  !State.modify[Int](_ + x).at[Row]

!.run(Delim.delimited[(Int, Int), P](State.handle[Int](0)(inside)))
// (11, 11)
```

`k(10)` starts from **0**, not from 1. The handler is inside the
delimiter, so the continuation includes the handler's own fold — each
invocation re-runs it from the state it had at the capture point. The
branches never see each other. Total: `11`.

The two answers differ by one, which is the whole point: the
arithmetic is identical, the `modify` is identical, and the second
branch saw a different world. Nothing in either body announces this.
The handler order decided it.

## The part the compiler will not let you get wrong

The first draft of this chapter tried to write **one** body and run it
both ways, moving only the brackets. That does not compile, and the
error is the most useful sentence in the chapter:

```
No given instance of type okay.Delim.Prompted[Int] was found
```

A prompt is typed by **what the delimiter answers**. With the handler
outside, the delimiter answers `Int`, so a capture inside answers
`Int`. With the handler inside, the delimiter answers the *handler's*
result — `(Int, Int)`, a state paired with a value — so the capture
answers `(Int, Int)` and the body has to say what to do with two of
them.

This is worth dwelling on, because it inverts the usual worry. Moving a
handler past a delimiter is not a silent semantic change that you must
be careful about. It is a **type change**, and the compiler stops you.
The body above could not be reused; it had to be rewritten, and
rewriting it forced the author to decide what combining two branches
means. That decision was invisible in the `outside` version because
the timeline made it for us.

So the rule has a corollary worth remembering:

> If moving a handler compiles without complaint, the capture never
> crossed it.

## Reading a row

The row is where this is written down. In `outside`:

```scala
type Row = Delim + State % Int          // inside the delimiter
val prog: Int ! State % Int = Delim.delimited[Int, State % Int](outside)
```

`Delim` is in the body's row and **not** in the result's. That is what
`delimited` does: it discharges the machine. `State % Int` passes
through untouched, which is precisely the statement "the state handler
is somewhere further out, and I have not run it".

In `inside`, the same body ends as `(Int, Int) ! P` — pure — because
both brackets closed. Reading the two result types side by side tells
you the nesting without looking at the call.

This is the practical skill: **the row is the bracket structure**. When
a capture and an effect disagree about what should have happened, do
not reason about the runtime. Write down the two types and see which
effect is still in the row at the delimiter.

## When a capture and an effect disagree

The diagnostic question, in order:

1. **Is the effect still in the row at the delimiter?** If yes, the
   handler is outside — one timeline, writes carry across branches.
   If no, the handler is inside — each branch re-runs it.
2. **How many times is `k` invoked?** Once is the ordinary case and
   both orders agree. The orders only diverge at zero invocations
   (chapter 5) and more than one (chapter 13).
3. **Does the effect have a resource or an external side effect
   attached?** Then re-running it per branch is not a semantic
   curiosity, it is a second `INSERT`. Chapter 19 has the cases and
   chapter 27 the mistakes.

Most of the time the answer to (2) is "once", which is why this
chapter's distinction can go years without being noticed — and then
matters enormously the first time somebody writes a retry, a
backtracking search, or a multi-shot capture over code that already
had a handler in it.

## Which order do you want?

Neither is the default and neither is safer. They answer different
questions:

- **Handler outside** — one shared world. Use it when the branches are
  *the same run* seen more than once: a resumable dialogue, a retry
  that should remember what already happened, an audit log that must
  not lose an entry because a branch was re-entered.
- **Handler inside** — a fork per branch. Use it when the branches are
  *alternatives*: a search that must not let one candidate's writes
  pollute another, a speculative attempt that should leave no trace,
  the semantics chapter 13's multi-shot examples want.

If what you want is a fork and the effect is state, there is a third
answer that is usually better than either: use the effect that *means*
a fork. `Logic`/`Choice` backtrack by construction, and you will not
have to explain the bracket order to whoever reads it next.

## What carries over to every other effect

`State` is the demonstration because its difference is one number. The
rule is not about state:

- **`Resource`** — a handler outside means opens accumulate across
  branches and release LIFO at the end (chapter 19 measures this: two
  branches hold two handles at once). Inside, each branch opens and
  closes its own.
- **`Throws`** — a raise inside a captured continuation reaches
  whichever handler is outside the delimiter; a handler that raises
  instead of resuming leaves the rest of the program unrun.
- **`Async`** — a dialogue can pause across an async operation, which
  is the whole of chapters 6 and 23, and works because the async
  handler sits outside the machine.

Each of those is pinned by a test in `TestDelimLimits`, and chapter 19
is a guided tour of that file. The chapter you are reading gives you
the rule; that one gives you the cases where the rule bites.

---

← [16b · Two monads at once](16b-two-monads-at-once.md) ·
[Contents](index.md) ·
[18 · What belongs in a library →](18-what-belongs-in-a-library.md)
