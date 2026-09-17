# 18 · What belongs in a library, what in an application

> **Part IV is about building.** Compiled in
> `src/test/scala/TestBookLibrary.scala`. Chapter 14 said *if it can be
> an effect it should be*. That was too generous, and this chapter is
> the correction: most things should not be, and there is a test that
> tells you which.

---

## The criterion

> **A recipe earns the right to be an effect when more than one
> interpreter for it exists.**

Not "could conceivably exist". Exists — you can name the second one
and say who needs it. One interpreter and the effect is ceremony: a
row member, a handler, a constructor, an extra type in every
signature, and in exchange a level of indirection over code that was
going to do the one thing anyway.

This is the whole chapter. The rest of it is what the criterion looks
like when applied, including the case where the same concept went both
ways in one codebase.

## Applied: reading the clock

Here is the effect, in full:

```scala
enum Ticks[+A] derives okay.Effect:
  case Millis extends Ticks[Long]

object Ticks:
  inline def millis: Long ! Ticks = effect(Millis)
```

And here are the two interpreters, which is the part that matters:

```scala
val live: Handler[Ticks] = new:
  def handle[A](e: Ticks[A]): A = e match
    case Millis => System.currentTimeMillis()

def fixed(at: Long): Handler[Ticks] = new:
  def handle[A](e: Ticks[A]): A = e match
    case Millis => at
```

One program, run both ways, unmodified:

```scala
def stamped(msg: String): String ! Ticks = direct:
  s"$msg@${!Ticks.millis}"

given Handler[Ticks] = Ticks.fixed(7L)
stamped("hi").runWith        // "hi@7"     -- exactly, every time
```

Now the same recipe as an ordinary function:

```scala
def stampedDirectly(msg: String): String =
  s"$msg@${System.currentTimeMillis()}"
```

It works. It is shorter. And it cannot be pinned: a test can assert
the shape, or reach for a global clock mock, and that is the entire
cost — **the second interpretation has nowhere to live**. The effect
did not buy abstraction for its own sake; it bought a place to put the
interpreter that already needed to exist.

## Applied the other way: a helper that stays a helper

```scala
def attempt[A](times: Int)(f: () => A): Either[Throwable, A]
```

Retry-n-times. It is stateful, it is about control flow, it *feels*
like an effect, and several libraries ship it as one. Ask the
criterion: what is the second interpreter? "Retry, but don't actually
retry" is not an interpretation anybody wants; "retry with a different
backoff" is a *parameter*, not an interpreter. There is one way to run
it, so it stays a function, and the test for it is four lines with no
handler in sight.

The mistake this prevents is the common one in effect-oriented
codebases: everything becomes an effect, every signature grows a row
of six members, and the rows stop carrying information because they
are always the same six.

## The case worth studying: time, both ways, in one codebase

This library has no `Clock` effect. It also journals every clock read.
Both are true, and the comment that explains it is in
`okay-persist/.../Worker.scala`:

```scala
/** wall time, for the lease only: the PROGRAM's clock is
 *  `Wf.Runtime`, and it is journalled. These two must not be
 *  confused -- one is operational, the other is state. */
clock: () => Long = () => System.currentTimeMillis(),
```

Two clocks, two answers from the same criterion:

- **The worker's lease clock** is a plain parameter with a default. Who
  is the second interpreter? Nobody. A test passes a different
  function, which is what parameters are for. It stays a parameter.
- **The workflow's clock** is `Sys.Now`, an operation in the runtime's
  own enum, sitting beside `Sys.Uuid` and `Sys.Random`. Its second
  interpreter is not hypothetical and not a test double: it is
  **replay**. A durable workflow that re-runs from its journal must
  see the time it saw the first time, or the program takes a different
  branch and the whole mechanism of chapter 23 collapses.

So the question is never "is this a side effect?" — both of those read
a clock. It is "does somebody need a second answer?" For the lease,
no. For the program, yes, and the somebody is the replayer.

This is also the cleanest statement of when continuations are involved
at all. `Sys.Now` has to be an effect *because the program is
resumable*. Resumability is what created the second interpreter. Most
of the effects in this book earned their place the same way.

## A checklist that fits on one line each

Before making something an effect, answer these. Any "no" and it stays
a function:

1. **Name the second interpreter, and who needs it.** "Tests" counts
   only if a parameter would not do. "Replay", "a dry run", "a trace",
   "a different backend" all count.
2. **Does the caller need to not know which one is running?** If the
   call site would pick anyway, pass the thing instead.
3. **Does it need to be in the type?** An effect puts itself in every
   signature between the call site and the handler. That is the price,
   and it is only worth paying if readers should see it there.
4. **Would a capture be involved?** If the recipe needs to stop,
   resume, or run the rest of the block more than once, it cannot be a
   parameter — that is chapter 14's territory and the answer is yes.

Question 4 is the one specific to this book, and it is the reason
chapter 14's `Budget` is an effect while `attempt` above is not. Both
are "control flow helpers". Only one of them needs the rest of the
block as a value.

## Where the library line falls

The same criterion, applied to the boundary between a library and an
application:

- **A library ships the effect and at least two interpreters.** If it
  can only supply one, it is shipping a function and should say so.
- **An application ships the handler that knows its own world** — the
  database, the deploy, the tenant. It is the one place where the
  second interpreter's existence is a fact rather than a design guess.
- **A capture belongs to whoever wrote the effect**, and to nobody
  else. This is chapter 14's rule restated: the mature use of a
  capture is that call sites never see one.

That last one is the payoff of the whole of Part IV. A library that
gets it right leaves an application writing `!Budget.spend(30)` and
`!Ticks.millis`, with no prompt, no `k`, no `shift`, and no idea that
any of this was ever a question.

---

← [17 · In the effect system](17-in-the-effect-system.md) ·
[Contents](index.md) ·
[19 · What a capture does to everything else →](19-what-a-capture-does.md)
