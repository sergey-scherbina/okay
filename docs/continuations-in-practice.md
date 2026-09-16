# Continuations in practice

The literature examples in [theory/2](theory/02-continuations.md) prove
that delimited control *works*. They do not prove it is *useful* — they
are all from papers. This page is the other half: the four shapes that
earn a capture in ordinary code, each under a name, each next to the
way the same thing is usually written, and a section on when it makes
code worse.

Everything here is in `TestDelimPatterns`, so the code below is
checked rather than claimed.

## The rule

> **Reach for an effect first. Write `shift` only when no effect fits.
> And when you do write one, the first thing you do is wrap it in a
> name.**

Three claims, each with a reason.

**Reach for an effect first.** A continuation is not a feature you
choose, it is what effect handlers are made of: a handler in this
library is a Cont-valued natural transformation (`F !> S = F ==> ([X]
=>> X /> S)`, `Effects.scala:120`), so `Logic`'s backtracking,
`Once`'s call-by-need cell, `State`'s threading and `Choice`'s
multi-shot are already captures — with names, laws, and handlers you
can swap. When one of them fits, using it gets you the capture *and*
the ability to reinterpret it later. Raw `shift` gets you the capture
only.

**Only when none fits.** The honest test is: can you name the effect
your code needs — "it can fail", "it remembers", "it may branch"? If
yes, that noun is an effect, and it belongs in the row. `shift` is
for control flow that is *not* an effect of the program but a shape
of it: leaving early, coming back later, running the rest twice.

**Wrap it in a name.** A `shift` at a call site makes the reader stop
and reconstruct what the continuation is. A name does not:

```scala
if r(t) then !Delim.exit(Some(t))              // obvious
if r(t) then !Delim.shift[Unit](_ => pure(Some(t)))   // a puzzle
```

Both compile to the same thing. The second one asks every future
reader to notice that `k` is dropped and work out what that means.
The name is also where the *evidence* goes: `exit` can only be
written inside a `delimited`, because holding the `Prompted[R]`
requires it — so the wrapper is what turns a runtime `NoPrompt` into
a compile error. Writing a capture and not naming it throws away the
only part of the work that is reusable.

The four names below are what came out of applying that rule to the
patterns people actually hit.

## 1 · Leave early with an answer

**The shape:** you are deep in nested loops, or inside a lambda, and
you have the answer. You want out, with it.

```scala
Delim.delimited[Option[Int], Pure]:
  direct:
    for t <- txs; r <- rules do
      if r(t) then !Delim.exit(Some(t))   // out of BOTH loops, with a value
    None
```

**How it is usually written** — the loops become a fold, and "am I
done already" is now a question every iteration has to ask:

```scala
txs.foldLeft(Option.empty[Int]): (acc, t) =>
  if acc.isDefined then acc
  else if rules.exists(_(t)) then Some(t) else acc
```

That fold is fine at this size. It stops being fine when the work
inside the loop is not a predicate, when there are three levels, or
when the exit is inside a callback that the loop does not own. The
alternatives at that point are an exception thrown for control flow
(untyped, and it walks past everything that was counting on an
orderly exit) or a sentinel threaded through every caller.

`exit` is a capture that **drops** its continuation — that is all an
early return is. `Delim.abort` is the same thing outside a direct
block.

## 2 · A push producer, read as a pull

**The shape:** the producer wants to call you; you want a sequence.

```scala
def walk(t: Tree[Int])(using Delim.Emitting[Int]): Unit ! R = direct:
  t match
    case Tree.Leaf(a)    => !Delim.emit(a)
    case Tree.Node(l, r) => !walk(l); !walk(r)

Delim.collect[Int, Pure](walk(t))    // List(1, 2, 3)
```

**How it is usually written:** a `ListBuffer` threaded through the
producer as a parameter, or a callback parameter that turns the
producer inside out and makes it someone else's job to know when it
ends.

The walk above is ordinary recursion and knows nothing about lists:
`emit` builds the list out of *the rest of the walk*, which is why
nothing has to be inverted. The same shape covers paginated APIs, a
`ResultSet`, a watcher, a parser — anything that pushes when you
wanted to pull.

## 3 · Stop in the middle, carry on later

**The shape:** the program needs an answer from outside — a person, a
different service, the next HTTP request.

```scala
def booking(using Delim.Asking[String, String, String, R]): String ! R = direct:
  val city   = !Delim.pause("Which city?")
  val nights = !Delim.pause(s"How many nights in $city?")
  val pay    = !Delim.pause(s"Pay ${nights.toInt * 90} for $city?")
  if pay == "yes" then s"Booked $city for $nights nights" else "Cancelled"

val start = !.run(Delim.resumable[String, String, String, Pure](booking))
!.run(Delim.drive(start)(answering(List("Kyiv", "3", "yes"))))
```

`resumable` answers with a `Paused[Q, A, R, G]`: either `Ask(question,
resume)` — and `resume` **is** the rest of the program — or `Done(value)`.

**How it is usually written:** a state machine with a `step` column
and a hand-rolled record of everything the process knew so far, which
has to be written, migrated, and kept in sync with the code by hand.
Here the process is straight-line code and the record is the
continuation.

Two properties worth knowing. A `Paused` is a *value*, so resuming it
does not consume it — the test answers the same start page twice,
differently. And the honest limit: it lives in memory. It outlives a
request, a retry, a fork of the dialogue — **not** a restart of the
process. Making it outlive that is persistence, and a different piece
of work.

## 4 · Do something on the way back

**The shape:** from somewhere in the middle, you want to act on what
the *rest* of the block produces.

```scala
Delim.delimited[String, Pure]:
  direct:
    !Delim.onReturn(s => if s.startsWith("failed") then s"$s; refunded $amount" else s)
    if ok then s"charged $amount" else "failed: card declined"
```

**How it is usually written:** wrap the remainder in a function and
pass it down (which restructures everything after this point), or a
`finally` — which cannot see the answer, so it cannot compensate
based on it.

This is the generalization of the reverse-mode AD example: run the
rest, then do something with what came back. Compensation in a saga,
undo, an audit line that records what the remainder produced,
measuring what the rest of a request cost — all the same two lines.

## When a capture makes code worse

- **Multi-shot and `var` do not mix.** If the continuation is invoked
  more than once, the rest of the block runs more than once, and
  anything mutable it touches is shared across those runs. This is a
  documented footgun with a test, not a bug — but it is the first
  thing to check when a captured continuation gives a surprising
  answer.
- **Resources under a captured `k`.** Who closes the file if `k` is
  never invoked, or invoked twice? `bracket` and `Resource` answer
  for their own scope; a capture that crosses that scope is on you.
- **Stack traces stop describing your code.** The machine reifies the
  continuation, so a debugger shows the interpreter's loop, not the
  call chain you wrote.
- **`shift` where `flatMap` would do.** If the control flow is a
  sequence, `direct` is the whole answer. A capture for elegance is a
  cost with no matching benefit.

## Deciding

| The control flow you need | Reach for |
|---|---|
| a sequence, possibly failing | `direct` over your row |
| it can fail / branch / remember | the effect: `Fail`, `Choice`, `State`, `Once` |
| leave from the middle with an answer | `Delim.exit` (`Delim.abort` in `for`) |
| a producer that pushes, a consumer that pulls | `Delim.collect` / `Delim.emit` |
| stop now, resume when the answer arrives | `Delim.resumable` / `pause` / `drive` |
| act on what the rest of the block answers | `Delim.onReturn` |
| none of the above | `Delim.shift`, and then give it a name |

---

← [Direct style](direct-style.md) · [Docs](README.md) ·
[2 · Continuations (theory)](theory/02-continuations.md) →
