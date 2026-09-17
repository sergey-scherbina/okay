# 1 · Four programs that are hard to write

> **Part I is the case.** It argues that a family of ordinary
> requirements has no straight-line form in most languages, that every
> team meets at least one of them, and that the usual replacements
> each leave a specific bug open. No solution appears until chapter 3,
> and no library API until Part II. A reader who will never write
> Scala can finish Part I and decide.

The code in this chapter is **illustrative**: much of it is the code
somebody writes first, which is to say code that does not work. It is
written to be read, not compiled. From Part II onward the runnable
examples are backed by a test suite and say so.

---

## Why start with problems

A book about a technique usually starts by explaining the technique.
That order flatters the author and fails the reader, because the
reader's question is not "what is a continuation" but "is there
anything in my system that this would fix". If the answer is no, the
definition is a waste of their evening.

So: four programs. Each one is something a working team has been asked
for. Each has an obvious first attempt. Each first attempt is wrong,
and wrong in a way that is not the author's fault — the language
declined to express what they meant.

Read them and count how many you recognise. That count is the whole
argument of Part I.

---

## One · Leave early, with an answer, from deep inside

You are validating an invoice. It has lines; lines have allocations;
allocations have cost centres. The rule: **if any cost centre is
closed, the whole invoice is rejected with that centre's name.**

Here is what anybody writes first:

```scala
def check(invoice: Invoice): Unit =
  for line <- invoice.lines do
    for alloc <- line.allocations do
      if closed(alloc.centre) then
        ???            // ← and here the trouble starts
```

What goes in place of `???`. The answer must travel from the middle of
two nested loops, inside a method, out to the caller — past code that
has no opinion about cost centres and should never have heard of them.

**The three things people do.**

*Throw.* It works, and it is the closest thing most languages have to
what we mean. But an exception is a side channel: the signature of
`check` says `Unit`, so nothing in the type tells the caller this can
happen, nothing makes them handle it, and `catch` will also catch
things you did not mean — a `NullPointerException` from a bug three
layers down arrives at the same `catch` as your business rule.

*Return a sentinel and thread it.* Now `check` returns
`Option[Rejected]`, and so must every function between the loops and
the caller, because each of them has to look at what the one below
returned and decide to stop:

```scala
def check(invoice: Invoice): Option[Rejected] =
  invoice.lines.foldLeft(None: Option[Rejected]):
    case (Some(r), _) => Some(r)               // already rejected
    case (None, line) => checkLine(line)       // ...and checkLine does
                                               //    the same for allocs
```

Notice what happened. Two functions that are about invoice structure
now contain the word `Rejected` and a rule about early exit. The
knowledge leaked upward into code that does not need it. Add a second
failure mode next quarter and every one of them changes again.

*Use a library combinator.* `find`, `exists`, `collectFirst`,
traverse-with-`Either` — for this example one of them fits, and you
should use it. The reason this problem is in the book is that the
combinator runs out exactly when the loop stops being a loop: when the
walk is a recursive descent over a tree with four node types, when the
early exit has to happen inside a callback you do not control, when
two different things can end the walk and they carry different values.
Then you are back to threading.

**What you wanted to write** is this, and it is worth looking at even
though nothing so far can run it:

```scala
if closed(alloc.centre) then exit(Rejected(alloc.centre))
```

— where the layers in between are not mentioned at all, and the type
of the whole walk says what can come out of it.

---

## Two · A producer that pushes, read as something that pulls

You have a parser. Someone else wrote it, or you did, five years ago.
It works by callback: you hand it a function, it hands you tokens.

```scala
def parse(doc: String)(onToken: Token => Unit): Unit
```

Now you need the first ten tokens. Or: tokens until you see one that
means "stop". Or: to read two documents in step, one token from each.

`onToken` gives you no way to say any of that. It is called; you take
what you are given; you do not decide when the next one comes. The
producer holds the loop, and you hold a function that is called from
inside it.

**The three things people do.**

*Collect everything first.* `val all = buffer(parse(doc))`, then work
with the list. Correct, and it reads the whole document to answer a
question about its first ten tokens. On a 2 GB log file it is not an
inefficiency, it is an outage.

*Throw to stop early.* A `StopIteration` exception, caught outside. It
works. It also means a control-flow exception crosses a library you do
not own, through code that may have its own `catch`, and that you now
cannot use to read two producers in step — there is only one stack to
throw off.

*Run the producer on another thread and hand items over a queue.* This
is the industrial answer and it is genuinely used. It also turns a
question about tokens into a question about threads: a bounded queue,
a poison pill, a shutdown path, backpressure, an exception on the
producer thread that must be re-raised on the consumer's, and a test
suite that is now nondeterministic. A colleague reviewing it has to
think about liveness.

**What you wanted** is for the producer to stop, hand you a token, and
*wait* — without a thread, because nothing here is concurrent. You
wanted a coroutine. Languages that have generators (`yield`) give you
this for one specific case; the shape underneath is a continuation,
and Part II builds it in four lines.

---

## Three · Stop in the middle, and carry on tomorrow

This is the one that reaches management, so it gets more room.

A purchase over ten thousand needs a director's approval. The program
is four lines of business logic:

```scala
val order = place(request)
val ok    = approval(order)        // ← a human, some time this week
if ok then ship(order) else refund(order)
```

The second line waits for a person. Not milliseconds: **days.** The
process will be restarted for a deploy before the answer arrives.
Whatever is holding that program in memory will be gone.

**What teams build instead.** Almost always the same thing: the
program is turned inside out into a state machine, persisted in a
table.

```scala
enum OrderState:
  case Placed, AwaitingApproval, Approved, Rejected, Shipped, Refunded

def onEvent(state: OrderState, e: Event): OrderState = (state, e) match
  case (Placed, Submitted)            => AwaitingApproval
  case (AwaitingApproval, Approved)   => Approved
  case (Approved, Shipped)            => Shipped
  // ...and so on, for every pair that can occur
```

This is not a bad design. It is the standard one, it is honest about
persistence, and a good team writes it well. But look at what it did
to the four lines:

- **The control flow became data.** `if ok then ship else refund` is
  now two table rows and a `match`. The business rule is no longer
  written anywhere in one place; it is distributed across transitions.
- **The compiler stopped helping.** Nothing checks that every
  reachable `(state, event)` pair is handled. Add a state, and the
  pairs you forgot are found in production, by a run that stops moving.
- **The program got longer in the middle.** The interesting line —
  "wait for a director" — is the one that vanished. In its place is
  machinery about *how* waiting is done.
- **Two things must now agree.** The code and the state table. They
  drift, and the drift is silent until a run lands in a state the
  current code no longer understands.

And the alternative — adopting a workflow engine — trades those costs
for different ones: a service to operate, a new deployment unit, a
vendor's opinions about how your code is structured, and, in most such
engines, a hand-written fold from events back to state which must be
kept in step with the program by discipline alone.

**What you wanted** is for the four lines to stay four lines, and for
the process to be allowed to die at line two. Chapter 21 shows exactly
that running; the pause is a continuation, and what gets written down
is not the program but the *answers* it has received.

---

## Four · Do something on the way back

The smallest of the four, and the one most often written subtly wrong.

Every request through your service should be timed, and the timing
recorded with the outcome — including when the outcome is an error.

```scala
def handle(req: Request): Response =
  val started = now()
  val res = route(req)
  record(now() - started, res.status)     // ← does not run if route throws
  res
```

So you reach for `try`/`finally`:

```scala
try route(req)
finally record(now() - started, ???)      // ← and what is the status here?
```

`finally` runs on both paths but cannot see the *result*. You wanted
"on the way back out, with whatever is coming back" — and the two
halves (always runs / sees the value) are in different constructs. So
you write it twice, once in the success path and once in the failure
path, and six months later somebody adds a third exit and updates one
of them.

**What you wanted** is a hook on the boundary that sees the value
passing through it, whichever way it is going. That is the fourth
shape, and it is four lines in Part II.

---

## What the four have in common

Read them again as a group and the same noun is missing from all four.

| | what the code needed to name |
|---|---|
| leave early | *the rest of the walk*, so it can be discarded |
| a pushing producer | *the rest of the producer*, so it can be paused |
| wait for a person | *the rest of the program*, so it can be kept |
| act on the way back | *the rest of the caller*, so it can be wrapped |

**"The rest of the program" is a thing these programs need to talk
about, and no ordinary language lets them.** You can talk about the
value a function returns. You cannot talk about *what the caller will
do with it*, even though that is exactly what every one of these four
needs to grab hold of.

A continuation is that missing noun: the rest of the program, as a
value you can hold. That is the whole idea, and chapter 3 spends a
page on it.

## The claim, stated plainly

Three claims, and the rest of Part I defends them:

1. **These four are common.** Not exotic; not research examples. Most
   systems of any size contain at least one, usually badly.
2. **The standard replacements are reasonable and each leaves a
   specific bug open** — not "are bad". Chapter 2 names the bug for
   each one, because "inelegant" is not an argument anybody should act
   on.
3. **Naming the missing noun collapses all four into one mechanism**,
   which is why it is worth learning once rather than working around
   four times.

If you recognised none of the four, the honest recommendation is to
stop here; chapter 4 says so again in more detail, and means it.

---

← [Contents](index.md) · [2 · What teams build instead →](02-what-teams-build-instead.md)
