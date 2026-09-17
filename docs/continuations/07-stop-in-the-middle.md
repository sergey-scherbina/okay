# 7 · Stop in the middle, carry on later

> Compiled in `src/test/scala/TestBookStopInTheMiddle.scala`. This is
> the shape with the largest consequences: everything in Part VI is
> built from it, and chapter 6's ceiling is removed here.

---

## Where chapter 6 stopped

`collect`/`emit` gave the consumer back its control flow and ran the
producer to the end. "The first two files, and don't walk the rest"
was still out of reach.

The difference is one word. Instead of **emitting** an item, the
producer **pauses** with it — and hands back *the rest of itself*.

## The same walk, pausing

Chapter 6's directory walk, with `emit` replaced by `pause`:

```scala
def walk(e: Entry, prefix: String = "")
        (using Delim.Asking[String, Unit, Unit, Row]): Unit ! Row =
  direct:
    e match
      case Entry.File(n, _) => !Delim.pause(s"$prefix$n")
      case Entry.Dir(n, es) =>
        val here = if n == "/" then "/" else s"$prefix$n/"
        for child <- es do !walk(child, here)
```

`Delim.Asking[Q, A, R, Row]` reads: *this program asks questions of
type `Q`, expects answers of type `A`, eventually produces an `R`.*
Here it asks with a path (`String`), needs nothing back (`Unit`), and
produces nothing (`Unit`) — a pure producer.

Starting it gives you a value:

```scala
var p = !.run(Delim.resumable[String, Unit, Unit, Pure](walk(tree)))
```

`p` is not a running walk. Nothing is running. It is a **`Paused`** —
either `Done(result)`, or `Ask(question, rest, where)` where `rest` is
the walk from this point on.

## Taking two, and not walking the rest

```scala
p match
  case Delim.Paused.Ask(q, _, _) if got.size < n =>
    got = got :+ q
    p = !.run(Delim.answer(p, Nil)(()))._1     // hand back an answer, get the next stop
  case _ => going = false
```

and the suite checks the thing that matters:

```scala
val (files, pulls) = take(2)
assertEquals(files, List("/a.txt", "/src/Main.scala"))
assertEquals(pulls, 2, "it kept walking after it had enough")
```

**Two pulls.** The third file was not visited, the second directory
was not descended into. Not filtered afterwards — never walked. That
is chapter 1's "first ten tokens of a two-gigabyte file", and there is
no thread, no queue, and no buffer of everything.

> **A practical trap, met while writing this chapter.** `Ask` carries
> a `rest` function, and calling it directly does not typecheck the
> way you expect: it returns a *program in the `Delim` row*, not a
> finished value — it has to be run by the machine that owns the
> prompt. `Delim.answer(p, journal)(a)` is the door for stepping by
> hand, and `Delim.drive(p)(oracle)` for running to the end. Reach for
> `rest` only when you are building a driver of your own.

## Two producers, in step, with no thread

This is the one a callback simply cannot do:

```scala
(a, b) match
  case (Delim.Paused.Ask(qa, _, _), Delim.Paused.Ask(qb, _, _)) =>
    pairs = pairs :+ (qa, qb)
    a = !.run(Delim.answer(a, Nil)(()))._1
    b = !.run(Delim.answer(b, Nil)(()))._1
```

Two independent recursive walks, advanced alternately, zipped. With
callbacks you would need one of them on another thread and a rendezvous
between them; here both are values in local variables and the
"concurrency" is a `while` loop.

## A paused program is a value, and is not consumed

The claim deserves its own test, because it is the one that surprises
people:

```scala
val start = !.run(Delim.resumable[String, String, String, Pure](booking))

assertEquals(!.run(Delim.drive(start)(answering(List("Kyiv", "3")))), "Kyiv/3")
// the SAME start, answered again, differently
assertEquals(!.run(Delim.drive(start)(answering(List("Lviv", "2")))), "Lviv/2")
```

`start` was not used up by being resumed. It is an ordinary immutable
value; driving it produces a result and leaves it intact. **One past,
two futures.**

That is what makes a debugger possible — fork a run at a decision,
feed two different answers, compare the outcomes — and chapter 22 is a
production one. It also has costs and hazards that chapter 13 and
chapter 18 go through: anything effectful in the captured part happens
once per resumption.

## What this shape is really for

Three uses, in increasing order of how much they change a system.

**1. Pull from a push.** The walk above. Local, no ceremony.

**2. A dialogue.** A program that asks a series of questions and is
answered by something outside it — an operator, another service, a
test harness. The program reads as straight-line code; the questions
and answers are data:

```scala
def booking(using Delim.Asking[String, String, String, Row]): String ! Row = direct:
  val city   = !Delim.pause("Which city?")
  val nights = !Delim.pause(s"How many nights in $city?")
  s"$city/$nights"
```

There is no state machine here, and no `if` on a stored state. The
sequence of questions *is* the program.

**3. A program that outlives its process.** This is the big one, and
it needs care, so here is the honest version:

> A paused program is a closure. **It cannot be written to disk.**
> What can be written down is the sequence of ANSWERS, and the
> position is re-derived by starting the program again and feeding
> them back without asking.

That is the entire idea behind the durable engine in chapter 21: the
journal holds answers, the program is re-run over them, and where it
stops is where it stood. The consequence is a discipline — everything
non-deterministic must enter through a question — which chapter 21
enforces with a type rather than a rule in a document.

Do not skip that discipline on the strength of this chapter. Resuming
inside one process is easy; surviving a restart is a design, not a
feature.

## When this is the wrong tool

- **You want every item anyway.** `collect` (chapter 6) is simpler and
  has no stepping to get wrong.
- **You want concurrency.** The two walks above are interleaved, not
  parallel. If you want them to run at the same time on two cores,
  you want fibres.
- **The "dialogue" has two questions.** Two `pause`s and a driver is
  more machinery than a function taking two parameters. The shape pays
  when the sequence is long, branching, or must survive something.
- **You are tempted to serialise the `Paused`.** Stop; see above. This
  is the single most expensive misunderstanding available here, and
  chapter 25 lists it as such.

## The recipe, condensed

```scala
// the program: it asks, and reads as straight-line code
def prog(using Delim.Asking[Q, A, R, Row]): R ! Row = direct:
  val x = !Delim.pause(question)
  ...

// start it: nothing runs yet
val p = !.run(Delim.resumable[Q, A, R, F](prog))

// step it by hand...
val (next, _) = !.run(Delim.answer(p, Nil)(answer))
// ...or run it to the end with an oracle
val result = !.run(Delim.drive(p)(q => okay.pure(answerFor(q))))
```

---

← [6 · A push producer, read as a pull](06-push-as-pull.md) ·
[Contents](index.md) ·
[8 · Do something on the way back →](08-on-the-way-back.md)
