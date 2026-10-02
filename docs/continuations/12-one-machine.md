# 12 · One machine, one prompt stack

> Compiled in `okay-direct/src/test/scala/TestBookOneMachine.scala`,
> including the shape this chapter used to call a **hole** in the
> guard — and the test that shows it is closed.

---

## The rule

> **A machine owns one prompt stack.** A delimiter installed by one
> machine cannot be reached from another.

"Machine" is the interpreter that runs captures. A program has one.
`scope`, `collecting` and `pausing` install a boundary on whichever
machine is already running. `delimited`, `collect` and `resumable` do
the same when a machine is already running, and start one when none
is. What decides between the two is the row, and the compiler reads it.

## The shape that reads like ordinary code

Here is the sentence somebody wants to write: *a producer that pauses
for an answer*. Naturally:

```scala
Shift.resumable[Q, A, R, F]:        // I want to pause...
  ...
    Shift.collect[Int, F]:          // ...and also collect
      ...
```

Every part of it is a combinator the book has already taught, and it
reads correctly. The history of this shape is the history of this
chapter:

1. It **compiled and threw `NoPrompt` at run time.** The inner
   `collect` started a second machine; walking the program, that machine
   met the `pause` aimed at the OUTER prompt, claimed it (it is a
   `Shift` operation, and this is a `Shift` machine), looked for the
   prompt on its own stack and did not find it.
2. It became a **compile error** naming the fix: write `collecting`,
   which installs a boundary and leaves the machine alone.
3. Today it **works**. A row is a union, so the inner block's row says
   `Shift` is already there, which means a machine is running. The door
   reads that and pushes its delimiter on the running machine, which is
   exactly what `collecting` does. One machine, two delimiters, and the
   `pause` crosses the `collect` as it should.

The suite pins the third, with the nested `delimited` and an `abort`
aimed past it:

```scala
val p = Shift.prompt[String]
val inner: Int ! Row = Shift.delimited[Int, Row](Shift.abort[String, Int, Row](p)("escaped"))
val prog: String ! Row = Shift.push[String, Pure](p)(inner.map(_.toString))
assertEquals(!.run(Shift.run[String, Pure](prog)), "escaped")
```

`Row` is `Shift % ? + Pure`. The inner `delimited` is written at a row
that already holds `Shift`, so it stands on the outer machine and the
capture to `p` crosses it.

## The evidence

Every door that runs a machine takes one piece of evidence:

```scala
def run[R, F[+_]](prog: R ! Shift % ? + F)(using m: Machine[F]): R ! F =
```

`Shift.Machine[F]` answers one question: *does a machine already run in
`F`?* The answer is yes when `F` holds a `Shift` of any key: a keyed
`Shift % R`, a dynamic `Shift % ?`, or both. It is read off the row at
compile time, and the door acts on it. Outermost, it runs its own
machine. Inside one, it pushes on that one.

```scala
assert(summon[Shift.Machine[Shift % ? + Pure]].inner)
assert(summon[Shift.Machine[Shift % Int + Pure]].inner)
assert(!summon[Shift.Machine[Pure]].inner)
```

The keyed `reset` (a delimiter named by its answer type,
[continuations in practice](../continuations-in-practice.md)) takes the same evidence and decides the
same way. That is why a `reset` written inside another `reset` has
always nested on one machine.

## The hole, and why it is closed

An earlier version of this chapter had a section titled *What the guard
does NOT catch*:

```scala
def runAnything[A, F[+_]](p: A ! Shift % ? + F): A ! F =
  Shift.run(p)
```

Inside the body `F` is abstract. The old guard was a `NotGiven`
(`NotGiven[Shift[?, Any] <:< F[Any]]`), and `NotGiven` reads
**"cannot be proved" as "false"**. So the helper manufactured the
evidence itself, and a caller at a `Shift` row got a second machine and
a `NoPrompt` at run time. The chapter's advice was to pass the
obligation on. Advice is not a check, and the check that replaced the
`NotGiven` found a library helper in this repository (okay-persist's
`Dialogue`) that had swallowed it.

Now an abstract row is not guessed. It is a compile error, and the
error says what to write:

```
whether a machine already runs in the row F cannot be read here:
F[scala.Any] is abstract, and it may hold a Shift.
Pass the obligation on to the caller, who knows the row: take `(using Shift.Machine[F])`
```

Written that way, the helper works at both kinds of row:

```scala
def runAnything[A, F[+_]](p: A ! Shift % ? + F)(using Shift.Machine[F]): A ! F =
  Shift.run(p)
```

At `Pure` it runs its own machine. At `Shift % ? + Pure` it nests, and
a capture to a prompt its caller installed crosses it. That is the
shape that used to be a `NoPrompt`.

A row with an abstract part AND a `Shift` (`Shift % ? + F`) is read as
nested, because the `Shift` is certain. Only a row where nothing
answers the question is refused.

## What is left of `NoPrompt`

A capture still names its prompt by value, so it can still miss. What
remains are the prompts that are genuinely gone or never installed:

- a prompt kept past its block, or a continuation resumed after its
  block returned;
- a capture to a prompt no block installed;
- a program run by hand by a machine other than the one holding the
  prompt.
- a block typed at a row that says no machine runs (`delimited[Int,
  Pure]` written inside another block): the door believes the row and
  starts its own machine, and a capture through it to the outer
  boundary misses. Inside a block, write the block's row (chapter 9).

The keyed forms (`reset`/`shift` by answer type) and the statically
stacked prompts (`Shift.Stacked`, both in
[continuations in practice](../continuations-in-practice.md)) make these a
compile error as well. The dynamic `Shift % ?` form is the one that
trades that check for prompts as values. Its type says so: `NoPrompt`
is possible exactly where the key is `?`.

---

← [11 · Four captures](11-four-captures.md) ·
[Contents](index.md) ·
[13 · Multi-shot: a continuation is a value →](13-multi-shot.md)
