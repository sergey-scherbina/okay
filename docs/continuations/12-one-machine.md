# 12 · One machine, one prompt stack

> Compiled in `src/test/scala/TestBookOneMachine.scala`, including two
> tests that demonstrate the **hole** in the safety guard this chapter
> is about. A chapter about a safety feature that does not say what it
> misses is an advertisement.

---

## The rule

> **A machine owns one prompt stack.** A delimiter installed by one
> machine cannot be reached from another.

"Machine" is the interpreter that runs captures. It is started by
`delimited`, `collect` and `resumable`; it is *not* started by `scope`,
`collecting` and `pausing`, which install a boundary on whichever
machine is already running.

Chapter 9 gave that as a rule to follow. This chapter is why it exists
and what enforces it.

## The mistake that reads like ordinary code

Here is the sentence somebody wants to write: *a producer that pauses
for an answer*. Naturally:

```scala
Delim.resumable[Q, A, R, F]:        // I want to pause...
  ...
    Delim.collect[Int, F]:          // ...and also collect
      ...
```

Two machines, one nested in the other. Every part of it is a
combinator the book has already taught. It reads correctly.

It is wrong, and before the guard existed it **compiled and threw
`NoPrompt` at run time** — which is the worst possible arrangement,
because the mistake is in a shape people naturally write and the
failure arrives in production.

**Why it breaks.** A row is a union, so the inner `Delim` is the *same
`Delim` by class* as the outer one. The inner machine, walking the
program, meets a capture aimed at the OUTER machine's prompt and
claims it: it is a `Delim` operation and this is a `Delim` machine.
Then it looks for that prompt on its own stack, does not find it, and
raises `NoPrompt`.

## The guard

The three machine-starting combinators ask for a witness:

```scala
def run[R, F[+_]](prog: R ! (Delim + F))(using OneMachine[F]): R ! F
```

and `OneMachine[F]` exists only when `F` does not already contain
`Delim`. So the nested spelling fails to compile, with a message that
names the fix:

```
this row already contains Delim, so this would start a SECOND machine,
and a capture cannot cross from one machine's prompt stack to another's.
Use the nested form, which installs a delimiter on the machine already running:
  delimited -> scope,   collect -> collecting,   resumable -> pausing
```

The suite asserts both halves — that it is refused, and that the
message offers `scope` — because an error message that does not say
what to do instead is only half a guard.

## Why the witness is spelled the way it is

A detail worth one paragraph, because it is a real constraint and not
a style choice.

The natural spelling is "prove `Delim` is not a member of `F`", using
a membership witness (`NotGiven[In[Delim, F]]`). That does not work:
proving membership in an *abstract* row makes the compiler unfold the
row into a union and try to join its alternatives, and dotty crashes —
`AssertionError: Failure to join alternatives`. It crashed at this
library's own call sites, so the core did not compile.

The spelling that works is subtyping: `NotGiven[Delim[Any] <:< F[Any]]`.
A union on the *right* of a `<:<` needs no join, because subtyping
*into* a union is the easy direction.

So a guard in this book is shaped partly by a compiler bug. That is
ordinary, and worth saying out loud: the reason a piece of library
code looks unusual is often a constraint you cannot see from the call
site.

## What the guard does NOT catch

Here is the hole, and the suite demonstrates it rather than describing
it.

```scala
def runAnything[A, F[+_]](p: A ! Delim + F): A ! F =
  Delim.run(p)
```

This compiles. Inside the body `F` is abstract, so
`Delim[Any] <:< F[Any]` cannot be proved — and `NotGiven` reads
**"cannot be proved" as "false"**. The witness is manufactured inside
the helper, and the obligation never reaches the caller. Instantiate it
at a row that already contains `Delim` and you have the exact mistake
the guard exists to refuse, at run time again.

> **The first draft of this test wrote the helper with
> `(using Delim.OneMachine[F])`, and the guard CAUGHT it** — with the
> obligation propagated, a caller at a concrete `Delim` row cannot
> satisfy it and the code does not compile. That failure is the guard
> working, and it is why the hole needs this exact shape: a helper
> that swallows the obligation instead of passing it on.

**So the rule for generic code is one line: pass the obligation on.**
If your helper starts a machine, take `using OneMachine[F]` and let
your caller prove it. A `using` clause here is not ceremony; it is the
difference between a guard that protects your callers and a guard you
have quietly disabled for them.

This is also the general shape of the limitation, and it is worth
recognising elsewhere: **`NotGiven` is not a proof of absence, it is a
failure to find.** Under an abstract type it always succeeds, so any
guard built on it is a guard against the shape people write, not a
theorem.

## What would make it a theorem

Region types, the `runST` trick: give the machine a scope tag that
cannot escape, so the type system tracks which machine a prompt
belongs to. It is designed and costed in this repository's
`specs/delim-safety.md`, and deliberately not built — it would put a
type parameter on every signature that carries evidence, including the
inline doors whose whole design is that a call site writes as few type
arguments as possible.

That is a legitimate trade, and stating it is the point: the guard
catches the mistake people actually make, it misses a shape you now
know to look for, and the cost of closing that gap is known.

---

← [11 · Four captures](11-four-captures.md) ·
[Contents](index.md) ·
[13 · Multi-shot: a continuation is a value →](13-multi-shot.md)
