# 10 · Prompts, and why they are first class

> **Part III opens the machine.** Parts I and II can be read by
> somebody who only wants to use this; from here on the audience is
> whoever wants to build with it, or to review somebody who did.
> Compiled in `src/test/scala/TestBookPrompts.scala`.

---

## The one value everything is built on

Part II kept saying "the boundary" without saying what it is. It is a
value:

```scala
val p = Delim.prompt[Int]
```

`Prompt[R]` — a **prompt**. Making one allocates nothing interesting
and installs nothing; it is a name. Installing it is a separate act
(`scope`, or `delimited` which installs and runs), and a capture
aimed at it travels up to wherever it was installed.

Three properties, and each one buys something specific.

## It is typed, and the type is the contract

`Prompt[R]` says: **what leaves through this boundary is an `R`.**

```scala
Delim.delimited[String, Pure]:
  direct:
    !Delim.exit("a string, because the boundary says String")
    "unreachable"
```

The exit cannot hand it an `Int`; the block cannot end with one. A
reader looking at the boundary knows what the whole thing produces
without finding every exit — which is the difference between this and
an exception, where the type says nothing and the answer is found by
reading `catch` blocks.

Different types are different boundaries, and they nest without
interference:

```scala
Delim.delimited[String, Pure]:
  direct:
    val n = !Delim.scope[Int, Pure]:      // an Int boundary inside a String one
      direct:
        !Delim.exit(42)
        0
    s"got $n"
// "got 42"
```

The inner `exit(42)` is checked against the inner boundary. Nothing
about the outer `String` is involved.

## It has identity, not just a type

Two prompts of the same type are two different boundaries:

```scala
val a = Delim.prompt[Int]
val b = Delim.prompt[Int]
assert(a ne b)
```

This matters the moment there are two of them in scope. Aiming at a
boundary means naming *that* value, not "the nearest `Int` one". A
system that identified boundaries by type would make two `Int` scopes
indistinguishable; one that identified them by a string name would let
two libraries collide on `"retry"`.

## It knows where it came from

```scala
val p = Delim.prompt[Int]
p.label      // "prompt @ TestBookPrompts.scala:29"
```

Every prompt carries the door that made it and the line that asked. It
costs nothing when nothing goes wrong: the two halves are stored as
they arrive and joined only when something asks — a detail that exists
because building the string eagerly cost a measured 21% on the
push-heavy benchmark.

What it buys shows up on the bad day:

```
the capture at Booking.scala:41 named the prompt 'retry @ Retry.scala:12',
which is not on the stack of the machine running it.
Installed here, innermost first:
  scope @ Booking.scala:22
  delimited @ Main.scala:9
```

That is the error you get for aiming at a boundary that is not there —
and it tells you *what you aimed at*, *where it was made*, and *what
was actually installed*. Compare with the alternative, which is a
`NoSuchElementException` from inside a library.

The suite pins that message rather than describing it:

```scala
val e = intercept[NoPrompt](...)
assert(e.getMessage.contains("is not on the stack"))
assert(e.getMessage.contains("Installed here"))
assert(e.getMessage.contains("delimited @"))
```

## First-class means it travels

A prompt is an ordinary value, so it can be passed like one. That is
not a curiosity — it is what crossing a boundary requires:

```scala
def deep(n: Int)(using p: Delim.Prompted[String]): Int ! Row = direct:
  if n == 0 then
    !Delim.exit(using p)("bottom reached")
    0
  else !deep(n - 1)

Delim.delimited[String, Pure]: outer ?=>
  direct:
    val _ = !deep(30)(using outer)
    "never"
// "bottom reached"
```

Thirty frames down, a function that knows nothing about the caller
leaves through a boundary the caller installed. The thirty frames in
between are not consulted, do not propagate anything, and do not
mention the answer type.

Carried as a `using` parameter it reads as a capability: *this
function may leave through a `String` boundary*. That is visible in
the signature, which is the difference between this and an exception
travelling invisibly.

> **The open edge, stated because the book does not hide these.**
> Nothing stops you storing a prompt in a `var` and using it after its
> boundary has returned. That is a genuine hole — the evidence outlives
> what it names — and closing it needs region types (`runST`'s trick),
> which would put a type parameter on every signature that carries
> evidence. It is designed, costed and deliberately not built:
> `specs/delim-safety.md` stage 2, open because nothing has asked for
> it. Chapter 27 lists the mistake it permits.

## Why this is the interesting design decision

Most effect systems have handlers and no prompts. A handler is
installed dynamically and found by *type*: you `raise` an error and
the nearest handler for that error type catches it. It composes well
and it is enough for almost everything.

What it cannot express is **this** boundary rather than *a* boundary
of this kind. Two nested retry scopes, and you want the outer one. A
parser inside a parser, and you want to abandon the outer parse. A
transaction inside a transaction, and the inner one wants to abort the
whole thing.

With prompts as values that is a parameter you pass. Without them it
is a redesign — usually a flag threaded through, or a distinct error
type invented for the purpose of being caught somewhere specific,
which is a prompt with worse ergonomics and no type.

That is the case for first-class prompts, and it is the reason this
library has `Delim` underneath its effects rather than only handlers
on top.

---

← [9 · Composing the shapes](09-composing.md) ·
[Contents](index.md) ·
[11 · Four captures →](11-four-captures.md)
