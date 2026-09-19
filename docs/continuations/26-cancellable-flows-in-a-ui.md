# 26 · Cancellable flows in a UI

> **Part VI is production.** Compiled in
> `okay-ui/src/test/scala/okay/ui/TestScope.scala`; the thing itself is
> `okay-ui/.../Scope.scala`, and it is about forty lines. The smallest
> production use in this book, and the clearest.

---

## The problem

A wizard. Four screens: pick a plan, enter details, confirm, pay. On
every screen there is a **Cancel** button, and cancelling means the
whole flow ends — not this screen, the flow.

Written the ordinary way, every step returns `Option[Something]` or a
`Cancelled | Result`, and every step between must check it and pass it
on. The type of step three says something about cancellation, and step
three does not care about cancellation. That is chapter 2's complaint,
in the smallest possible instance:

```scala
def details(plan: Plan): Option[Details]
def confirm(d: Details): Option[Confirmed]
def pay(c: Confirmed): Option[Receipt]
// and a fold of Options at the call site that is the real flow
```

The flow is now the `flatMap` chain, and the four screens are
fragments of it.

## The shape

A prompt delimits a cancellable sub-flow:

```scala
def push[A](body: okay.Prompt[A] => A ! Row): A ! Row =
  val p = Delim.prompt[A]
  Delim.push(p)(body(p))

def cancel[A, R](p: okay.Prompt[R])(value: R): A ! Row =
  Delim.abort[R, A, Dialog](p)(value)
```

Inside the scope, **no step threads an `Option`**. `cancel` aborts to
the named scope's boundary with the value the scope answers, however
deep the steps in between, and the steps in between keep the types
they would have had if cancellation had never been a requirement.

The whole API is four functions: `lift` an ordinary step into the
scoped row, `push` a scope, `cancel` to one, `run` to erase the row.
There is a `scoped` for the common push-and-run case.

## Three properties, and the middle one is the argument

```scala
test("a scope cancels as a unit: no Option threading between its steps")
test("multi-prompt: an inner scope aborts ACROSS its boundary to the outer one")
test("Dialog itself is untouched: a plain scenario still runs beside the scoped one")
```

The first is the feature. The third is the adoption doctrine — this is
an option, not a migration: a scenario may run in the `Delim + Dialog`
row, and nothing in `Dialog` changed. A plain scenario still runs
beside a scoped one, which is what makes it possible to try this on one
flow.

The second is why a **prompt** rather than a handler. An inner scope
can abort across its own boundary to an outer one — cancel the payment
sub-flow *and* the wizard containing it, from inside the payment
sub-flow, in one call. Nested handlers cannot express that: a handler
catches at its own boundary and the inner one is in the way. Prompts
are first-class and typed, so the target is named (chapter 10), and
"which boundary" stops being a question about nesting order.

## The discipline that makes nesting work

One rule, and it is the whole of chapter 12 in a sentence:

> `push` installs scopes; **one** `run` erases the `Delim` row at the
> top.

Nested `run`s would be separate machines, and a prompt lives in the
machine that pushed it. Get this wrong and the failure is `NoPrompt` at
runtime — or, since chapter 21's `OneMachine`, a compile error instead.

## What it cost

Chapter 25 measured it: a guard of this shape roughly **doubles** the
cost of whatever runs inside it, because entering `Delim + Dialog` puts
every operation of the body through the machine.

For a wizard that is nothing. The body is four screens and a human
being; the machine's overhead is nanoseconds against a user's seconds.
The measurement matters for where you put the boundary, not whether:
scope the flow, not the application.

## Why this is the chapter to show a sceptic

Everything else in Part VI is impressive and large — durable workflows,
agent debuggers, streaming validators. This is forty lines, it solves a
problem every UI has, and the before-and-after is legible in the
signatures:

```
before:  def confirm(d: Details): Option[Confirmed]
after:   def confirm(d: Details): Confirmed ! Row
```

The second one does not mention cancellation, and cancellation still
works. That is the argument of the entire book at the smallest scale
anybody will accept it.

---

← [25 · Cutting a model mid-sentence](25-cutting-a-model.md) ·
[Contents](index.md) ·
[27 · Everything that typically goes wrong →](27-what-goes-wrong.md)
