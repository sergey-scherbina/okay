## cont-macro-inline-helpers - inline helpers on k's path are read by the Cont macro; direct { } bodies were already safe

cont-stack-layer1-c items (3) and (4), in the agreed order (the
operator: "Дальше", "next").

**Inline helpers.** An `inline def` helper called with `k` or `k`'s
answer (`applyTo(k, x)`, `plusOne(k(x))`) is expanded before `shift`'s
macro reads the body, as an `Inlined` node whose bindings hold the
arguments. The macro gave up on any binding that mentions `k`. Now:
- a binding that is `k` itself is an alias, replaced by `k` in the
  helper's body;
- the others are vals, in order.

A million each on a 128 KB thread, with zero switches (red first:
StackOverflowError). Meaning and multi-shot are kept. Library bodies
reach the new path (Generate, Writer, Source), but end in the same leaf,
since their `k` sits inside a program's `flatMap` lambda. A probe at the
final choice found only the new tests changed form.

**`direct { }` bodies** over a program answer needed nothing. The block
is binds by the time `shift` reads it, so `k` is called later, from the
interpreter's loop. TestContDirectDepth (okay-direct) passes a million
on 128 KB on unchanged code and stays as the guard.

Tests: TestContMacro, TestContDirectDepth. Docs: docs/cont-stack.md.
