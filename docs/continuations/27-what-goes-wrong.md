# 27 · Everything that typically goes wrong

> **A catalogue, not a memoir.** The mistakes available to somebody
> *using* continuations — and to somebody *implementing* them — are a
> small, recurring set. Naming them is worth more than any particular
> incident, because a named mistake is one you can look for.
>
> Written as a checklist, so it can be read at any point in the book.
> Chapter 4 (deciding) and chapter 19 (the limits) both point here.

Each entry: **what it looks like**, why it is easy to make, what it
costs, and the cheapest way to rule it out.

---

# A · Choosing wrongly

### A1. Reaching for a capture where an ordinary effect is simpler

*Looks like:* a `shift` in application code, where the requirement was
"this step might fail" or "collect these as you go".

*Easy because* the capture is the most powerful tool in the box, and
once you understand it, everything looks like a control-flow problem.

*Costs:* a reader who now needs chapters 10–13 to maintain a function
that wanted `Either`. And, per chapter 20, about 2x on whatever runs
inside the boundary.

*Rule it out:* ask what the continuation is **for**. If you never
invoke `k`, you wanted an exit — which is probably an effect that
already exists. If you invoke it exactly once and immediately, you
wanted `flatMap`.

### A2. Using a capture to avoid learning a combinator that already fits

*Looks like:* a hand-rolled generator, retry, or traversal built on
`shift`, beside a library function that does it.

*Easy because* building it is more interesting than finding it.

*Costs:* maintenance of a thing nobody else in the codebase knows,
plus the performance difference, which is usually large: chapter 20
measured a capture-built generator at about **22x** a hand-written
loop.

*Rule it out:* write the call site you want first, then search for it.
If a combinator's signature matches, use it.

### A3. Adopting the mechanism for one call site

*Looks like:* an entire module moved into a `Delim` row because one
function needed a non-local exit.

*Easy because* the row is viral in exactly the way that makes it feel
inevitable — once one signature has it, its callers want it.

*Costs:* every operation in the module now goes through the machine
(chapter 25 measured the shape: ~2x), and every signature mentions a
capability almost none of them use.

*Rule it out:* put the boundary around **the smallest span that needs
it**. If the row spread further than the feature, the boundary is in
the wrong place.

### A4. Refusing the mechanism where it is the only answer

The mirror, and worth stating because chapters 1–4 are an argument
against it. If the requirement is *stop this, from deep inside, without
the intervening layers learning about it* — or *resume this later, in
another process* — there is no combinator that does it. Threading a
result type through every layer is not a simpler solution; it is the
same solution, spread over more files, maintained by hand.

---

# B · Boundaries and evidence

### B1. Letting a prompt escape its delimiter

*Looks like:* a `Prompt[R]` stored in a field, returned from a
function, or closed over by a callback, and used after the `delimited`
block that created it has returned.

*Easy because* a prompt is an ordinary value, and values outlive
blocks.

*Costs:* `NoPrompt` at runtime, at the point of use, which may be far
from the mistake — and in an async system, on another thread.

*Rule it out:* keep prompts as parameters, never as fields. The
evidence-passing style (`using Prompted[R]`) exists so you rarely hold
one at all. A type-level fix — a scope tag, as `runST` has — is
possible and has a real cost; see chapter 21.

### B2. Two machines in one program

*Looks like:* a nested `delimited`/`run`, so a prompt pushed in one
machine is aborted from another.

*Easy because* nesting is how everything else in the language composes,
and the outer/inner distinction is invisible at the call site.

*Costs:* `NoPrompt`, or worse, a boundary that silently is not the one
you meant.

*Rule it out:* one machine per program — install scopes with `push`,
erase the row with **one** `run` at the top. A guard can make this a
compile error (`OneMachine`); if your system has one, put it in the
signature so it propagates.

### B3. Identifying prompts by name or string

*Looks like:* `prompt("checkout")`, and two boundaries in different
modules that agreed on a string without meaning to.

*Easy because* names are readable and debuggable, and it works until
two subsystems are composed.

*Costs:* an abort that lands at the wrong boundary. This is the worst
failure in the catalogue to diagnose, because the code is correct
everywhere you look.

*Rule it out:* prompts should have **identity**, not names. A fresh
value per installation, compared by reference. Keep the name as a
*label* for diagnostics, never as the identity.

### B4. Letting implicit search pick which boundary you meant

*Looks like:* two `Prompted` evidences in scope and a `shift` that
compiles.

*Easy because* it compiles, and the ambiguity rules may resolve it
"successfully" by specificity rather than by your intent.

*Costs:* a silent choice of boundary that changes when somebody adds
an import.

*Rule it out:* where two boundaries are genuinely in scope, name the
prompt explicitly at the capture. Ambiguity that the compiler resolves
is more dangerous than ambiguity it rejects.

### B5. Catching at the nearest delimiter when you meant an outer one

*Looks like:* a cancel that ends the sub-flow when it should have ended
the whole flow.

*Easy because* it is the default in every exception system anybody has
used.

*Costs:* a user who cancels twice, or a retry loop that retries the
wrong scope.

*Rule it out:* multi-prompt exists exactly for this — name the target.
If your mechanism cannot name it, you have handlers, not prompts, and
the fix is structural.

---

# C · Effects under a capture

### C1. Expecting hand-written cleanup after a capture point to run

*Looks like:*

```
val r = open()
...
exitOrCapture()
r.close()        // does not run
```

*Easy because* it reads as sequential code and it is sequential code —
right up to the line that might not come back.

*Costs:* a leak, and one that only appears on the path where something
was cancelled, which is the path least exercised by tests.

*Rule it out:* cleanup goes in an effect with a handler outside the
machine, never in a line of code after the thing that might not
return.

### C2. Expecting `try`/`finally` to mean what it usually means

*Easy because* it is the most reflexive construct in the language.

*Costs:* a finalizer that does not run, or runs at a moment you did not
intend.

*Rule it out:* this should be a **compile error** in a system that
supports captures, and in this one it is. If yours allows it, treat
every `finally` around a capture as a bug.

### C3. Mixing a bracket-shaped resource with a capture

*Looks like:* `bracket(acquire)(use)(release)` around a body that
captures.

*Easy because* `bracket` is the recommended shape everywhere else.

*Costs:* `bracket` runs its body to completion inside one suspension —
which is precisely what a capture breaks.

*Rule it out:* refuse it in the type system if you can. Otherwise: no
`bracket` in a row that carries a machine.

### C4. Assuming state is shared — or assuming it is not

*Looks like:* a multi-shot capture over code that touches state, and a
firm belief about what the second branch sees.

*Easy because* both beliefs are correct, and which one holds depends on
something invisible at the call site: whether the handler sits inside
or outside the delimiter (chapter 17).

*Costs:* silent wrong answers. Nothing throws.

*Rule it out:* write down the two types. Is the effect still in the
row at the delimiter? Outside the delimiter means one shared timeline;
inside means a fork per branch. And remember that a plain `var` has no
handler at all, so it is always shared.

### C5. Assuming exception handlers and captures compose in an obvious order

*Easy because* both are "non-local control flow", so they feel like one
mechanism.

*Costs:* a raise that reaches a different handler than you expected, or
a handler that raises instead of resuming and silently drops the rest
of the program.

*Rule it out:* the same question as C4 — which bracket is inside which.
Test both directions; this is not something to reason about.

---

# D · Resumption

### D1. Expecting a paused program to be serialisable

**The single most expensive misunderstanding in this list.**

*Looks like:* a plan to "save the continuation to the database and
restore it after the deploy".

*Easy because* every other kind of state in the system can be written
down, and a paused program feels like state.

*Costs:* an architecture. This is usually discovered after the
interfaces are designed around it.

*Rule it out:* a continuation is a closure. Save **what the program
was told**; re-derive where it stands by running it again (chapters
22 and 23). Any library claiming otherwise is restricting what you may
write, or lying about restarts.

### D2. Assuming one-shot when the continuation can be resumed twice

*Looks like:* a side effect between a capture and its use — a counter
incremented, a row inserted, an email sent.

*Costs:* it happens twice. With resources: **n branches hold n open
handles at once**, which turns a search with eleven branches into a
deadlock on a pool of ten.

*Rule it out:* know which your system gives you. If multi-shot is
possible, treat everything after a capture point as re-entrant, and
put side effects in effects whose handler position you have chosen on
purpose.

### D3. Assuming multi-shot when the runtime gives you one

The mirror. A stack-copying implementation typically gives one-shot,
and a design that forks at a capture will not port to it. Multi-shot
requires the continuation to be an immutable value, which is a
property of the implementation, not of the idea.

### D4. Performing I/O, reading a clock, or rolling a die in a program that will be replayed

*Easy because* replay is invisible: the same line of code runs, and
nothing marks the second time as different.

*Costs:* a restart that lands somewhere the first run never was. Its
worst form is silent and rare — a branch taken differently because a
clock moved.

*Rule it out:* everything the outside world tells the program enters
through the pause. Make it a **type**, not a convention (chapter 21),
and make the deliberate breach a named method so it shows up in a
diff.

### D5. Resuming a continuation that no longer matches the state it was captured against

*Looks like:* a resume after the world moved — a journal that gained
entries, a deploy, a cache that was invalidated.

*Costs:* an answer that is internally consistent and wrong.

*Rule it out:* an answer carries the position it expected, and the fold
rejects it if that position is filled. Optimistic concurrency, in the
projection rather than in the caller.

---

# E · Implementing

### E1. Reading an identity or index before an operation and using it after

*Looks like:* capturing a route, offset, or buffer index, then acting
on it after a structure grew or was replaced.

*Costs:* misattributed data, usually visible only under load.

*Rule it out:* re-read through the current structure, or carry the
identity in a form that cannot go stale. And if a buffer can grow,
check the growth's write ordering: the repair must be published before
the thing it repairs.

### E2. Enforcing a rule at the one call site where the symptom appeared

*Looks like:* a fix in the place the bug was reported, while three
other places implement the same rule.

*Easy because* the symptom names one location, and the fix works.

*Costs:* the same bug, twice more, months apart.

*Rule it out:* after every fix, **count the doors**. Grep for the
pattern, count with `wc -l` rather than eyeballing a truncated list,
and prefer one shared implementation to a rule applied n times. "There
are two" is a claim to verify, not to assume.

### E3. Quadratic re-walking of a captured segment

*Looks like:* a trampoline node per operation, a `.length` inside a
loop, or a fold that re-walks what it has already built.

*Costs:* fine in tests, fatal at depth — and the profile blames the
wrong thing, because the cost is spread evenly.

*Rule it out:* count operations, not seconds, and test at depth
(chapter 19 runs 10 000 emits on purpose). Delete a suspected node and
measure; if nothing changes, it was not the cost.

### E4. Renumbering a persisted enum by inserting a case in the middle

*Looks like:* a tidy addition to a sealed hierarchy that is also a wire
format.

*Costs:* old journals decode into the wrong cases. Every value looks
plausible.

*Rule it out:* append-only for anything persisted, an explicit tag
rather than an ordinal, and a test that decodes a **recorded** old
payload rather than a freshly encoded one.

### E5. Conflating "cannot proceed NOW" with "cannot proceed EVER"

*Looks like:* one failure verdict for a busy lease, an exhausted retry
policy, an incompatible journal, and a genuinely broken run.

*Costs:* an operator who cannot tell "come back in a minute" from
"this will never work", and automation that retries the wrong one
forever.

*Rule it out:* separate verdicts, named for what the operator should
do. And when you add one, count the doors (E2) — a verdict usually has
to be produced in more places than you remember.

---

# F · Claims

### F1. Asserting a cost without measuring the shape the claim is about

**The subtle form, and the most common failure in this whole
catalogue:** real benchmarks sit beside the claim and answer different
questions.

*Looks like:* "this boundary costs a push, not a capture" — where a
benchmark for pushes exists, a benchmark for captures exists, both are
honest, and neither measures a body running *under* a boundary.

*Easy because* adjacent numbers confer credibility. A reviewer sees
measurements and stops.

*Costs:* a decision made on a number that is off by 2x, believed for
as long as nobody writes the missing lane.

*Rule it out:* for every cost claim, name the lane that measures **that
shape**. If none exists, the number does not exist yet. Pair the
readings within one run, check the machine's load *before*, and prefer
allocation counts — they decompose and they are load-proof.

### F2. Documenting a limit that a later fix removed

*Looks like:* a caveat in a doc, true when written, describing a
restriction that no longer exists.

*Costs:* readers work around a problem you fixed, and conclude the
system is worse than it is.

*Rule it out:* limits get a test that asserts the bad behaviour. When
the limit goes, the test fails, and the failure sends somebody to the
prose. A caveat with no test is a caveat that will outlive its truth.

### F3. Leaving a promise in a doc that the code stopped keeping

*Looks like:* an example that no longer compiles, a command that no
longer parses, a number that was re-measured somewhere else.

*Costs:* trust, all of it. A reader who finds one stale example
discounts the whole document.

*Rule it out:* compile the examples. Every runnable snippet in this
book is in a test suite for exactly this reason. For numbers: date them
and name the commit, so a reader can tell an old measurement from a
wrong one.

---

## The three cheapest habits

If you take nothing else from this chapter:

1. **Write down the two types.** Most of group C is answered by asking
   which bracket is inside which, and the answer is in the rows.
2. **Count instead of remembering.** Doors, operations, branches,
   records. Every entry in group E is a counting failure.
3. **Make the rule a type and the breach a named method.** Groups B and
   D are full of conventions that hold until the day they do not, and
   every one of them can be a compile error instead.

---

← [26 · Cancellable flows in a UI](26-cancellable-flows-in-a-ui.md) ·
[Contents](index.md) ·
[28 · A short history →](28-a-short-history.md)
