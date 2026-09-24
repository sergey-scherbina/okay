# 25 · Cutting a model mid-sentence

> **Part VI is production.** Compiled in
> `okay-llm/src/test/scala/okay/llm/TestCut.scala`; the thing itself is
> `okay-llm/.../Cut.scala`. The smallest useful non-local exit in this
> repository, and the chapter where a comment about cost was wrong for
> weeks.

---

## The problem

A model is streaming tokens. Somewhere around token 200 it says
something that violates a rule — a forbidden claim, a malformed
structure, a leaked identifier. You want generation to **stop**, and
you want nothing downstream to have seen the poisoned prefix.

The stages between the model's mouth and your caller are a pipeline:
pull, decode, accumulate, emit. To stop from inside the validator you
must either thread an `Option` (or an `Either`) through every stage —
each of which now has a "or we gave up" case it does not care about —
or throw, and lose the streaming boundary's structure.

## The shape

A validator stands **in** the token stream and, on a violation, aborts
to a **named prompt** installed over the generation:

```scala
def guarded[A](gen: Prompt[Either[Violation, A]] => A ! (Writer % String + (Delim + Async)))
    : Either[Violation, A] ! Writer % String + Async =
  val p = Delim.prompt[Either[Violation, A]]
  Delim.run(Delim.push(p)(gen(p).map(Right(_))))

def cut[A, X](p: Prompt[Either[Violation, A]])(v: Violation): X ! ... =
  Delim.abort[Either[Violation, A], X, Writer % String + Async](p)(Left(v))
```

Three things worth noticing in that small amount of code:

**The violation is a value.** `Violation(rule, at, seen)` — the rule
that fired, the token position, what was seen. The caller retries,
reprompts, or surfaces it. It is not an exception and it does not need
to be caught.

**The exit type is `X`.** `cut` returns whatever the call site needs,
because it never returns: it is chapter 5's `exit`, named and typed.
The validator can be called in the middle of an expression.

**`Delim.abort` drops the continuation.** No pipeline stage between the
validator and the boundary learns that anything happened. There is no
`Option` threading on the stages between, and no poisoned partial
output flows further.

Three properties, pinned:

- a violating stream gives `Left(rule, position)` and **no further
  pulls** — generation actually stops, which is the point;
- a passing stream is **identical to the unguarded run**;
- nested guards work, and the inner violation aborts the **inner**
  prompt while the outer continues — chapter 10's typed prompts doing
  the only job that matters here.

That second property is the adoption doctrine: `guarded` wraps a
streaming generation and the unguarded path is untouched. A feature
you can add to one call site and delete from it is a feature a team
will try.

## The cost, and the sentence that was wrong

The header of that file used to say the guard "costs the prompt push,
not the capture price". It stood for weeks. Half of it is right and
the half that matters is not.

The right half: a passing stream never captures, and the push really
is nothing — a thousand pushes cost 28.4 µs, so one is about 0.03 µs.

The wrong half: entering `Delim + Async` puts **every operation of the
body** through the delimited-control machine, and the body is
per-token.

```
writerTell            15.098 / 18.012 µs   N tells, no machine
writerTellUnderDelim  30.376 / 40.847 µs   one push, same N, under it
                                  ratio    2.01x then 2.27x
```

Two rounds at load 9–10; the absolutes moved 20–35% between them and
**the ratio held**, which is why a pair must come from one run.

The benchmark had to be **written** for this. Neither of the two that
already existed measures this shape: `delimPushOnly` counts N pushes
and `delimGenerator` counts N captures. So the claim read as measured
while the numbers beside it answered other questions — chapter 20's
rule, and chapter 27's first entry.

## The decision it informs

> **A guard roughly doubles the cost of whatever runs inside it.**

For token streams that is far below the model's own latency and worth
paying. For a hot inner loop it is not.

The practical consequence is about **placement**, not about whether to
use it: the boundary belongs around the smallest span that needs it,
rather than around the whole generation. Wrapping the entire agent run
in a guard to validate one field puts every operation of the run
through the machine to protect a few tokens.

The same applies to any guard of this shape — `okay.ui.Scope`
included, which is the next chapter.

## Why a capture, and not something simpler

Fair question, since the validator could return an `Either` and let
the pipeline deal with it. Three reasons, in order of how much they
matter:

1. **The stages between do not gain a case.** Every intermediate stage
   keeps the type it had. This is chapter 2's argument — the
   replacements teams build all leak into the signatures of code that
   has no opinion.
2. **Stopping is immediate.** `no further pulls` is asserted. A
   threaded `Either` stops at the next stage that checks it, which is
   at least one token later and usually more.
3. **The boundary is named and typed.** `Prompt[Either[Violation, A]]`
   says what the exit carries, and nesting two of them is
   unambiguous — the inner cut cannot accidentally unwind the outer
   generation.

The honest counterweight: it costs about 2x on the guarded body, and
before this measurement existed the file claimed otherwise.

---

← [24 · A debugger for agents](24-a-debugger-for-agents.md) ·
[Contents](index.md) ·
[26 · Cancellable flows in a UI →](26-cancellable-flows-in-a-ui.md)
