# 20 · The costs, measured

> **Part V is the limits.** The numbers below come from
> `src/jmh/scala/okay/DelimBenchmark.scala` and are recorded, with
> their methodology, in `src/jmh/history.tsv`. Every one of them is
> dated and carries the commit it was taken at, because a benchmark
> number without a date is a rumour.

---

## What the lanes measure

Four lanes and a floor, all at **N = 1000**:

| lane | does, one thousand times |
|---|---|
| `plainList` | `xs = i :: xs` in a `while` loop — the floor |
| `delimPushOnly` | installs a delimiter and captures **nothing** |
| `delimGenerator` | a generator written in user code — one **capture** each |
| `writerTell` | a native effect operation, **outside** any machine |
| `writerTellUnderDelim` | the same tells, under **one** delimiter |

Read that table before any number. Most wrong conclusions about the
cost of continuations come from comparing two lanes that answer
different questions, and this chapter ends on exactly that mistake.

## The numbers

Per operation, dividing by N, from the paired runs of 2026-09-17:

| | per op | what it is |
|---|---|---|
| a cons in a loop | **~4 ns** | the floor |
| a delimiter push | **~24 ns** | installing a prompt, nothing captured |
| a capture | **~90 ns** | reifying the rest of the program |
| a prompt's label | **8 bytes, no measurable time** | `what @ where` |
| a capture's allocation | **104 B** | the `Delay` and the closure |

Whole-lane figures, which is how they were actually taken:
`delimPushOnly` 24.205 → 24.470 µs/op, `delimGenerator` 93.190 →
92.995 µs/op, control `plainList` 4.156 → 4.112 µs/op (commit
`b91b4338`, `-f 3 -prof gc`, paired master-vs-lane on the same box
back to back).

The headline, said plainly: **a generator built out of captures costs
about 22x a hand-written `while` loop** for this shape. That is the
real number, it is not small, and chapters 5–9 are the argument for
when it is worth paying. If your inner loop conses a million times,
do not build it out of captures.

## Bytes are the verdict; time is the rumour

The most useful methodological rule in this repository:

> **Bytes are load-proof. Time is not.**

Allocation per operation is the same on a busy box and an idle one, so
it decomposes and it can be trusted:

- `+8000.001 B/op` on `delimPushOnly` is **1000 prompts × 8 bytes** —
  one reference field.
- `+8008.476 B/op` on `delimGenerator` is **1000 captures × 8** for
  `Capture`'s `at`, **plus the one prompt**.

You can read the feature out of the arithmetic. No allocation was added
at all for the label itself: the `At` value class is scalar-replaced
and no string is built.

Time, by contrast, lies whenever the box is busy. One round was taken
while a Virtualization.framework VM held 125% CPU, and the *unchanged*
control lanes moved 30–40% — `plainList` went 4.13 → 5.46. The round
was discarded, and the reason it had to be is the general rule:

> **The instrument must be smaller than the effect.** A lane swinging
> 37% cannot price an 8% change.

## What a label costs: the version that was refuted first

Chapter 21 says prompt labels cost 8 bytes and no time. That is the
*second* implementation. The first one built the label eagerly in the
`Prompt` constructor:

```scala
s"$what @ $where"
```

and cost **23.220 → 28.155 µs/op on `delimPushOnly`, +21%** — because
that lane makes a thousand prompts per operation and every one of them
built a string nobody would ever read. The fix is a lazy `def label`
that stores two references and joins them only on the failure path.

This is worth keeping as a pattern. Diagnostics are paid for on the
happy path and collected on the sad one; anything you format eagerly is
a cost you take a thousand times to serve a message you print once.

## Forwarding costs nothing, and the run was not optional

`delim-forward-not-throw` moved `run`'s whole body into a private
`machine` method. Result: `delimGenerator` 89.547 → 88.716 µs/op,
`delimPushOnly` 23.914 → 23.833, and **bytes identical to the digit**
(942328.043 → 942328.397, 358040.165 → 358040.164).

Nothing changed, which was the point of running it. In this codebase a
callee crossing the inlining threshold re-decides every caller — that
has happened repeatedly and by surprise — so "this is just a
refactoring" is a hypothesis, not a fact. It was measured, it held, and
the measurement is what makes it a fact.

## The trap: real numbers that answer a different question

Now the mistake this chapter exists for, because it is the one that
survives review.

`okay-llm`'s `Cut` carried a claim in its header: a boundary "costs the
prompt push, not the capture price". Both halves are true statements
about real lanes. The push *is* nothing — 1000 pushes is 28.4 µs, so
one is about 0.03 µs. Captures *are* the expensive part. Real
benchmarks sat right beside the claim.

And the claim was wrong.

`Cut` does not install a prompt and then leave. It enters `Delim +
Async`, which puts **every operation of the body** through the machine.
Neither `delimPushOnly` (N pushes) nor `delimGenerator` (N captures)
measures that shape. The lane that does had to be written:
`writerTellUnderDelim` — one push, N ordinary tells, under the machine
— read against `writerTell`, the same N tells outside it.

> **2.01x, then 2.27x**, across two rounds at load 9–10.

The absolutes moved 20–35% between the rounds and the **ratio held**,
which is why a pair must come from a single run.

So the guard costs about **2x on the work inside it**, not "a prompt
push". The claim had been believed for weeks because it was surrounded
by honest numbers that answered adjacent questions.

> **The rule:** a cost claim is measured only if a lane measures *the
> shape the claim is about*. Numbers nearby are camouflage, not
> evidence.

This is the single most transferable thing in Part V, and it is the
first entry in chapter 27's catalogue.

## What the machine allocated for nothing (2026-09-27)

The first table above is from 2026-09-17. Ten days later
`delim-machine-allocs` read the machine for allocations that carry no
information, and found three that paid (`-prof gc`, three alternating
rounds per change, each against the commit before it):

| change | `delimPushOnly` | `delimGenerator` | `writerTellUnderDelim` |
|---|---|---|---|
| before | 358 B, 22.9 µs | 902 B, 87.4 µs | 310 B, 26.0 µs |
| `step` answers its next state, no `Right` per op | −16 B, 0.99x | −48 B, 0.94x | ±0, 1.00x |
| the delimiter frame carries the answer type, no identity bind under it | −64 B, 0.79x | −128 B, 0.83x | ±0, 1.01x |
| a foreign op resumes at the head bind's program, not at a `pure` | ±0, 1.00x | ±0, 0.99x | −40 B, 0.99x |

(bytes per operation, N = 1000.) The second one is the lesson. Every
`push` added a frame whose only job was to say "the prompt's answer is
also the operation's answer" — a TYPE fact, paid for at run time as a
closure call, a `pure` and a loop step on every return, and as one more
frame copied by every capture. Carried as a witness on the delimiter
itself (`r <:< X`, which the compiler already had), it costs a field
and nothing else. A capture now costs about **69 ns and 726 B**.

And one that was refuted three times over: skipping the bind frame a
foreign operation is suspended under saved 72 B and 13% on
`writerTellUnderDelim`, and in every shape tried it made the lanes
that never meet a foreign operation 2-5% slower with not one byte
changed. Bytes decompose; time also answers to what the JIT decides
about a method's shape, and only the lanes you did NOT mean to touch
tell you that.

CAPTURE DEPTH was never measured before this: `delimGenerator` has
about one frame between the shift and its prompt. `DelimDepthBenchmark`
varies it. A level the machine holds as frames (a delimiter and a bind)
costs ~42 ns and ~425 B for one capture called once, and ~21 ns and
~225 B more per level for each further call of k; a level of plain
`map`s — which `Free.resume` folds into one composed k before the
machine ever sees them — costs ~12.5 ns and ~137 B, and ~6 ns and
~49 B per further call. A search that captures through many delimiters
and resumes often pays the first slope; that is what a defunctionalized
continuation would have to beat.

## How to price your own

1. **Write the lane that has your shape.** If none of the existing
   lanes does what your code does, your number does not exist yet.
2. **Pair it.** Two readings from one run. Cross-run comparisons move
   20–35% on this box for reasons that have nothing to do with the
   code.
3. **Check the box first.** `uptime` before, not after. A load of 20
   invalidates a 10% result.
4. **Start at `-f 3`.** One fork lied by 53% here once.
5. **Prefer bytes.** They decompose, they are load-proof, and they let
   you check the arithmetic against the feature.
6. **Date the number and name the commit.** Chapter 16 nearly shipped a
   1.7x from a source comment; re-measured, it was 1.29x. Numbers rot
   silently, and only a date tells you to look again.

---

← [19 · What a capture does to everything else](19-what-a-capture-does.md) ·
[Contents](index.md) ·
[21 · The disciplines that make it safe →](21-the-disciplines.md)
