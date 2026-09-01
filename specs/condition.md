# Conditions: the road between throwing and damage-as-data

## Overview

This stack has two answers to "something went wrong": ABORT
(`Throws` — the continuation is discarded, `runEither` makes the
abort a value at the boundary) and DAMAGE-AS-DATA (a `Bad`/`Left`
per element — total readers, the caller folds over failures). Both
are boundary disciplines. What neither can say is the thing an
OPERATOR says mid-incident: "that record is malformed — here is
the corrected value, CONTINUE FROM WHERE YOU WERE." Between
throwing and tolerating there is REPAIRING, and repairing needs
the failure point to still exist when the decision is made.

That is the condition system of Common Lisp (and R's restarts —
specs/r.md already models "condition as data" for its subprocess;
this gives the host language the same vocabulary). Three parts:

- **signal** — raise a condition WITHOUT unwinding: the deciding
  policy runs while the signal point's continuation is still live,
  so "resume with this value" is possible at all. In this stack
  that is not exotic machinery — it is what EVERY effect operation
  already does: the handler answers, the continuation continues.
  A condition is an operation whose answer is a decision.
- **restarts** — named recovery frames established BETWEEN the
  signal point and the policy, each saying "if you unwind to me, I
  know how to continue from HERE" (skip this element, use a
  default, retry the batch). Invoking one unwinds exactly to its
  frame — the Delim shape: a restart is a prompt, invocation is an
  abort to it with a value.
- **the policy** — supplied at `run`, sees the condition AND the
  menu of currently-established restarts, answers one of:
  `Resume(value)` (continue at the signal point), `Invoke(name,
  value)` (unwind to that restart), or `Fail` (this run has no
  answer: escalate as an exception, the honest default).

Additive, per the operator's rule (2026-09-01): `Throws` is
untouched, no existing seam changes, a program that never signals
never pays. The machinery is its OWN small machine in the Delim
discipline — operation payloads are programs in the same row,
erased at the operation and re-typed inside the machine, exactly
as Delim.scala documents for itself — rather than a client of the
generic multi-prompt machine, because the restart stack and the
menu are ONE structure and one owner keeps them consistent.

## Interface

```scala
package okay

object Condition:
  /** what the policy answers */
  enum Decision:
    case Resume(value: Any)          // continue AT the signal point
    case Invoke(restart: String, value: Any)  // unwind TO that frame
    case Fail                        // no answer here: escalate

  /** the effect: signal carries the condition; within establishes
   * a named restart around a body */
  enum Op[+A]: ...                   // Signal, Within (payload erased)

  /** raise; the answer is what the policy resumed with */
  def signal[C, A](c: C): A ! Op

  /** a restart frame: if the policy invokes `name` with v, the
   * whole `within` answers `recover(v)` — the body's remaining
   * work is abandoned, everything OUTSIDE continues */
  def within[A](name: String)(body: A ! Op)(recover: Any => A): A ! Op

  /** the machine: interprets signals via the policy, owns the
   * restart frames, forwards every other effect (the Resource.run
   * shape); an unanswered Fail throws Unhandled naming the
   * condition and the menu it declined */
  def run[A, F[+_]](policy: (Any, Vector[String]) => Decision)
                   (prog: A ! (Op + F)): A ! F
```

## Behavior

- [x] Resume: the policy answers a value and the computation
      continues AT the signal point with it — the counter after
      the signal increments, nothing unwound
- [x] Invoke: signalling deep inside `within("skip")(...)` and
      invoking "skip" unwinds exactly to that frame — `recover`
      supplies the frame's answer, code between signal and frame
      never resumes, code OUTSIDE the frame continues
- [x] the menu accumulates lexically: nested `within`s offer both
      names, inner first; invoking the OUTER restart unwinds past
      the inner frame
- [x] Fail escalates as `Unhandled` naming the condition and the
      menu; a policy that always fails makes `signal` behave like a
      throw — the degenerate case is the familiar one
- [x] other effects forward: a signal-and-resume inside an Async
      row leaves the Async operations undisturbed (the Resource.run
      forwarding shape)
- [x] the repair story, end to end: a decode loop over records
      where damage SIGNALS with restarts "skip" and "patch" — one
      policy patches (the corrected value flows back into the
      decode), another skips (the element vanishes, the loop
      continues), a third fails (the loop aborts) — three outcomes
      of one program, chosen at run
- [x] a `within` whose body completes normally is invisible: the
      recover function never runs, the answer is the body's

## Out of scope

- serializing a suspended condition across processes — replay,
  not serialization, is the settled decision; a condition lives
  within one run
- integrating R's native restarts through the r.md wire — filed as
  its own step once okay-r lands its subprocess (the vocabulary
  now matches on both sides, which was half the point)
- interactive (human-in-the-loop) policies — a policy is a
  function; wiring it to a UI or an agent is the caller's
  composition, deliberately not machinery here

## Decisions

- **A condition is an operation, resumption is the handler's
  native move** — no new control primitive for Resume; the effect
  system was already a resumable-exception system waiting to be
  named. Rejected: exceptions-with-retry wrappers (they re-run
  from a boundary, losing the point: the signal site's progress).
- **Restarts are frames of the machine, not prompts of the generic
  Delim** — menu and frames are one structure; one owner keeps
  them consistent, and the payload-erasure discipline is exactly
  Delim's own documented one. Rejected: composing State-for-menu
  with Delim-for-frames (two machines racing to agree).
- **The policy sees the menu** — recovery is a DECISION and
  decisions need options on the table (the Ack/OnRepeat pattern
  again); a policy without the menu is a catch block. Rejected:
  blind handlers.
- **Fail throws** — an unhandled condition must not limp; naming
  the declined menu makes the incident report write itself.
  Rejected: a silent default resume (the lie).
- **Additive** — Throws, runEither, damage-as-data all stay what
  they are; conditions serve the case they cannot: repair at the
  point of failure. (The operator's rule, applied.)

## Results

Landed (conditions, 2026-09-01): `Condition` in the core — ~120
lines for the machine, in the Resource.run shape (a loop that owns
its frames and forwards everything else) with Delim's payload
discipline (a Within's body is a program in the same row, erased
at the operation, re-typed inside the one owner). Eight tests: the
full battery plus a policy bug of its own class (`NoSuchRestart` —
invoking off the menu is named as the policy's fault, not the
program's). The repair story runs verbatim: a decode loop with one
`within("skip")` frame PER ELEMENT, and the same program yields
patched / skipped / failed under three policies — which is what
"chosen at run" was always supposed to mean. The first consumer landed the same day (typed-bad-repair):
`Repair` in okay-persist — `decode`/`read` over the Typed view
where each damaged record SIGNALS `Damaged(offset, error, raw)`
under a per-element "skip" frame; the same log answers three ways
under three policies (patched in place, skipped with order intact,
aborted naming the offset), and a clean slice never consults the
policy at all — the never-signals-never-pays half of the additive
rule, tested. And the seam is ENGINE-AGNOSTIC by construction
(Repair works over Typed, Typed over any Topic) — proven on the
wire the same day (kafka-repair): TestKafkaRepair runs the same
three policies against a REAL broker with the production shape of
damage, a foreign producer's garbage bytes in the middle of the
topic — patched at the broker-assigned offset, skipped with order
intact, aborted naming offset 1. Live-gated, skips without the
broker. Still filed: r.md's native restarts bridged over the
now-shared vocabulary once okay-r lands.


## Direct style (2026-09-01, condition-direct)

A condition is an operation, so the direct machinery applies as-is —
and the direct reading is the Common Lisp reading: a signal is a
CALL THAT MAY RETURN.

- [x] `signal[Int]("how many?").?` in a direct block resumes AT the
  mark with the policy's value; progress before the signal survives
- [x] `within(...)(...)` reflects with `.?` (or runs as a bare
  statement); Invoke unwinds exactly to the frame, code between
  never resumes — semantics identical to the monadic spelling
- [x] the `frame` door: `Condition.frame(name)(directBody)(recover)`
  takes the body as a DIRECT block (two-line door: it forwards to
  `within` over `direct`), so nested `direct { }` ceremony
  disappears at restart frames

Recorded roads (BACKLOG): condition-restart-caps (lexical restarts
as capabilities — a nonexistent restart uncompilable in scope),
condition-typed-signal (type the condition/answer pair).
